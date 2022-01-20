{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

module Args(argn, getArgByName, newLambdaArg, addLambda, newLetArg, popArg, debugContext, argImplicit, deBruijnArgReps) where

import Expr
import Types
import Header
import Hs
import Parse (parseError,litMatch)

import Data.List
import Data.Maybe
import State
import qualified Data.Set as Set

argStr n tn = "arg" ++ show n ++ "t" ++ show tn

newLambdaArg :: [VT] -> ArgUsedness -> String -> ParseState Args
newLambdaArg argT argUsedness from = do
	context <- gets pdContext
	let depth = 1 + length context
	let impls = zipWith (\t tn -> Impl {
		implType = t,
		implCode = hsAtom $ argStr depth tn,
		implDeps = Set.singleton depth,
		implName = Nothing,
		implUsed = argUsedness
		} ) argT [1..]
	let newArg = Args impls LambdaArg from
	modify $ \s -> s { pdContext=newArg:context }
	return newArg

newLetArg :: ArgUsedness -> [Args] -> Impl -> [VT] -> String -> Args
newLetArg argUsedness context (Impl _ defHs defDepth _ usedness) defTypes from = newArg where
	depth = 1 + length context
	impls = zipWith (\t tn -> Impl t (hsAtom $ argStr depth tn) defDepth Nothing argUsedness) defTypes [1..]
	newArg = Args impls (LetArg defHs) from

argn :: Int -> ParseState Impl
argn deBruijnIndex = do
	context <- gets pdContext
	let flattenedArgs = concatMap flattenArg context
	case at flattenedArgs (deBruijnIndex-1) of
		Nothing -> parseError $ "Attempt to access " ++ (snd $ indexToOp (deBruijnIndex-1)) ++ debugContext context
		Just impl -> setUsed impl

getArgByName :: ParseState (Maybe Impl)
getArgByName = do
	context <- gets pdContext
	getArgByNameH 0 (concatMap flattenArg context)
	where
		getArgByNameH :: Int -> [Impl] -> ParseState (Maybe Impl)
		getArgByNameH _ [] = return Nothing
		getArgByNameH n (impl:rest) =
			if isJust (implName impl) then do
				matched <- litMatch [fromMaybe undefined $ implName impl]
				if matched then do
					appendRep $ (fst $ indexToOp n, " ")
					return $ Just impl
				else getArgByNameH (n+1) rest
			else getArgByNameH (n+1) rest

setUsed :: Impl -> ParseState Impl
setUsed impl = do
	context <- gets pdContext
	let newContext = map (\arg -> arg { argsImpls=setUsedImpl (argsImpls arg) } ) context
	modify $ \s -> s { pdContext=newContext }
	return impl where
	
	setUsedImpl :: [Impl] -> [Impl]
	setUsedImpl = map (\argImpl ->
		if implCode argImpl == implCode impl then
			argImpl { implUsed=UsedArg }
		else argImpl)

argImplicit :: (?isSimple::Bool) => ParseState Impl
argImplicit = do
	if ?isSimple
	then parseError "Expecting more expressions at EOF"
	else do
		context <- gets pdContext
		modify $ \s -> s { pdImplicitArgUsed = True }
		argn $ 1 + (fromMaybe 0 $ findIndex ((==UnusedArg).implUsed) (concatMap flattenArg context))

flattenArg (Args impls (LambdaArg) _) = impls
flattenArg (Args impls (LetArg _) _) = tail impls

addLambda :: Args -> Impl -> Impl
addLambda arg (Impl t body d _ _) = Impl {
	implType = t,
	implCode = hsFn (map implCode $ argsImpls arg) body,
	implDeps = Set.difference d (getArgDeps arg),
	implName = Nothing,
	implUsed = UsednessDoesntMatter }

-- Remove arg # and all its dependent let args (adding let statements for them).
popArg :: Int -> Impl -> ParseState Impl
popArg depth impl = do
	context <- gets pdContext
	let (finalImpl, finalContext) = mapAccumL maybePopIt impl context
	modify $ \s -> s { pdContext=concat finalContext }
	return finalImpl where
	
	maybePopIt :: Impl -> Args -> (Impl, [Args])
	maybePopIt impl eachArg
		| Set.member depth (getArgDeps eachArg) = (popIt eachArg impl, [])
		| otherwise = (impl, [eachArg])

	popIt :: Args -> Impl -> Impl
	popIt (Args _ LambdaArg _) impl = impl
	popIt (Args varImpls (LetArg refHs) _) (Impl retT bodyHs dep _ _) =
		Impl retT (hsLet (map implCode varImpls) refHs bodyHs) dep Nothing UsednessDoesntMatter

getArgDeps (Args (impl : _) _ _) = implDeps impl -- They should all be the same
getArgDeps (Args [] _ _) = implDeps noArgsUsed
--------- for debugging // errors -----------

debugContext :: [Args] -> String
debugContext context = "\nContext:\n" ++ (unlines $ snd $ mapAccumL (\count arg ->
	let args = flattenArg arg in
	(count+length args, unlines $ showArgType arg : zipWith (\n a->("  "++showArg n a)) [count..] args)
	) 0 $ filter (not.null.flattenArg) context)

showArgType (Args _ LambdaArg from) = "LambdaArg " ++ from
showArgType (Args _ (LetArg _) from) = "LetArg " ++ from

showArg n impl = snd (indexToOp n) ++ " " ++ fromMaybe "" (implName impl) ++ " :: " ++ (toHsReadType $ implType impl) ++ " " ++ explainUsedness (implUsed impl) 
-- only really useful for debugging code that checks where let statements go
-- ++ " deps: " ++ show (Set.toList $ implDeps impl)

explainUsedness OptionalArg = "" -- "(optionally used, equivalent to used in priority)"
explainUsedness UnusedArg = "(unused so far, prioritized for implicit args)"
explainUsedness UsedArg = "" -- ""
explainUsedness UsednessDoesntMatter = "usedness doesn't matter, should be impossible"

indexToOp :: Int -> ([Int], String)
indexToOp = fromMaybe (error "deBruijn index too high") . (at deBruijnArgReps) where

deBruijnArgReps :: [([Int], String)]
deBruijnArgReps = [(replicate unary 6 ++ [nib], replicate unary ';' ++ sym)
	           | unary <- [0..11], (sym,nib) <- [("$",3),("@",4),("_",5)]]