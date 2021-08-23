{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

module Args(argn, newLambdaArg, addLambda, newLetArg, popArg, debugContext, argImplicit) where

import Expr
import Types
import Header
import Hs

import Parse (parseError)
import Data.List
import Data.Maybe
import State
import qualified Data.Set as Set

argStr n tn = "arg" ++ show n ++ "t" ++ show tn

newLambdaArg :: [VT] -> ArgUsedness -> ParseState Arg
newLambdaArg argT argUsedness = do
	context <- gets pdContext
	let depth = 1 + length context
	let impls = zipWith (\t tn -> Impl {
		implType = t,
		implCode = hsAtom $ argStr depth tn,
		implDeps = Set.singleton depth,
		implName = Nothing,
		implUsed = argUsedness
		} ) argT [1..]
	let newArg = Arg impls LambdaArg
	modify $ \s -> s { pdContext=newArg:context }
	return newArg

newLetArg :: [Arg] -> Impl -> [VT] -> Arg
newLetArg context (Impl _ defHs defDepth _ _) defTypes = newArg where
	depth = 1 + length context
	impls = zipWith (\t tn -> Impl t (hsAtom $ argStr depth tn) defDepth Nothing UnusedArg) defTypes [1..]
	newArg = Arg impls $ LetArg defHs

argn :: Int -> ParseState Impl
argn deBruijnIndex = do
	context <- gets pdContext
	let flattenedArgs = concatMap flattenArg context
	case at flattenedArgs (deBruijnIndex-1) of
		Nothing -> parseError $ "Attempt to access " ++ indexToOp (deBruijnIndex-1) ++ debugContext context
		Just impl -> setUsed impl

setUsed :: Impl -> ParseState Impl
setUsed impl = do
	context <- gets pdContext
	let newContext = map (\arg -> arg { argImpls=setUsedImpl (argImpls arg) } ) context
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
		argn $ 1 + (fromMaybe 0 $ findIndex ((==UnusedArg).implUsed) (concatMap flattenArg context))

flattenArg (Arg impls (LambdaArg)) = impls
flattenArg (Arg impls (LetArg _)) = tail impls

addLambda :: Arg -> Impl -> Impl
addLambda arg (Impl t body d _ _) = Impl {
	implType = t,
	implCode = hsFn (map implCode $ argImpls arg) body,
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
	
	maybePopIt :: Impl -> Arg -> (Impl, [Arg])
	maybePopIt impl eachArg
		| Set.member depth (getArgDeps eachArg) = (popIt eachArg impl, [])
		| otherwise = (impl, [eachArg])

	popIt :: Arg -> Impl -> Impl
	popIt (Arg _ LambdaArg) impl = impl
	popIt (Arg varImpls (LetArg refHs)) (Impl retT bodyHs dep _ _) =
		Impl retT (hsLet (map implCode varImpls) refHs bodyHs) dep Nothing UsednessDoesntMatter

getArgDeps (Arg (impl : _) _) = implDeps impl -- They should all be the same
getArgDeps (Arg [] _) = implDeps noArgsUsed
--------- for debugging // errors -----------

debugContext :: [Arg] -> String
debugContext context = "\nContext:\n" ++ (unlines $ snd $ mapAccumL (\count arg ->
	let args = flattenArg arg in
	(count+length args, unlines $ showArgType arg : zipWith (\n a->("  "++showArg n a)) [count..] args)
	) 0 $ filter (not.null.flattenArg) context)

showArgType (Arg _ LambdaArg) = "LambdaArg"
showArgType (Arg _ (LetArg _)) = "LetArg"

showArg n impl = indexToOp n ++ " " ++ show (implType impl) ++ " " ++ fromMaybe "" (implName impl) ++ " used: " ++ show (implUsed impl) -- ++ " depth: " ++ show (implDeps impl)

indexToOp :: Int -> String
indexToOp = fromMaybe "<truncated>" . (at indexOps) where
	indexOps = [replicate unary ';' ++ sym | unary <- [0..11], sym <- ["$","@","_"]]