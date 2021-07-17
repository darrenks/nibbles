module Args(argn, newLambdaArg, addLambda, newLetArg, popArg, debugContext) where

import Expr
import Types
import Header
import Hs

import Parse (parseError)
import Data.List
import Data.Maybe
import State

argStr n tn = "arg" ++ show n ++ "t" ++ show tn

newLambdaArg :: [VT] -> ParseState Arg
newLambdaArg argT = do
	context <- gets pdContext
	let depth = 1 + length context
	let impls = zipWith (\t tn -> Impl t (hsAtom $ argStr depth tn) depth Nothing False) argT [1..]
	let newArg = Arg impls LambdaArg
	modify $ \s -> s { pdContext=newArg:context }
	return newArg

newLetArg :: [Arg] -> Impl -> [VT] -> Arg
newLetArg context (Impl _ defHs defDepth _ _) defTypes = newArg where
	depth = 1 + length context
	impls = zipWith (\t tn -> Impl t (hsAtom $ argStr depth tn) defDepth Nothing False) defTypes [1..]
	newArg = Arg impls $ LetArg defHs

argn :: Int -> ParseState Impl
argn deBruijnIndex = do
	context <- gets pdContext
	let flattenedArgs = concatMap flattenArg context
	case at flattenedArgs (deBruijnIndex-1) of
		Nothing -> parseError $ "Attempt to access " ++ indexToOp (deBruijnIndex-1) ++ debugContext context
		Just impl -> return impl

flattenArg (Arg impls (LambdaArg)) = impls
flattenArg (Arg impls (LetArg _)) = tail impls

addLambda :: Arg -> Impl -> Impl
addLambda arg (Impl t body d _ _) = Impl t (hsFn (map implCode $ argImpls arg) body) d Nothing False

-- Remove arg # and all its dependent let args (adding let statements for them).
popArg :: Int -> Impl -> ParseState Impl
popArg depth impl = do
	context <- gets pdContext
	let (finalImpl, finalContext) = mapAccumL maybePopIt impl context
	modify $ \s -> s { pdContext=concat finalContext }
	return finalImpl where
	
	maybePopIt :: Impl -> Arg -> (Impl, [Arg])
	maybePopIt impl eachArg
		| getArgDepDepth eachArg >= depth = (popIt eachArg impl, [])
		| otherwise = (impl, [eachArg])
	
	getArgDepDepth (Arg (impl : _) _) = minUsedDepth impl -- They should all be the same
	getArgDepDepth (Arg [] _) = minUsedDepth noArgsUsed

	popIt :: Arg -> Impl -> Impl
	popIt (Arg _ LambdaArg) impl = impl
	popIt (Arg varImpls (LetArg refHs)) (Impl retT bodyHs dep _ _) =
		Impl retT (hsLet (map implCode varImpls) refHs bodyHs) dep Nothing False

--------- for debugging // errors -----------

debugContext :: [Arg] -> String
debugContext context = "\nContext:\n" ++ (unlines $ snd $ mapAccumL (\count arg ->
	let args = flattenArg arg in
	(count+length args, unlines $ showArgType arg : zipWith (\n a->("  "++showArg n a)) [count..] args)
	) 0 $ filter (not.null.flattenArg) context)

showArgType (Arg _ LambdaArg) = "LambdaArg"
showArgType (Arg _ (LetArg _)) = "LetArg"

showArg n impl = indexToOp n ++ " " ++ show (implType impl) ++ " " ++ fromMaybe "" (implName impl)

indexToOp :: Int -> String
indexToOp = fromMaybe "<truncated>" . (at indexOps) where
	indexOps = [replicate unary ';' ++ sym | unary <- [0..11], sym <- ["$","@","_"]]