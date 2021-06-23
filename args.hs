module Args(argn, newLambdaArg, addLambda, newLetArg, popArg, flattenArg) where

import Expr
import Types
import Header
import Hs

import Parse (nextHex)
import Data.List
import Data.Maybe
import State

argStr n tn = "arg" ++ show n ++ "t" ++ show tn

newLambdaArg :: [VT] -> ParseState Arg
newLambdaArg argT = do
	context <- gets pdContext
	let depth = 1 + length context
	let impls = zipWith (\t tn -> Impl t (hsAtom $ argStr depth tn) depth) argT [1..]
	let newArg = Arg impls LambdaArg
	modify $ \s -> s { pdContext=newArg:context }
	return newArg

newLetArg :: [Arg] -> Impl -> [VT] -> Arg
newLetArg context (Impl _ defHs defDepth) defTypes = newArg where
	depth = 1 + length context
	impls = zipWith (\t tn -> Impl t (hsAtom $ argStr depth tn) defDepth) defTypes [1..]
	newArg = Arg impls $ LetArg defHs

argn :: Int -> ParseState Impl
argn deBruijnIndex = do
	context <- gets pdContext
	let flattenedArgs = concatMap flattenArg context
	return $ fromMaybe (error "negative arg index todo better msg") $
			at flattenedArgs (deBruijnIndex-1)

flattenArg (Arg impls (LambdaArg)) = impls
flattenArg (Arg impls (LetArg _)) = tail impls

addLambda :: Arg -> Impl -> Impl
addLambda arg (Impl t body d) = Impl t (hsFn (map implCode $ argImpls arg) body) d

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
	
	getArgDepDepth (Arg (Impl _ _ dep : _) _) = dep
	getArgDepDepth (Arg [] _) = minUsedDepth noArgsUsed

	popIt :: Arg -> Impl -> Impl
	popIt (Arg _ LambdaArg) impl = impl
	popIt (Arg varImpls (LetArg refHs)) (Impl retT bodyHs dep) =
		Impl retT (hsLet (map implCode varImpls) refHs bodyHs) dep
