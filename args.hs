module Args(getArg, getArgN, newLambdaArg, addLambda, newLetArg, popArg, flattenArg) where

import Expr
import Types
import Header
import Numeric (showHex)
import Parse (nextHex)
import Data.List
import Data.Maybe

argStr n tn = "arg" ++ show n ++ "t" ++ show tn

newLambdaArg :: [Arg] -> [VT] -> ([Arg], Arg)
newLambdaArg context argT = (newArg : context, newArg) where
	depth = 1 + length context
	impls = zipWith (\t tn -> Impl t (HsAtom $ argStr depth tn) depth) argT [1..]
	newArg = Arg impls LambdaArg

newLetArg :: [Arg] -> Impl -> [VT] -> Arg
newLetArg context (Impl _ defHs defDepth) defTypes = newArg where
	depth = 1 + length context
	impls = zipWith (\t tn -> Impl t (HsAtom $ argStr depth tn) defDepth) defTypes [1..]
	newArg = Arg impls $ LetArg defHs

argn :: [Arg] -> Int -> Impl
argn context deBruijnIndex = 
		fromMaybe (error "negative arg index todo better msg") $
			at flattenedArgs deBruijnIndex
	where
		flattenedArgs = concatMap flattenArg context

flattenArg (Arg impls (LambdaArg)) = impls
flattenArg (Arg impls (LetArg _)) = tail impls

addLambda :: Arg -> Impl -> Impl
addLambda arg (Impl t body d) = Impl t (HsFn (argsLhs $ map (flatHs . getHs) $ getArgImpls arg) body) d
	where
		argsLhs [] = "()"
		argsLhs hss = intercalate " " $ hss

-- Remove arg # and all its dependent let args (adding let statements for them).
popArg :: Int -> [Arg] -> Impl -> ([Arg], Impl)
popArg depth context impl = (concat finalContext, finalImpl) where
	(finalImpl, finalContext) = mapAccumL maybePopIt impl context

	maybePopIt :: Impl -> Arg -> (Impl, [Arg])
	maybePopIt impl eachArg
		| getArgDepDepth eachArg >= depth = (popIt eachArg impl, [])
		| otherwise = (impl, [eachArg])
	
	getArgDepDepth (Arg (Impl _ _ dep : _) _) = dep
	getArgDepDepth (Arg [] _) = 0 --todo

	popIt :: Arg -> Impl -> Impl
	popIt (Arg _ LambdaArg) impl = impl
	popIt (Arg varImpls (LetArg refHs)) (Impl retT bodyHs dep) =
		Impl retT (HsLet (map getHs varImpls) refHs bodyHs) dep

getArg :: Int -> Rep -> Thunk -> (Thunk, Expr)
getArg n r thunk = (thunk, Expr r $ argn (getContext thunk) n) where
	
getArgN :: Rep -> Thunk -> (Thunk, Expr)
getArgN r (Thunk code context) =
	(Thunk afterSlashCode context, Expr rep impl) where
		rep = addRep r $ Rep [v] $ showHex v ""
		impl = argn context v
		(v, afterSlashCode) = nextHex code
