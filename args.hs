module Args(getArg, getArgN, newLambdaArgs, addLambda, newLetArg, popArg) where

import Expr
import Types
import Numeric (showHex)
import Parse (nextHex)
import Data.List

argStr n = "arg" ++ show n

newLambdaArgs :: [Arg] -> VT -> ([Arg], [Arg])
newLambdaArgs context argT = (newArgs ++ context, newArgs) where
	initDepth = 1+foldr (const . getArgDepth) 0 context
	(_, newArgs) = mapAccumL addLambdaArg initDepth (flattenArg argT)

	addLambdaArg :: Int -> VT -> (Int, Arg)
	addLambdaArg depth t = (depth+1, toLambdaArg t depth)

	toLambdaArg :: VT -> Int -> Arg
	toLambdaArg t depth = arg where
		impl = Impl t [HsAtom $ argStr depth] depth
		arg = Arg impl depth LambdaArg

-- only returns last arg of many newly gen from pair
newLetArg :: [Arg] -> Impl -> ([Arg], Arg)
newLetArg context (Impl defType [defHs] defDepth) = (newArgs ++ context, head newArgs) where
	initDepth = 1+foldr (const . getArgDepth) 0 context
	(_, newArgs) = mapAccumL addLetArg initDepth (flattenArg defType)
		
	addLetArg :: Int -> VT -> (Int, Arg)
	addLetArg depth t = (depth+1, toLetArg t depth)

	toLetArg :: VT -> Int -> Arg
	toLetArg t depth = arg where
		impl = Impl t [HsAtom $ argStr depth] depth
		arg = Arg impl depth $ LetArg defDepth defHs

--todo what about pairs of pairs
argn :: [Arg] -> Int -> Impl
argn [] _ = error $ "negative arg index"
argn (Arg (Impl (VPair a b) [hs1,hs2] depth) _ _ : r) 0 = Impl a [hs1] depth
argn (Arg (Impl (VPair a b) [hs1,hs2] depth) _ _ : r) 1 = Impl b [hs2] depth
-- argn ((Arg (Impl (VPair a b) depth [hs,_]) _ _ : r) _ _) 0 = Impl a depth [hs]
-- argn (Arg (Impl (VPair a b) _ hs : r) _ _) 1 = Impl b
argn (Arg impl _ _:_) 0 = impl
argn (_:r) n = argn r (n-1)

flattenArg :: VT -> [VT]
flattenArg (VPair a b) = flattenArg a ++ flattenArg b
flattenArg a = [a]

addLambda :: [Arg] -> Impl -> Impl
addLambda args (Impl t [body] d) = Impl t [HsFn (map getCode args) body] d -- todo undefined because lambdas aren't first class yet

popArg :: Int -> [Arg] -> Impl -> ([Arg], Impl)
popArg depth context impl = (concat finalContext, finalImpl) where
	(finalImpl, finalContext) = mapAccumR maybePopIt impl context

	maybePopIt :: Impl -> Arg -> (Impl, [Arg])
	maybePopIt impl eachArg
		| getArgDepDepth eachArg >= depth = (popIt eachArg impl, [])
		| otherwise = (impl, [eachArg])
	
	getArgDepDepth (Arg _ dep LambdaArg) = dep
	getArgDepDepth (Arg _ _ (LetArg dep _)) = dep

	popIt :: Arg -> Impl -> Impl
	popIt (Arg _ _ LambdaArg) impl = impl
	popIt (Arg (Impl _ varHs _) _ (LetArg _ refHs)) (Impl retT [bodyHs] _) =
		Impl retT [HsApp (HsFn (map getCode2 varHs) bodyHs) refHs] undefined

getCode (Arg (Impl _ [HsAtom hs] _) _ _) = hs
getCode2 (HsAtom hs) = hs

getArg :: Int -> Rep -> Thunk -> (Thunk, Expr)
getArg n r thunk = (thunk, Expr r $ argn (getContext thunk) n) where
	
getArgN :: Rep -> Thunk -> (Thunk, Expr)
getArgN r (Thunk code context) =
	(Thunk afterSlashCode context, Expr rep impl) where
		rep = addRep r $ Rep [v] $ showHex v ""
		impl = argn context v
		(v, afterSlashCode) = nextHex code
