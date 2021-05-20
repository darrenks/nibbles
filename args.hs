module Args where -- (getArg, getArgN, addLambda, newLet, addLetsToExpr, addLambdaArgToContext) where

import Expr
import Types
import Numeric (showHex)
import Parse (nextHex)
import Data.List

argStr n = "arg" ++ show n

-- toArgs :: Context -> VT -> [Arg]
-- toArgs (Context _ size seen) t = snd $ mapAccumL toArgAccum 0 (flattenArg t) where
-- 	toArgAccum :: Int -> VT -> (Int, Arg)
-- 	toArgAccum n t = (n+1, toArg t n)
-- 	toArg :: VT -> Int -> Arg
-- 	toArg t n = Arg (Impl t (HsAtom $ argStr $ seen+n) (size+n)) LambdaArg

-- addToContext :: [Arg] -> Context -> Context
-- addToContext newArgs (Context args size seen) =
-- 	Context (newArgs++args) (size + argLen) (seen + argLen) where
-- 		argLen = length newArgs

-- -- todo for now assume now pair
-- toLetArgs :: Context -> Impl -> [Arg]
-- toLetArgs context (Impl t hs minDepth) =
-- 	[Arg varImpl (LetArg minDepth hs)] where
-- 		varImpl = Impl t (HsAtom $ argStr seen) minDepth
-- 		(Context _ _ seen) = context
-- 
--                        (new context, newly added args)

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

		
-- addLetToContext :: VT -> ArgKind -> Context -> Context

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

-- addLambda :: Context -> VT -> Expr -> Expr
-- addLambda context argT (Expr r (Impl t hs _)) =
-- 	Expr r $ Impl t (HsFn (map getCode $ toArgs context argT) hs) undefined -- todo undefined because lambdas aren't first class yet

-- addLetsToExpr :: Context -> Expr -> Context -> (Expr, [Arg])
-- addLetsToExpr origContext fn afterFnContext = (wlets, superLets) where
-- 		newLets = filter isLet afterFnArgs
-- 		(superLets, theseLets) = partition (isSuperLet.getArgData) newLets
-- 		Context _ depth _ = origContext
-- 		Context afterFnArgs _ afterFnSeen = afterFnContext
-- 		isSuperLet (LetArg d _) = d < depth
-- 		(wlets, _) = addLets (fn, depth) theseLets
-- 		
-- addLets :: (Expr, Int) -> [Arg] -> (Expr, Int)
-- addLets lambda lets = foldl addLet lambda lets
-- 
-- addLet :: (Expr, Int) -> Arg -> (Expr, Int)
-- addLet (Expr r (Impl lambdaT lambdaHs _), depth) (Arg argT (LetArg _ argHs)) =
-- 	(Expr r $ Impl lambdaT newHs undefined, depth+1) where
-- 		newHs = HsApp (HsFn [argStr depth] lambdaHs) argHs --todo hardcoded 1 arg
-- 
-- 

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
