module Args(getArg, getArgN, newLambdaArg, addLambda, newLetArg, popArg) where

import Expr
import Types
import Header
import Numeric (showHex)
import Parse (nextHex)
import Data.List
import Data.Maybe

argStr n = "arg" ++ show n

newLambdaArg :: [Arg] -> VT -> ([Arg], Arg)
newLambdaArg context argT = (newArg : context, newArg) where
	depth = 1+foldr (const . getArgDepth) 0 context
	impl = Impl argT (HsAtom $ argStr depth) depth
	newArg = Arg impl depth LambdaArg

newLetArg :: [Arg] -> Impl -> VisibleFirst -> ([Arg], Arg)
newLetArg context (Impl defType defHs defDepth) visible = (newArg : context, newArg) where
	depth = 1+foldr (const . getArgDepth) 0 context
	impl = Impl defType (HsAtom $ argStr depth) depth
	newArg = Arg impl depth $ LetArg defDepth defHs visible

argn :: [Arg] -> Int -> Impl
argn context deBruijnIndex = 
		fromMaybe (error "negative arg index todo better msg") $
			at flattenedArgs deBruijnIndex
	where
		flattenedArgs = concatMap flattenArg context
		flattenArg (Arg (Impl VTuple0 hs depth) _ k) = []
		flattenArg (Arg (Impl (VPair a b) hs depth) _ k) =
			(flattenArg $ Arg (Impl a (app1 "fst" hs) depth) undefined k) ++
			(flattenArg $ Arg (Impl b (app1 "snd" hs) depth) undefined visibleArg)
		flattenArg (Arg impl _ k)
			| isVisible k = [impl]
			| otherwise = []
			
		visibleArg = LambdaArg
		isVisible (LetArg _ _ Hidden) = False
		isVisible _ = True

addLambda :: Arg -> Impl -> Impl
addLambda arg (Impl t body d) = Impl t (HsFn (getCode arg) body) d

popArg :: Int -> [Arg] -> Impl -> ([Arg], Impl)
popArg depth context impl = (concat finalContext, finalImpl) where
	(finalImpl, finalContext) = mapAccumL maybePopIt impl context

	maybePopIt :: Impl -> Arg -> (Impl, [Arg])
	maybePopIt impl eachArg
		| getArgDepDepth eachArg >= depth = (popIt eachArg impl, [])
		| otherwise = (impl, [eachArg])
	
	getArgDepDepth (Arg _ dep LambdaArg) = dep
	getArgDepDepth (Arg _ _ (LetArg dep _ _)) = dep

	popIt :: Arg -> Impl -> Impl
	popIt (Arg _ _ LambdaArg) impl = impl
	popIt (Arg (Impl _ varHs dep) _ (LetArg _ refHs _)) (Impl retT bodyHs _) =
		Impl retT (HsApp (HsFn (getCode2 varHs) bodyHs) refHs) dep

getCode (Arg (Impl _ (HsAtom hs) _) _ _) = hs
getCode2 (HsAtom hs) = hs

getArg :: Int -> Rep -> Thunk -> (Thunk, Expr)
getArg n r thunk = (thunk, Expr r $ argn (getContext thunk) n) where
	
getArgN :: Rep -> Thunk -> (Thunk, Expr)
getArgN r (Thunk code context) =
	(Thunk afterSlashCode context, Expr rep impl) where
		rep = addRep r $ Rep [v] $ showHex v ""
		impl = argn context v
		(v, afterSlashCode) = nextHex code
