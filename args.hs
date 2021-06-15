module Args(getArg, getArgN, newLambdaArg, addLambda, newLetArg, popArg) where

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
	impls = zipWith (\t tn -> SImpl t (HsAtom $ argStr depth tn) depth) argT [1..]
	newArg = Arg impls LambdaArg

newLetArg :: [Arg] -> Impl -> Arg
newLetArg context (Impl defTypes defHs defDepth) = newArg where
	depth = 1 + length context
	impls = zipWith (\t tn -> SImpl t (HsAtom $ argStr depth tn) defDepth) defTypes [1..]
	newArg = Arg impls $ LetArg defHs

argn :: [Arg] -> Int -> SImpl
argn context deBruijnIndex = 
		fromMaybe (error "negative arg index todo better msg") $
			at flattenedArgs deBruijnIndex
	where
		flattenedArgs = concatMap flattenArg context
		-- flattenArg (Arg (Impl VTuple0 hs depth) _ k) = []
-- 		flattenArg (Arg (Impl (VPair a b) hs depth) _ k) =
-- 			(flattenArg $ Arg (Impl a (app1 "fst" hs) depth) undefined k) ++
-- 			(flattenArg $ Arg (Impl b (app1 "snd" hs) depth) undefined visibleArg)
-- 		flattenArg (Arg (Impl (VP ts) hs depth) _ k) = zipWith (\t n->
-- 			Impl t (HsAtom $ (flatHs hs) ++ "t" ++ show n) depth
-- 			) ts [1..]
		flattenArg (Arg impls (LambdaArg)) = impls
		flattenArg (Arg impls (LetArg _)) = tail impls
			
		visibleArg = LambdaArg

addLambda :: Arg -> SImpl -> SImpl
addLambda arg (SImpl t body d) = SImpl t (HsFn (argsLhs $ map (flatHs . getHs) $ getArgImpls arg) body) d
	where
		argsLhs [] = "()"
		argsLhs hss = intercalate " " $ hss

-- Remove arg # and all its dependent let args (adding let statements for them).
popArg :: Int -> [Arg] -> SImpl -> ([Arg], SImpl)
popArg depth context impl = (concat finalContext, finalImpl) where
	(finalImpl, finalContext) = mapAccumL maybePopIt impl context

	maybePopIt :: SImpl -> Arg -> (SImpl, [Arg])
	maybePopIt impl eachArg
		| getArgDepDepth eachArg >= depth = (popIt eachArg impl, [])
		| otherwise = (impl, [eachArg])
	
	getArgDepDepth (Arg (SImpl _ _ dep : _) _) = dep
	getArgDepDepth (Arg [] _) = 0 --todo

	popIt :: Arg -> SImpl -> SImpl
	popIt (Arg _ LambdaArg) impl = impl
	popIt (Arg varImpls (LetArg refHs)) (SImpl retT bodyHs dep) =
		SImpl retT (HsApp (HsFn ("("++(intercalate "," $ map (flatHs . getHs) varImpls)++")") bodyHs) refHs) dep

-- getCode (Arg (Impl _ (HsAtom hs) _) _ _) = hs

getArg :: Int -> Rep -> Thunk -> (Thunk, SExpr)
getArg n r thunk = (thunk, SExpr r $ argn (getContext thunk) n) where
	
getArgN :: Rep -> Thunk -> (Thunk, SExpr)
getArgN r (Thunk code context) =
	(Thunk afterSlashCode context, SExpr rep impl) where
		rep = addRep r $ Rep [v] $ showHex v ""
		impl = argn context v
		(v, afterSlashCode) = nextHex code
