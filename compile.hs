-- todo
-- clean up parse
-- more coercers
-- multiletter commands
-- allow flipped ops if diff type

module Compile(compile,uselessOp) where

import Data.List
import Data.Char
import Control.Monad (msum)
import Data.Maybe
import Data.Tuple

import Header
import Polylib
import Ops
import Types
import Expr
import Args
import Parse

compile :: Code -> Expr
compile input = if empty rest then e else error $ "unused code (todo make do something useful)\n"++(show rest)
	where (Thunk rest _, e) =  head $ toExprList $ Thunk (consumeWhitespace input) []

-- Gets the arg list
toExprList :: Thunk -> [(Thunk, Expr)]
toExprList thunk =
	(rest, expr) : toExprList rest where
		(rest, expr) = getValue thunk
-- toExprList = head . exprsByOffset
-- 
-- -- Gets the arg list (given an arg from each possible offset after this one?)
-- getValueL :: Thunk -> [[(Code, Expr)]] -> [(Code, Expr)]
-- getValueL thunk offsetExprs = (after,expr) : getValueL (Thunk after context) offsetAfterExprs where
-- 	(Thunk after _,expr) = getValue thunk offsetExprs
-- 	Thunk code context = thunk
-- 	offsetAfterExprs = drop (cp after-cp code) offsetExprs
-- 
-- -- Gets the arg lists from each possible offset
-- exprsByOffset :: Thunk -> [[(Code, Expr)]]
-- exprsByOffset (Thunk code vt) =
-- 	getValueL (Thunk code vt) rest : rest where
-- 		rest = exprsByOffset (Thunk (nextOffset code) vt)

-- type Accum = (Thunk, [VT], VT)
-- 
-- handle (Thunk _ vt, prefixTs, coercedType) (afterArgThunk, argExpr) exprFn =
-- 	((afterArgThunk, retT argExpr:prefixTs, coercedType), \ct -> exprFn ct argExpr)

-- todo support coerce on Fn, etc
-- todo this could be cleaned up now that coerce and vec aren't used at same time
-- todo also convert artype to argspec
-- prepassArg :: Accum -> (ArgSpec, (Thunk, Expr)) -> (Accum, VT -> Expr)
-- prepassArg (Thunk code vt, prefixTs, coercedType) (Fn fnT, _) =
-- 	((afterFnThunk, prefixTs, coercedType), \_ -> addLambda vt argT fnExpr) where
-- 		argT = fnT $ reverse prefixTs
-- 		(afterFnThunk, fnExpr) = head $ toExprList $ Thunk code $ argT:vt
-- prepassArg state (Coerce argSpec, arg) =
-- 	((thunk, prefixTs, coerce2 (coercedType, retT $ snd arg)), \ct -> coerceTo ct (f ct)) where
-- 		((thunk, prefixTs, coercedType), f) = prepassArg state (argSpec, arg)
-- prepassArg state (PromoteList argSpec, arg) =
-- 	((thunk, prefixTs, coercedType), (\ct -> promoteList $ f ct)) where
-- 		((thunk, prefixTs, coercedType), f) = prepassArg state (argSpec, arg)
-- prepassArg state (Vec argSpec, (ic, Expr (VList t) b l hs)) = (accum, \ct->appT Vec (f ct))
-- 	where (accum, f) = prepassArg state (Vec argSpec, (ic, Expr t b l hs))
-- prepassArg state (Vec argSpec, arg) = prepassArg state (argSpec, arg)
-- prepassArg state (argSpec, arg) = handle state arg $ flip const

-- unVec (Vec t) = VList (unVec t)
-- unVec t = t

-- massageArgs :: Thunk -> [(ArgSpec, (Thunk, Expr))] -> (Thunk, [Expr])
-- massageArgs thunk args = (afterArgThunk, resultArgs) where
-- 	((afterArgThunk, _, coercedType), prepass) = mapAccumL prepassArg (thunk, [], NoType) args
-- 	resultArgs = map (\x->x coercedType) prepass
-- 
-- promoteList t | isList $ retT t = t
-- promoteList (Expr t b l hs) = Expr (VList t) b l (app1 "(\\x->[x])" hs)

applyExpr :: Expr -> Expr -> Expr
-- applyExpr e1 (Expr (Vec t2) b2 l2 hs2) = appT unVec $ applyExpr (applyHs "map" e1) (Expr t2 b2 l2 hs2)
applyExpr (Expr t1 b1 l1 hs1) (Expr t2 b2 l2 hs2) =
	Expr t1 (b1++b2) (l1++l2) (HsApp hs1 hs2)

-- convertAutoType VAuto = VInt
-- convertAutoType t = t
-- convertAuto (Expr VAuto b l _) auto = Expr VInt b l $ i $ fromIntegral auto
-- convertAuto e auto = e
-- convertAutos l autos = zipWith (\(c,e) a -> (c,convertAuto e a)) l (autos ++ repeat undefined)

-- Bool for is this arg a fn?
convertLambdas :: Thunk -> [(Maybe VT, (Thunk, Expr))] -> (Thunk, [Expr])
convertLambdas = mapAccumL convertLambda

convertLambda :: Thunk -> (Maybe VT, (Thunk, Expr)) -> (Thunk, Expr)
convertLambda _ (Nothing, arg) = arg
convertLambda (Thunk code vt) (Just argType, _) = (afterFnThunk, addLambda vt argType fnExpr) where
	(afterFnThunk, fnExpr) = head $ toExprList $ Thunk code $ argType:vt

getValue :: Thunk -> (Thunk, Expr)
getValue (Thunk code contextTs) = fromMaybe fail $ msum $ map tryOp ops where
	fail = parseError "no matching op" $ Thunk code contextTs
	tryOp (lit, nib, op) = match code (lit, nib) >>= \afterOpCode -> let
		afterOpThunk = (Thunk afterOpCode contextTs)
		-- -1
		valList :: [(Thunk, Expr)]
		valList = toExprList $ Thunk (foldr (\_ c->nextOffset c) code [1..(cp afterOpCode - cp code)]) contextTs
		valTypes = map (retT.snd) valList
		partialExpr = (Expr undefined nib lit undefined)
		convertOp :: Operation -> Maybe (Thunk, Expr)		
		convertOp (Op ats impl autos) = if all isJust typeMatch then Just makeExpr else Nothing where
			typeMatch = zipWith id ats (tail $ inits valTypes)
			isFns = map fromJust typeMatch
			(nextCode, argList) = convertLambdas afterOpThunk $ zip isFns valList
			makeExpr = (nextCode, foldl applyExpr initExpr argList)
			initExpr = setTAndHs partialExpr rt (HsAtom $ "("++hs++")")
			(rt, hs) = impl $ map retT argList
		convertOp (Atom impl) = Just $ impl partialExpr afterOpThunk
		convertOp Let = Just $ lets partialExpr afterOpThunk
		in convertOp op

lets :: Expr -> Thunk -> (Thunk, Expr)
lets (Expr _ nib lit _) (Thunk code vt) =
	(Thunk nextCode (t:nextTypes), Expr t (nib++b) (lit++l) $ HsLet hs)
		where
			(Thunk nextCode nextTypes, Expr t b l hs) = getValue (Thunk code vt)
