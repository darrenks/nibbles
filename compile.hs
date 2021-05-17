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

coerce2 :: (VT, VT) -> VT
coerce2(NoType, b) = b
coerce2(a, VVec b) = coerce2(a, b)
coerce2(VInt, VInt) = VInt
coerce2(a, b) | isNum a && isNum b = VChr
coerce2(a, VList VChr) | isNum a = vstr
coerce2(VList VChr, a) | isNum a = vstr
coerce2(VList a, b) | isNum b = VList $ coerce2(a, b)
coerce2(b, VList a) | isNum b = coerce2(VList a, b)
coerce2(VList a, VList b) = VList $ coerce2(a, b)

coerceTo :: VT -> Expr -> Expr
coerceTo to from = setT to (applyHs (coerceToH (to, retT from)) from)

coerceToH (VList VChr, VInt) = "(sToA.show)"
coerceToH (VList a, VInt) = "(\\x->[x])"
--todo need more, think through
coerceToH _ = ""


-- todo these aren't used yet
dim (VList e) = 1 + dim e
dim _ = 0
-- dim where string is a scalar
sdim (VList VChr) = 0
sdim (VList e) = 1 + sdim e
sdim _ = 0

-- todo might want to override with str, etc
innermostElem (VList t) = innermostElem t
innermostElem t = t

argMatch :: ArgSpec -> VT -> [VT] -> Bool
argMatch (Vec t) b priorArgs = argMatch t (innermostElem b) priorArgs
argMatch (Fn _) _ _ = True
argMatch a VAuto priorArgs = argMatch a VInt priorArgs
argMatch (Cond _ a) b priorArgs = a priorArgs b
argMatch (Coerce a) b priorArgs = argMatch a b priorArgs
argMatch (PromoteList a) b priorArgs = argMatch a b priorArgs
argMatch (Exact (VList a)) (VList b) priorArgs = argMatch (Exact a) b priorArgs
argMatch (Exact a) b _ = a==b

todo = error "todo"

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

type Accum = (Thunk, [VT], VT)

handle (Thunk _ vt, prefixTs, coercedType) (afterArgThunk, argExpr) exprFn =
	((afterArgThunk, retT argExpr:prefixTs, coercedType), \ct -> exprFn ct argExpr)

-- todo support coerce on Fn, etc
-- todo this could be cleaned up now that coerce and vec aren't used at same time
-- todo also convert artype to argspec
prepassArg :: Accum -> (ArgSpec, (Thunk, Expr)) -> (Accum, VT -> Expr)
prepassArg (Thunk code vt, prefixTs, coercedType) (Fn fnT, _) =
	((afterFnThunk, prefixTs, coercedType), \_ -> addLambda vt argT fnExpr) where
		argT = fnT $ reverse prefixTs
		(afterFnThunk, fnExpr) = head $ toExprList $ Thunk code $ argT:vt
prepassArg state (Coerce argSpec, arg) =
	((thunk, prefixTs, coerce2 (coercedType, retT $ snd arg)), \ct -> coerceTo ct (f ct)) where
		((thunk, prefixTs, coercedType), f) = prepassArg state (argSpec, arg)
prepassArg state (PromoteList argSpec, arg) =
	((thunk, prefixTs, coercedType), (\ct -> promoteList $ f ct)) where
		((thunk, prefixTs, coercedType), f) = prepassArg state (argSpec, arg)
prepassArg state (Vec argSpec, (ic, Expr (VList t) b l hs)) = (accum, \ct->appT VVec (f ct))
	where (accum, f) = prepassArg state (Vec argSpec, (ic, Expr t b l hs))
prepassArg state (Vec argSpec, arg) = prepassArg state (argSpec, arg)
prepassArg state (argSpec, arg) = handle state arg $ flip const

unVec (VVec t) = VList (unVec t)
unVec t = t

massageArgs :: Thunk -> [(ArgSpec, (Thunk, Expr))] -> (Thunk, [Expr])
massageArgs thunk args = (afterArgThunk, resultArgs) where
	((afterArgThunk, _, coercedType), prepass) = mapAccumL prepassArg (thunk, [], NoType) args
	resultArgs = map (\x->x coercedType) prepass

promoteList t | isList $ retT t = t
promoteList (Expr t b l hs) = Expr (VList t) b l (app1 "(\\x->[x])" hs)

applyHs :: String -> Expr -> Expr
applyHs s (Expr t b l hs) = Expr t b l $ app1 s hs

applyExpr :: Expr -> Expr -> Expr
applyExpr e1 (Expr (VVec t2) b2 l2 hs2) = appT unVec $ applyExpr (applyHs "map" e1) (Expr t2 b2 l2 hs2)
applyExpr (Expr t1 b1 l1 hs1) (Expr t2 b2 l2 hs2) =
	Expr t1 (b1++b2) (l1++l2) (HsApp hs1 hs2)

convertAutoType VAuto = VInt
convertAutoType t = t
convertAuto (Expr VAuto b l _) auto = Expr VInt b l $ i $ fromIntegral auto
convertAuto e auto = e
convertAutos l autos = zipWith (\(c,e) a -> (c,convertAuto e a)) l (autos ++ repeat undefined)

getValue :: Thunk -> (Thunk, Expr)
getValue (Thunk code contextTs) = fromMaybe fail $ msum $ map tryOp ops where
	fail = parseError "no matching op" $ Thunk code contextTs
	tryOp (lit, nib, op) = match code (lit, nib) >>= \afterOpCode -> let
		afterOpThunk = (Thunk afterOpCode contextTs)
		-- -1
		valList = toExprList $ Thunk (foldr (\_ c->nextOffset c) code [1..(cp afterOpCode - cp code)]) contextTs
		valTypes = map (convertAutoType.retT.snd) valList
		partialExpr = (Expr undefined nib lit undefined)	
		convertOp (Op ats impl autos) =
			if typeMatch then Just $ makeExpr else Nothing where
				typeMatch = and $ zipWith3 argMatch ats valTypes (init $ inits valTypes)
				(nextCode, argList) =  massageArgs afterOpThunk $ zip ats (convertAutos (valList) autos)
				makeExpr = (nextCode, foldl applyExpr initExpr argList)
				initExpr = setTAndHs partialExpr rt (HsAtom $ "("++hs++")")
				(rt, hs) = impl $ map retT argList
		convertOp (Atom impl) = Just $ impl partialExpr afterOpThunk
		in convertOp op

