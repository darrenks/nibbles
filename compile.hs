-- todo
-- clean up parse
-- more coercers
-- multiletter commands
-- allow flipped ops if diff type

module Compile(compile) where

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
import InputCode
import Args

compile :: InputCode ic => ic -> Expr
compile input=snd $ getValue $ Thunk input[]

coerce2(NoType, b) = b

coerce2(a, Vec b) = coerce2(a, b)

coerce2(VInt a, VInt b) = VInt (a||b)
coerce2(VInt a, VList (VInt True)) = str
coerce2(VList (VInt True), VInt a) = str
coerce2(VList a, VInt b) = VList $ coerce2(a, VInt b)
coerce2(VInt b, VList a) = coerce2(VList a, VInt b)
coerce2(VList a, VList b) = VList $ coerce2(a, b)

coerceTo :: VT -> Expr -> Expr
coerceTo to from = setT to $ case coerceToH (to, retT from) of
	"" -> from
	s -> applyHs ("("++s++")") from

coerceToH (VList (VInt True), VInt False) = "sToA.show"
coerceToH (VList a, VInt False) = "\\x->[x]"
--todo need more, think through
coerceToH _ = ""


-- todo these aren't used yet
dim (VList e) = 1 + dim e
dim _ = 0
-- dim where string is a scalar
sdim (VList (VInt True)) = 0
sdim (VList e) = 1 + sdim e
sdim _ = 0

type ArgType = VT

-- todo might want to override with str, etc
innermostElem (VList t) = innermostElem t
innermostElem t = t

argMatch (Vec t) b priorArgs = argMatch t (innermostElem b) priorArgs
argMatch (Fn _) _ _ = True
argMatch a VAuto priorArgs = argMatch a int priorArgs
argMatch (Cond _ a) b priorArgs = a priorArgs b
argMatch (Coerce a) b priorArgs = argMatch a b priorArgs
argMatch (PromoteList a) b priorArgs = argMatch a b priorArgs
argMatch (VList a) (VList b) priorArgs = argMatch a b priorArgs
argMatch a b _ = a==b

todo = error "todo"

toExprList :: InputCode ic => Thunk ic -> [(ic, Expr)]
toExprList (Thunk code vt) =
	(rest, e) : toExprList (Thunk rest vt) where
		(rest, e) = getValue (Thunk code vt)

type Accum ic = (Thunk ic, [VT], VT)

handle (Thunk _ vt, prefixTs, coercedType) (afterArgCode, argExpr) exprFn =
	((Thunk afterArgCode vt, retT argExpr:prefixTs, coercedType), \ct -> exprFn ct argExpr)

-- todo support coerce on Fn, etc...
prepassArg :: InputCode ic => Accum ic -> (ArgType, (ic, Expr)) -> (Accum ic, VT -> Expr)
prepassArg (Thunk code vt, prefixTs, coercedType) (Fn fnT, _) =
	((Thunk afterFnCode vt, prefixTs, coercedType), \_ -> addLambda vt argT fnExpr) where
		argT = fnT $ reverse prefixTs
		(afterFnCode, fnExpr) = getValue $ Thunk code $ argT:vt
prepassArg state (Coerce argSpec, arg) =
	((thunk, prefixTs, coerce2 (coercedType, retT $ snd arg)), \ct -> coerceTo ct (f ct)) where
		((thunk, prefixTs, coercedType), f) = prepassArg state (argSpec, arg)
prepassArg state (PromoteList argSpec, arg) =
	((thunk, prefixTs, coercedType), (\ct -> promoteList $ f ct)) where
		((thunk, prefixTs, coercedType), f) = prepassArg state (argSpec, arg)
prepassArg state (Vec argSpec, (ic, Expr (VList t) b l hs)) = (accum, \ct->appT Vec (f ct))
	where (accum, f) = prepassArg state (Vec argSpec, (ic, Expr t b l hs))
prepassArg state (Vec argSpec, arg) = prepassArg state (argSpec, arg)
prepassArg state (argSpec, arg) = handle state arg $ flip const

unVec (Vec t) = VList (unVec t)
unVec t = t

massageArgs :: InputCode ic => Thunk ic -> [(ArgType, (ic, Expr))] -> (ic, [Expr])
massageArgs thunk args = (code, resultArgs) where
	((Thunk code vt, _, coercedType), prepass) = mapAccumL prepassArg (thunk, [], NoType) args
	resultArgs = map (\x->x coercedType) prepass

promoteList t | isList $ retT t = t
promoteList (Expr t b l hs) = Expr (VList t) b l ("["++hs++"]")

applyHs :: String -> Expr -> Expr
applyHs s (Expr t b l hs) = Expr t b l $ app1 s hs

applyExpr :: Expr -> Expr -> Expr
applyExpr e1 (Expr (Vec t2) b2 l2 hs2) = appT unVec $ applyExpr (applyHs "map" e1) (Expr t2 b2 l2 hs2)
applyExpr (Expr t1 b1 l1 hs1) (Expr t2 b2 l2 hs2) =
	Expr t1 (b1++b2) (l1++l2) (app1 hs1 hs2)

convertAutoType VAuto = int
convertAutoType t = t
convertAuto (Expr VAuto b l _) auto = Expr int b l (show auto)
convertAuto e auto = e
convertAutos l autos = zipWith (\(c,e) a -> (c,convertAuto e a)) l (autos ++ repeat undefined)

getValue :: InputCode ic => Thunk ic -> (ic, Expr)
getValue thunk = fromMaybe fail $ msum (map tryOp (getOps thunk)) where
	fail = parseError "no matching op" thunk
	afterOpCode = nextInstruction code
	afterOpThunk = (Thunk afterOpCode contextTs)
	valList = toExprList afterOpThunk
	valTypes = map (convertAutoType.retT.snd) valList
	Thunk code contextTs = thunk
	tryOp (lit, nib, op) = if match code (lit, nib) then convertOp op else Nothing where 
		partialExpr = (Expr undefined nib lit undefined)	
		convertOp (Op ats impl autos) =
			if typeMatch then Just $ makeExpr else Nothing where
				typeMatch = and $ zipWith3 argMatch ats valTypes (init $ inits valTypes)
				(nextCode, argList) =  massageArgs afterOpThunk $ zip ats (convertAutos (valList) autos)
				makeExpr = (nextCode, foldl applyExpr initExpr argList)
				initExpr = setTAndHs partialExpr rt ("("++hs++")")
				(rt, hs) = impl $ map retT argList
		convertOp (Atom impl) = 
			Just $ impl contextTs partialExpr afterOpCode

