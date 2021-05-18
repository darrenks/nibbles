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

applyExpr :: Expr -> Expr -> Expr
applyExpr (Expr t1 b1 l1 hs1) (Expr t2 b2 l2 hs2) =
	Expr t1 (b1++b2) (l1++l2) (HsApp hs1 hs2)

convertAutoType VAuto = VInt
convertAutoType t = t
convertAuto (Expr VAuto b l _) auto = Expr VInt b l $ i $ fromIntegral auto
convertAuto e _ = e

convertAutos :: [Expr] -> [Int] -> [Expr]
convertAutos l autos = zipWith (\(e) a -> (convertAuto e a)) l (autos ++ repeat undefined)

simplifyArgSpecs :: [ArgSpec] -> [[VT] -> Maybe ArgMatchResult]
simplifyArgSpecs = map simplifyArgSpec
simplifyArgSpec (Exact spec) vts = maybeMatch $ spec == convertAutoType (last vts)
simplifyArgSpec (Fn f) vts = Just $ ArgFnOf $ f $ init vts
simplifyArgSpec (Cond _ f) vts = maybeMatch $ f vts -- last

maybeMatch b = if b then Just ArgMatches else Nothing

convertLambdas :: Thunk -> [(ArgMatchResult, (Thunk, Expr))] -> (Thunk, [Expr])
convertLambdas = mapAccumL convertLambda

convertLambda :: Thunk -> (ArgMatchResult, (Thunk, Expr)) -> (Thunk, Expr)
convertLambda _ (ArgMatches, arg) = arg
convertLambda (Thunk code vt) (ArgFnOf argType, _) = (afterFnThunk, addLambda vt argType fnExpr) where
	(afterFnThunk, fnExpr) = head $ toExprList $ Thunk code $ argType:vt

getValue :: Thunk -> (Thunk, Expr)
getValue (Thunk code contextTs) = fromMaybe fail $ msum $ map tryOp ops where
	fail = parseError "no matching op" $ Thunk code contextTs
	tryOp (lit, nib, op) = match code (lit, nib) >>= \afterOpCode -> let
		afterOpThunk = (Thunk afterOpCode contextTs)
		-- -1
		valList :: [(Thunk, Expr)]
		valList = toExprList $ Thunk (foldr (\_ c->nextOffset c) code [1..(cp afterOpCode - cp code)]) contextTs
		partialExpr = (Expr undefined nib lit undefined)
		in convertOp partialExpr afterOpThunk valList op

convertOp :: Expr -> Thunk -> [(Thunk, Expr)] -> Operation -> Maybe (Thunk, Expr)
convertOp partialExpr afterOpThunk valList (Op atso impl autos) = 
	if all isJust typeMatch then Just makeExpr else Nothing where
		valTypes = map (retT.snd) valList
		ats = simplifyArgSpecs atso
		typeMatch = zipWith id ats (tail $ inits valTypes)
		isFns = map fromJust typeMatch
		(nextCode, argList) = convertLambdas afterOpThunk $ zip isFns valList
		makeExpr = (nextCode, foldl applyExpr initExpr (convertAutos argList autos))
		initExpr = setTAndHs partialExpr rt (HsAtom $ "("++hs++")")
		(rt, hs) = impl $ map retT argList

convertOp partialExpr afterOpThunk _ (Atom impl) = Just $ impl partialExpr afterOpThunk
convertOp partialExpr afterOpThunk _ Let = Just $ lets partialExpr afterOpThunk

lets :: Expr -> Thunk -> (Thunk, Expr)
lets (Expr _ nib lit _) (Thunk code vt) =
	(Thunk nextCode (t:nextTypes), Expr t (nib++b) (lit++l) $ HsLet hs)
		where
			(Thunk nextCode nextTypes, Expr t b l hs) = getValue (Thunk code vt)
