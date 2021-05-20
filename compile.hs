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
compile input = if empty rest then prog else error $ "unused code (todo make do something useful)\n"++(show rest)
	where
-- 		(e, _) = addLetsToExpr [] e finalContext
		(Thunk rest afterContext, Expr rep body) = head $ toExprList $ Thunk (consumeWhitespace input)	[]	
		(_, progImpl) = popArg 0 afterContext body
		prog = Expr rep progImpl

-- Gets the arg list
toExprList :: Thunk -> [(Thunk, Expr)]
-- toExprList thunk =
-- 	(rest, expr) : toExprList rest where
-- 		(rest, expr) = getValue thunk
toExprList = head . exprsByOffset

-- Gets the arg list (given an arg from each possible offset after this one?)
getValueL :: Thunk -> [[(Thunk, Expr)]] -> [(Thunk, Expr)]
getValueL thunk offsetExprs = (afterThunk, expr) : getValueL afterThunk offsetAfterExprs where
	(afterThunk,expr) = getValue thunk offsetExprs
	Thunk after _ = afterThunk
	Thunk code context = thunk
	offsetAfterExprs = drop (cp after-cp code) offsetExprs
-- 
-- Gets the arg lists from each possible offset
exprsByOffset :: Thunk -> [[(Thunk, Expr)]]
exprsByOffset (Thunk code vt) =
	getValueL (Thunk code vt) rest : rest where
		rest = exprsByOffset (Thunk (nextOffset code) vt)

applyExpr :: Expr -> Expr -> Expr
applyExpr (Expr r1 (Impl t1 [hs1] d1)) (Expr r2 (Impl _ [hs2] d2)) =
	Expr (addRep r1 r2) (Impl t1 [HsApp hs1 hs2] (min d1 d2))

convertAutoType VAuto = VInt
convertAutoType t = t

convertAuto (Expr r (Impl VAuto _ _)) auto =
	Expr r $ Impl VInt [i $ fromIntegral auto] noArgsUsed
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
convertLambda (Thunk code origContext) (ArgMatches, (Thunk argCode argContext, expr)) =
	if origContext == argContext
		then (Thunk argCode argContext, expr) -- use memoized expr
		else head $ toExprList $ Thunk code origContext -- recompute expr
convertLambda (Thunk code origContext) (ArgFnOf argType, _) = 
	(Thunk afterFnCode finalContext, Expr rep lambda) where
		(lambdaContext, newArgs) = newLambdaArgs origContext argType 
		(Thunk afterFnCode afterFnContext, (Expr rep body)) =
			head $ toExprList $ Thunk code lambdaContext
		(finalContext, bodyWithLets) = popArg (getArgDepth $ head newArgs) afterFnContext body
		lambda = addLambda newArgs bodyWithLets

getValue :: Thunk -> [[(Thunk,Expr)]] -> (Thunk, Expr)
getValue (Thunk code contextTs) offsetExprs = fromMaybe fail $ msum $ map tryOp ops where
	fail = parseError "no matching op" $ Thunk code contextTs
	tryOp (lit, nib, op) = match code (lit, nib) >>= \afterOpCode -> let
		afterOpThunk = (Thunk afterOpCode contextTs)
		valList = toExprList $ Thunk (foldr (\_ c->nextOffset c) code [1..(cp afterOpCode - cp code)]) contextTs
-- 		valList = head (drop (cp afterOpCode - cp code - 1) offsetExprs)
		opRep = Rep nib lit
		in convertOp opRep afterOpThunk valList op

convertOp :: Rep -> Thunk -> [(Thunk, Expr)] -> Operation -> Maybe (Thunk, Expr)
convertOp opRep afterOpThunk valList (Op atso impl autos) = 
	if all isJust typeMatch then Just makeExpr else Nothing where
		valTypes = map (retT.snd) valList
		ats = simplifyArgSpecs atso
		typeMatch = zipWith id ats (tail $ inits valTypes)
		isFns = map fromJust typeMatch
		(nextCode, argList) = convertLambdas afterOpThunk $ zip isFns valList
		makeExpr = (nextCode, foldl applyExpr initExpr (convertAutos argList autos))
		initExpr = Expr opRep $ Impl rt [HsAtom $ "("++hs++")"] noArgsUsed
		(rt, hs) = impl $ map retT argList

convertOp opRep afterOpThunk _ (Atom impl) = Just $ impl opRep afterOpThunk
convertOp opRep afterOpThunk _ Let = Just $ lets opRep afterOpThunk

lets :: Rep -> Thunk -> (Thunk, Expr)
lets opRep origThunk =
	(Thunk nextCode afterLetContext, Expr (addRep opRep r) (getArgImpl letArg)) where
		(Thunk nextCode nextContext, Expr r letDefImpl) = head $ toExprList origThunk
		(afterLetContext, letArg) = newLetArg nextContext letDefImpl

