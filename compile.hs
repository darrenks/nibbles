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
		(Thunk rest afterContext, Expr rep body) = getValueMemo $ Thunk (consumeWhitespace input)	[]	
		(_, progImpl) = popArg 0 afterContext body
		prog = Expr rep progImpl


applyExpr :: Expr -> Expr -> Expr
applyExpr (Expr r1 (Impl t1 hs1 d1)) (Expr r2 (Impl _ hs2 d2)) =
	Expr (addRep r1 r2) (Impl t1 (HsApp hs1 hs2) (max d1 d2))

convertAutoType VAuto = VInt
convertAutoType t = t

convertAuto (Expr r (Impl VAuto _ _)) auto =
	Expr r $ Impl VInt (i $ fromIntegral auto) noArgsUsed
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
convertLambda (Thunk code origContext) (ArgMatches, result) = result
convertLambda (Thunk code origContext) (ArgFnOf argType, _) = 
	(Thunk afterFnCode finalContext, Expr rep lambda) where
		(lambdaContext, newArg) = newLambdaArg origContext argType 
		(Thunk afterFnCode afterFnContext, (Expr rep body)) =
			fillArg $ Thunk code lambdaContext
		(finalContext, bodyWithLets) = popArg (getArgDepth newArg) afterFnContext body
		lambda = addLambda newArg bodyWithLets
		
fillArg thunk = getValueMemo thunk

-- Gets the arg list
getValuesMemo :: Thunk -> [(Thunk, Expr)]
getValuesMemo = head . exprsByOffset
getValueMemo = head . getValuesMemo

-- only memoized by offset, not context
data MemoData = MemoData [Arg] [[(Thunk, Expr)]]

-- Gets the arg list (given an arg from each possible offset after this one)
getValues :: Thunk -> MemoData -> [(Thunk, Expr)]
getValues thunk offsetExprs = (afterThunk, expr) : getValues afterThunk offsetAfterExprs where
	(afterThunk,expr) = getValue thunk offsetExprs
	Thunk code context = thunk
	Thunk after _ = afterThunk
	MemoData _ offsetExprsz = offsetExprs
	offsetAfterExprs = MemoData context $ drop (cp after-cp code) offsetExprsz
-- 
-- Gets the arg lists from each possible offset
exprsByOffset :: Thunk -> [[(Thunk, Expr)]]
exprsByOffset (Thunk code context) =
	getValues (Thunk code context) (MemoData context rest) : rest where
		rest = exprsByOffset (Thunk (nextOffset code) context)

-- check the arg list for any changes to context, and regenerate the rest if so
checkMemos :: [Arg] -> Thunk -> [(Thunk, Expr)] -> [(Thunk, Expr)]
checkMemos memoContext thunk (first : rest)
	| memoContext == context = first : checkMemos memoContext afterThunk rest
	| otherwise = checkMemos context thunk (getValuesMemo thunk)
	where
		(Thunk _ context) = thunk
		(afterThunk, expr) = first

sortedOps = ops -- sortOn (\(_,b,_)-> -length b) ops

getValue :: Thunk -> MemoData -> (Thunk, Expr)
getValue (Thunk code context) memo = fromMaybe fail $ msum $ map tryOp sortedOps where
	fail = parseError "no matching op" $ Thunk code context
	tryOp (lit, nib, op) = match code (lit, nib) >>= \afterOpCode -> let
		afterOpThunk = (Thunk afterOpCode context)
-- 		valList = toExprList $ Thunk (foldr (\_ c->nextOffset c) code [1..(cp afterOpCode - cp code)]) context
		(MemoData memoContext offsetExprs) = memo
		valList = checkMemos memoContext afterOpThunk $ head (drop (cp afterOpCode - cp code - 1) offsetExprs)
		opRep = Rep nib lit
		in convertOp opRep afterOpThunk valList op

convertOp :: Rep -> Thunk -> [(Thunk, Expr)] -> Operation -> Maybe (Thunk, Expr)
convertOp opRep afterOpThunk valList (Op ats impl autos) = 
	if all isJust typeMatch then Just finalExpr else Nothing where
		valTypes = map (retT.snd) valList
		typeMatch = zipWith id (simplifyArgSpecs ats) (tail $ inits valTypes)
		isFns = map fromJust typeMatch
		(nextThunk, argList) = convertLambdas afterOpThunk $ zip isFns valList
		(rt, hs) = impl $ map retT argList
		initExpr = Expr opRep $ Impl rt (HsAtom $ "("++hs++")") noArgsUsed
		fullExpr = (nextThunk, foldl applyExpr initExpr (convertAutos argList autos))
		finalExpr = convertPairToLet fullExpr

convertOp opRep afterOpThunk _ (Atom impl) = Just $ impl opRep afterOpThunk

convertPairToLet :: (Thunk, Expr) -> (Thunk, Expr)
convertPairToLet (Thunk code context, Expr rep impl)
	| isPair (getImplType impl) = (Thunk code letContext, Expr rep firstImpl) where
		(letContext, letArg) = newLetArg context impl Hidden
		firstImpl = getFirstOf letArg
		
		isPair (VPair _ _) = True
		isPair _ = False
		getFirstOf (Arg (Impl (VPair a b) hs dep) _ _) = getFirstOf $ Arg (Impl a (app1"fst"hs) dep) undefined undefined
		getFirstOf (Arg impl _ _) = impl
		
convertPairToLet r = r