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

compile :: (VT -> String) -> String -> Code -> Expr
compile finishFn seperator input = Expr rep progImpl where
	initialThunk = Thunk (consumeWhitespace input) []
	exprs = takeOneMore codeAfter $ getValuesMemo True initialThunk
	Thunk _ afterContext = fst $ last exprs
	--todo could be empty program, no exprs
	Expr rep body = foldl1 combineExprs $ map (applyFinish.snd) exprs
	(_, progImpl) = popArg 0 afterContext body
	
	combineExprs a v = applyExpr (modifyImpl (app1Hs $"(\\a b-> a++"++(show$sToA seperator)++"++b)") a) v
	applyFinish (Expr rep impl) =
		Expr rep $ app1Hs (finishFn $ getImplType impl) impl
	codeAfter (Thunk code _, _) = not $ empty code
	takeOneMore f l = a++[b] where (a,(b:c)) = span f l


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
simplifyArgSpec (Exact VAuto) vts = maybeMatch $ VAuto == last vts
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
		(Thunk afterFnCode afterFnContext, Expr rep body) =
			makePairs $ take (length argType) $ getValuesMemo True $ Thunk code lambdaContext
		(finalContext, bodyWithLets) = popArg (getArgDepth newArg) afterFnContext body
		lambda = addLambda newArg bodyWithLets

makePairs :: [(Thunk, Expr)] -> (Thunk, Expr)
makePairs [one] = one
makePairs ((_, Expr firstRep (Impl fstT fstHs fstDep)):rest) =
		(restThunk, Expr (addRep firstRep restRep) (Impl (VPair fstT fstT) (HsApp (app1 "(\\a b->(a,b))" fstHs) restHs) (min fstDep restDep)))
	where (restThunk, Expr restRep (Impl restT restHs restDep)) = makePairs rest

-- Gets the arg list
getValuesMemo :: Bool -> Thunk -> [(Thunk, Expr)]
getValuesMemo = (head .) . exprsByOffset

-- only memoized by offset, not context
data MemoData = MemoData (Bool,[Arg]) [[(Thunk, Expr)]]

-- Gets the arg list (given an arg from each possible offset after this one)
getValues :: Bool -> Thunk -> MemoData -> [(Thunk, Expr)]
getValues isFirstInFn thunk offsetExprs = (afterThunk, expr) : getValues isFirstInFn afterThunk offsetAfterExprs where
	(afterThunk,expr) = getValue isFirstInFn thunk offsetExprs
	Thunk code context = thunk
	Thunk after _ = afterThunk
	MemoData _ offsetExprsz = offsetExprs
	offsetAfterExprs = MemoData (isFirstInFn,context) $ drop (cp after-cp code) offsetExprsz
-- 
-- Gets the arg lists from each possible offset
exprsByOffset :: Bool -> Thunk -> [[(Thunk, Expr)]]
exprsByOffset isFirstInFn (Thunk code context) =
	getValues isFirstInFn (Thunk code context) (MemoData (isFirstInFn,context) rest) : rest where
		rest = exprsByOffset isFirstInFn (Thunk (nextOffset code) context)

-- check the arg list for any changes to context, and regenerate the rest if so
checkMemos :: Bool -> (Bool,[Arg]) -> Thunk -> [(Thunk, Expr)] -> [(Thunk, Expr)]
checkMemos isFirstInFn memoConditions thunk (first : rest)
	| memoConditions == (isFirstInFn,context) = first : checkMemos isFirstInFn memoConditions afterThunk rest
	| otherwise = checkMemos isFirstInFn (isFirstInFn,context) thunk (getValuesMemo False thunk)
	where
		(Thunk _ context) = thunk
		(afterThunk, expr) = first

sortedOps = ops -- sortOn (\(_,b,_)-> -length b) ops

getValue :: Bool -> Thunk -> MemoData -> (Thunk, Expr)
getValue isFirstInFn (Thunk code context) memo = fromMaybe fail $ msum $ map tryOp sortedOps where
	fail = parseError "no matching op" $ Thunk code context
	tryOp (onlyAtBegin, lit, nib, op) = (if isFirstInFn || not onlyAtBegin then match code (lit, nib) else Nothing) >>= \afterOpCode -> let
		afterOpThunk = (Thunk afterOpCode context)
-- 		valList = toExprList $ Thunk (foldr (\_ c->nextOffset c) code [1..(cp afterOpCode - cp code)]) context
		(MemoData memoContext offsetExprs) = memo
		valList = checkMemos onlyAtBegin memoContext afterOpThunk $ head (drop (cp afterOpCode - cp code - 1) offsetExprs)
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
		finalExpr = convertMultRetToPair $ convertPairToLet fullExpr

convertOp opRep afterOpThunk _ (Atom impl) = Just $ applyFirstClassFn $ impl opRep afterOpThunk

-- todo memoize
-- todo coerce args
-- todo could put in getValue if wanted to support real first class functions
-- todo could unify function calling with convertOp code
-- convertMultRetToPair
-- todo recursion
applyFirstClassFn :: (Thunk, Expr) -> (Thunk, Expr)
applyFirstClassFn (thunk, Expr rep (Impl (VFn from to) hs dep)) =
	(nextThunk, foldl applyExpr initExpr argValues) where
		initExpr = Expr rep $ Impl to hs dep
		args = take (length from) $ getValuesMemo False $ thunk
		argValues = map snd args
		nextThunk = fst $ last args

applyFirstClassFn x = x


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

convertMultRetToPair (thunk, Expr rep (Impl t hs dep)) =
	(thunk, Expr rep (Impl (convertMultRetToPairH t) hs dep))
convertMultRetToPairH (VMultRet a b) = VPair (convertMultRetToPairH a) (convertMultRetToPairH b)
convertMultRetToPairH a = a