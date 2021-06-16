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

compile :: (VT -> String) -> String -> Code -> SExpr
compile finishFn seperator input = SExpr rep progImpl where
	initialThunk = Thunk (consumeWhitespace input) []
	exprs = takeOneMore codeAfter $ getValuesMemo initialThunk
	Thunk _ afterContext = fst $ last exprs
	--todo could be empty program, no exprs
	SExpr rep body = foldl1 combineExprs $ map (applyFinish.snd) exprs
	(_, progImpl) = popArg 0 afterContext body
	
	combineExprs a v = sApplyExpr (modifyImpl (app1Hs $"(\\a b-> a++"++(show$sToA seperator)++"++b)") a) v
	applyFinish (SExpr rep impl) =
		SExpr rep $ app1Hs (finishFn $ getImplType2 impl) impl
	codeAfter (Thunk code _, _) = not $ empty code
	takeOneMore f l = a++[b] where (a,(b:c)) = span f l

applyExpr2 :: Expr -> SExpr -> Expr
applyExpr2 (Expr r1 (Impl t1 hs1 d1)) (SExpr r2 (SImpl _ hs2 d2)) =
	Expr (addRep r1 r2) (Impl t1 (HsApp hs1 hs2) (max d1 d2))

sApplyExpr :: SExpr -> SExpr -> SExpr
sApplyExpr (SExpr r1 (SImpl t1 hs1 d1)) (SExpr r2 (SImpl _ hs2 d2)) =
	SExpr (addRep r1 r2) (SImpl t1 (HsApp hs1 hs2) (max d1 d2))

-- 
-- makePair :: Expr -> Expr -> Expr
-- makePair = undefined
-- -- makePair (Expr r1 (Impl t1 hs1 d1)) (Expr r2 (Impl t2 hs2 d2)) =
-- -- 	Expr (addRep r1 r2) (Impl (VPair t1 t2) (makePairHs hs1 hs2) (max d1 d2))
-- makePairHs hs1 hs2 = (HsApp (app1 "(,)" hs1) hs2)

makePairs :: [VT] -> [(Thunk, SExpr)] -> (Thunk, SExpr)
-- makePairs _ [(thunk, expr)] = (thunk, expr)
makePairs fromTypes args = (last thunks, foldl sApplyExpr initExpr exprs) where
	thunks = map fst args
	exprs = map snd args
	toTypes = map getExprType exprs
	--todo more ,,
	initExpr = SExpr (Rep [] "") (SImpl (VFn fromTypes toTypes) (HsAtom initHs) 0 {-todo-})
	-- todo instead of fold apply, build the (expr1, expr2), etc, cleaner hs
	initHs = if length exprs == 1 then "" else "("++replicate (length $ tail exprs) ','++")"
	
	
-- makePairs ((_, Expr firstRep (Impl fstT fstHs fstDep)):rest) =
-- 		(restThunk, Expr (addRep firstRep restRep) (Impl (VPair fstT restT) newHs (min fstDep restDep)))
-- 	where
-- 		(restThunk, Expr restRep (Impl restT restHs restDep)) = makePairs rest
-- 		newHs = makePairHs fstHs restHs
	
convertAutoType VAuto = VInt
convertAutoType t = t

-- todo make it use sexpr
convertAuto (SExpr r (SImpl VAuto _ _)) auto =
	SExpr r $ SImpl VInt (i $ fromIntegral auto) noArgsUsed
convertAuto e _ = e

convertAutos :: [SExpr] -> [Int] -> [SExpr]
convertAutos l autos = zipWith (\(e) a -> (convertAuto e a)) l (autos ++ repeat undefined)

simplifyArgSpecs :: [ArgSpec] -> [[VT] -> Maybe ArgMatchResult]
simplifyArgSpecs = map simplifyArgSpec
simplifyArgSpec (Exact VAuto) vts = maybeMatch $ VAuto == last vts
simplifyArgSpec (Exact spec) vts = maybeMatch $ spec == convertAutoType (last vts)
simplifyArgSpec (Fn numRets f) vts = Just $ ArgFn (Fn numRets f)
simplifyArgSpec (Cond _ f) vts = maybeMatch $ f vts -- last

maybeMatch b = if b then Just ArgMatches else Nothing

convertLambdas :: Thunk -> [(ArgMatchResult, (Thunk, SExpr))] -> (Thunk, [SExpr])
convertLambdas thunk estimatedArgs = (finalThunk, args) where
	((finalThunk,vts),args) = mapAccumL convertLambda (thunk, []) estimatedArgs

-- todo mark rec snd pair as used since, it's already served a purpose

convertLambda :: (Thunk, [VT]) -> (ArgMatchResult, (Thunk, SExpr)) -> ((Thunk, [VT]), SExpr)
convertLambda (Thunk code origContext, argTypes) (ArgMatches, (memoThunk, memoExpr)) =
	((memoThunk, argTypes ++ [getExprType memoExpr]), memoExpr)
convertLambda (Thunk origCode origContext, argTypes) (ArgFn (Fn numRets argTypeFn), _) = 
	((finalThunk, argTypes ++ [fnRetType]), SExpr (addRep bonusRep rep) (addLambda newArg body)) where
		(bonusRets, code) = parseCountTuple origCode
		bonusRep = Rep (replicate bonusRets 0) (replicate bonusRets '~') -- todo const
		argType = argTypeFn argTypes
		(newArg, finalThunk, SExpr rep body) = pushLambdaArg origContext argType bodyFn
		fnRetType = getImplType2 body
		-- 0 is special case for letrec, this is a hacky way to replace the 3rd arg type with its real Fn type which can only be known after 2nd arg type is determined.
		-- It would very tricky to allow the the 3rd argument to do things like auto pair
		-- without this (and do things like only add the recursive function to args for it.
		bodyFn = if numRets == 0
			then (\lambdaContext newArg -> let -- todo better dependent types
				nonRecImpls = init $ getArgImpls newArg
				contextWithoutRec = [Arg nonRecImpls LambdaArg] ++ tail lambdaContext
				[(c1,a)] = take 1 $ getValuesMemo $ Thunk code contextWithoutRec
				Thunk c1code ct = c1
				(bonusRets2, c1b) = parseCountTuple c1code
				bonusRep2 = Rep (replicate bonusRets2 0) (replicate bonusRets2 '~')
				(c2,bb) = makePairs undefined $ take (1+bonusRets2) $ getValuesMemo $ Thunk c1b ct
				(SExpr repb ib) = bb
				b = SExpr (addRep bonusRep2 repb) ib
-- 				c2 = fst $ last exprs
-- 				bs = map snd exprs
				from = map getImplType2 nonRecImpls
				ret (VFn from to) = to
				toType = ret $ getExprType b
				recType = VFn from toType
				recImpl = setType recType $ last $ getArgImpls newArg
				recArg = Arg (nonRecImpls ++ [recImpl]) LambdaArg
				recContext = [recArg] ++ tail lambdaContext
				(Thunk recCode _) = c2
				in [(c1,a)]++[(c2,b)] ++ [makePairs undefined (getNArgExprs toType $ Thunk recCode recContext)])
			else (\lambdaContext _ ->
				take (bonusRets + numRets) $ getValuesMemo $ Thunk code lambdaContext)


-- replaceArg oldDep new = map (\arg->
-- 	let (Arg _ dep _) = arg in 
-- 	if dep == oldDep then new else arg)

pushLambdaArg origContext argType f =
	(newArg, Thunk afterFnCode finalContext, SExpr rep bodyWithLets) where
		(lambdaContext, newArg) = newLambdaArg origContext argType
		(Thunk afterFnCode afterFnContext, SExpr rep body) = makePairs argType $ f lambdaContext newArg
		(finalContext, bodyWithLets) = popArg (length lambdaContext) afterFnContext body

-- Gets the arg list
getValuesMemo :: Thunk -> [(Thunk, SExpr)]
getValuesMemo = head . exprsByOffset

-- Gets the arg list (given an arg from each possible offset after this one)
getValues :: Thunk -> [[(Thunk, SExpr)]] -> [(Thunk, SExpr)]
getValues thunk offsetExprs = (afterThunk, expr) : getValues afterThunk offsetAfterExprs where
	(afterThunk,expr) = getValue thunk offsetExprs
	Thunk code context = thunk
	Thunk after afterContext = afterThunk
	offsetAfterExprs =
		if afterContext == context
		then drop (cp after-cp code) offsetExprs
		else drop 1 $ exprsByOffset afterThunk

-- Gets the arg lists from each possible offset
exprsByOffset :: Thunk -> [[(Thunk, SExpr)]]
exprsByOffset (Thunk code context) =
	getValues (Thunk code context) rest : rest where
		rest = exprsByOffset (Thunk (nextOffset code) context)

getValue :: Thunk -> [[(Thunk, SExpr)]] -> (Thunk, SExpr)
getValue (Thunk code context) offsetExprs = fromMaybe fail $ msum $ map tryOp sortedOps where
	sortedOps = ops -- sortOn (\(_,b,_)-> -length b) ops
	fail = parseError "no matching op" $ Thunk code context
	tryOp (lit, nib, op) = match code (lit, nib) >>= \afterOpCode -> let
		afterOpThunk = (Thunk afterOpCode context)
-- 		valList = toExprList $ Thunk (foldr (\_ c->nextOffset c) code [1..(cp afterOpCode - cp code)]) context
		valList = head (drop (cp afterOpCode - cp code - 1) offsetExprs)
		opRep = Rep nib lit
		in convertOp opRep afterOpThunk valList op

convertOp :: Rep -> Thunk -> [(Thunk, SExpr)] -> Operation -> Maybe (Thunk, SExpr)
convertOp opRep afterOpThunk valList (Op ats impl autos) = 
	if all isJust typeMatch then Just finalExpr else Nothing where
		valTypes = map (retT2.snd) valList
		typeMatch = zipWith id (simplifyArgSpecs ats) (tail $ inits valTypes)
		isFns = map fromJust typeMatch
		(nextThunk, argList) = convertLambdas afterOpThunk $ zip isFns valList
		(rt, hs) = impl $ map retT2 argList
		initExpr = Expr opRep $ Impl rt (HsAtom $ "("++hs++")") noArgsUsed
		fullExpr = (nextThunk, foldl applyExpr2 initExpr (convertAutos argList autos))
		finalExpr = convertPairToLet fullExpr

convertOp opRep afterOpThunk _ (Atom impl) = Just $ applyFirstClassFn $ impl opRep afterOpThunk

-- todo memoize the parse
-- todo could put in getValue if wanted to support real first class functions
-- todo could unify function calling with convertOp code
applyFirstClassFn :: (Thunk, SExpr) -> (Thunk, SExpr)
applyFirstClassFn (thunk, SExpr rep (SImpl (VFn from to) hs dep)) =
	convertPairToLet (nextThunk, foldl applyExpr2 initExpr argExprs) where
		initExpr = Expr rep $ Impl to hs dep
		exprs = getNArgExprs from thunk
		nextThunk = fst $ last exprs
		argExprs = map snd exprs
applyFirstClassFn x = x

getNArgExprs argTypes thunk = zip (map fst args) argValuesCoerced where
	args = take (length argTypes) $ getValuesMemo $ thunk
	argValues = map snd args
	argValuesCoerced = zipWith coerceExpr argValues argTypes

coerceExpr (SExpr rep (SImpl et hs dep)) t = SExpr rep (SImpl t (app1 (coerceTo(t,et)) hs) dep) -- todo assumes 1 t and et

convertPairToLet :: (Thunk, Expr) -> (Thunk, SExpr)
convertPairToLet (thunk, Expr rep (Impl [t] hs dep)) = (thunk, SExpr rep $ SImpl t hs dep)
convertPairToLet (Thunk code context, Expr rep impl) =
	(Thunk code $ letArg:context, SExpr rep firstImpl) where
		letArg = newLetArg context impl
		firstImpl = head $ getArgImpls letArg	
