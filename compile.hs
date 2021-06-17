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
import Hs

compile :: (VT -> String) -> String -> Code -> Expr
compile finishFn seperator input = Expr rep progImpl where
	initialThunk = Thunk (consumeWhitespace input) []
	exprs = takeOneMore codeAfter $ getValuesMemo initialThunk
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
	Expr (addRep r1 r2) (Impl t1 (hsApp hs1 hs2) (max d1 d2))

makePairs :: [VT] -> [(Thunk, Expr)] -> (Thunk, Expr)
makePairs fromTypes args = (last thunks, foldl applyExpr initExpr exprs) where
	thunks = map fst args
	exprs = map snd args
	toTypes = map getExprType exprs
	initExpr = Expr (Rep [] "") (Impl (VFn fromTypes toTypes) (hsAtom initHs) 0)
	-- todo instead of fold apply, build the (expr1, expr2), etc, cleaner hs
	initHs = if length exprs == 1 then "" else "("++replicate (length $ tail exprs) ','++")"
	
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
simplifyArgSpec (Fn numRets f) vts = Just $ ArgFn (Fn numRets f)
simplifyArgSpec (Cond _ f) vts = maybeMatch $ f vts -- last

maybeMatch b = if b then Just ArgMatches else Nothing

convertLambdas :: Thunk -> [(ArgMatchResult, (Thunk, Expr))] -> (Thunk, [Expr])
convertLambdas thunk estimatedArgs = (finalThunk, args) where
	((finalThunk,vts),args) = mapAccumL convertLambda (thunk, []) estimatedArgs

-- todo mark rec snd pair as used since, it's already served a purpose
convertLambda :: (Thunk, [VT]) -> (ArgMatchResult, (Thunk, Expr)) -> ((Thunk, [VT]), Expr)
convertLambda (Thunk code origContext, argTypes) (ArgMatches, (memoThunk, memoExpr)) =
	((memoThunk, argTypes ++ [getExprType memoExpr]), memoExpr)
convertLambda (Thunk origCode origContext, argTypes) (ArgFn (Fn numRets argTypeFn), _) = 
	((finalThunk, argTypes ++ [fnRetType]), Expr (addRep bonusRep rep) (addLambda newArg body)) where
		(bonusRets, code) = parseCountTuple origCode
		bonusRep = Rep (replicate bonusRets 0) (replicate bonusRets '~') -- todo const
		argType = argTypeFn argTypes
		(newArg, finalThunk, Expr rep body) = pushLambdaArg origContext argType bodyFn
		fnRetType = getImplType body
		-- 0 is special case for letrec, this is a hacky way to replace the 3rd arg type with its real Fn type which can only be known after 2nd arg type is determined.
		-- It would very tricky to allow the the 3rd argument to do things like auto pair
		-- without this (and do things like only add the recursive function to args for it.
		bodyFn = if numRets == 0
			then (\lambdaContext newArg -> let -- todo better dependent types
				nonRecImpls = init $ getArgImpls newArg
				contextWithoutRec = [Arg nonRecImpls LambdaArg] ++ tail lambdaContext
				[(c1@(Thunk c1code ct),a)] = take 1 $ getValuesMemo $ Thunk code contextWithoutRec
				(bonusRets2, c1b) = parseCountTuple c1code
				bonusRep2 = Rep (replicate bonusRets2 0) (replicate bonusRets2 '~')
				(c2,bb@(Expr repb ib)) = makePairs undefined $ take (1+bonusRets2) $ getValuesMemo $ Thunk c1b ct
				b = Expr (addRep bonusRep2 repb) ib
				from = map getImplType nonRecImpls
				toType = ret $ getExprType b
				recType = VFn from toType
				recImpl = setType recType $ last $ getArgImpls newArg
				recArg = Arg (nonRecImpls ++ [recImpl]) LambdaArg
				recContext = [recArg] ++ tail lambdaContext
				in [(c1,a)]++[(c2,b)] ++ [makePairs undefined (getNArgExprs toType $ Thunk (getCode c2) recContext)])
			else (\lambdaContext _ ->
				take (bonusRets + numRets) $ getValuesMemo $ Thunk code lambdaContext)

pushLambdaArg origContext argType f =
	(newArg, Thunk afterFnCode finalContext, Expr rep bodyWithLets) where
		(lambdaContext, newArg) = newLambdaArg origContext argType
		(Thunk afterFnCode afterFnContext, Expr rep body) = makePairs argType $ f lambdaContext newArg
		(finalContext, bodyWithLets) = popArg (length lambdaContext) afterFnContext body

-- Gets the arg list
getValuesMemo :: Thunk -> [(Thunk, Expr)]
getValuesMemo = head . exprsByOffset

-- Gets the arg list (given an arg from each possible offset after this one)
getValues :: Thunk -> [[(Thunk, Expr)]] -> [(Thunk, Expr)]
getValues thunk offsetExprs = (afterThunk, expr) : getValues afterThunk offsetAfterExprs where
	(afterThunk,expr) = getValue thunk offsetExprs
	Thunk code context = thunk
	Thunk after afterContext = afterThunk
	offsetAfterExprs =
		if afterContext == context
		then drop (cp after-cp code) offsetExprs
		else drop 1 $ exprsByOffset afterThunk

-- Gets the arg lists from each possible offset
exprsByOffset :: Thunk -> [[(Thunk, Expr)]]
exprsByOffset (Thunk code context) =
	getValues (Thunk code context) rest : rest where
		rest = exprsByOffset (Thunk (nextOffset code) context)

getValue :: Thunk -> [[(Thunk, Expr)]] -> (Thunk, Expr)
getValue (Thunk code context) offsetExprs = fromMaybe fail $ msum $ map tryOp sortedOps where
	sortedOps = ops -- sortOn (\(_,b,_)-> -length b) ops
	fail = parseError "no matching op" $ Thunk code context
	tryOp (lit, nib, op) = match code (lit, nib) >>= \afterOpCode -> let
		afterOpThunk = (Thunk afterOpCode context)
-- 		valList = toExprList $ Thunk (foldr (\_ c->nextOffset c) code [1..(cp afterOpCode - cp code)]) context
		valList = head (drop (cp afterOpCode - cp code - 1) offsetExprs)
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
		initExpr = Expr opRep $ Impl undefined (hsParen $ hsAtom hs) noArgsUsed
		fullExpr = (nextThunk, foldl applyExpr initExpr (convertAutos argList autos))
		finalExpr = convertPairToLet fullExpr rt

convertOp opRep afterOpThunk _ (Atom impl) = Just $ applyFirstClassFn $ impl opRep afterOpThunk

-- todo memoize the parse
-- todo could put in getValue if wanted to support real first class functions
-- todo could unify function calling with convertOp code
applyFirstClassFn :: (Thunk, Expr) -> (Thunk, Expr)
applyFirstClassFn (thunk, Expr rep (Impl (VFn from to) hs dep)) =
	convertPairToLet (nextThunk, foldl applyExpr initExpr argExprs) to where
		initExpr = Expr rep $ Impl undefined hs dep
		exprs = getNArgExprs from thunk
		nextThunk = fst $ last exprs
		argExprs = map snd exprs
applyFirstClassFn x = x

getNArgExprs argTypes thunk = zip (map fst args) argValuesCoerced where
	args = take (length argTypes) $ getValuesMemo $ thunk
	argValues = map snd args
	argValuesCoerced = zipWith coerceExpr argValues argTypes

coerceExpr (Expr rep (Impl et hs dep)) t = Expr rep (Impl t (hsApp (hsAtom$coerceTo(t,et)) hs) dep)

convertPairToLet :: (Thunk, Expr) -> [VT] -> (Thunk, Expr)
convertPairToLet (thunk, Expr rep (Impl _ hs dep)) [t] = (thunk, Expr rep $ Impl t hs dep)
convertPairToLet (Thunk code context, Expr rep impl) implTypes =
	(Thunk code $ letArg:context, Expr rep firstImpl) where
		letArg = newLetArg context impl implTypes
		firstImpl = head $ getArgImpls letArg	
