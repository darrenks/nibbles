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
	exprs = takeOneMore codeAfter $ getValuesMemo False initialThunk
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

makePair :: Expr -> Expr -> Expr
makePair (Expr r1 (Impl t1 hs1 d1)) (Expr r2 (Impl t2 hs2 d2)) =
	Expr (addRep r1 r2) (Impl (VPair t1 t2) (makePairHs hs1 hs2) (max d1 d2))
makePairHs hs1 hs2 = (HsApp (app1 "(,)" hs1) hs2)

makePairs :: [(Thunk, Expr)] -> (Thunk, Expr)
makePairs [one] = one
makePairs ((_, Expr firstRep (Impl fstT fstHs fstDep)):rest) =
		(restThunk, Expr (addRep firstRep restRep) (Impl (VPair fstT restT) newHs (min fstDep restDep)))
	where
		(restThunk, Expr restRep (Impl restT restHs restDep)) = makePairs rest
		newHs = makePairHs fstHs restHs
	
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
convertLambda (Thunk code origContext, argTypes) (ArgFn (Fn numRets argTypeFn), _) = 
	((finalThunk, argTypes ++ [fnRetType]), Expr rep (addLambda newArg body)) where
		argType = argTypeFn argTypes
		(newArg, finalThunk, Expr rep body) = pushLambdaArg origContext argType bodyFn
		fnRetType = getImplType body
		-- 0 is special case for letrec, this is a hacky way to replace the 3rd arg type with its real Fn type which can only be known after 2nd arg type is determined.
		-- It would very tricky to allow the the 3rd argument to do things like auto pair
		-- without this (and do things like only add the recursive function to args for it.
		bodyFn = if numRets == 0
			then (\lambdaContext newArg -> let -- todo better dependent types
				[(c1,a)] = take 1 $ getValuesMemo False $ Thunk code lambdaContext
				[(c2,b)] = take 1 $ getValuesMemo True $ c1
				(Arg (Impl (VPair t VRec) hs d) dep LambdaArg) = newArg
				toType = (getExprType b)
				recType = VFn (flattenPair t) toType
				recArg = Arg (Impl (VPair t recType) hs d) dep LambdaArg
				recContext = replaceArg newArg recArg lambdaContext
				(Thunk recCode _) = c2
				in [(c1,a),(c2,b)] ++ [getNArgsExpr (flattenPair toType) $ Thunk recCode recContext])
			else (\lambdaContext _ ->
				take numRets $ getValuesMemo True $ Thunk code lambdaContext)

replaceArg old new = map (\arg-> if arg == old then new else arg)

pushLambdaArg origContext argType f =
	(newArg, Thunk afterFnCode finalContext, Expr rep bodyWithLets) where
		(lambdaContext, newArg) = newLambdaArg origContext argType
		(Thunk afterFnCode afterFnContext, Expr rep body) = makePairs $ f lambdaContext newArg
		(finalContext, bodyWithLets) = popArg (getArgDepth newArg) afterFnContext body


-- Gets the arg list
getValuesMemo :: Bool -> Thunk -> [(Thunk, Expr)]
getValuesMemo = (head.) . exprsByOffset

-- Gets the arg list (given an arg from each possible offset after this one)
getValues :: Bool -> Thunk -> [[(Thunk, Expr)]] -> [(Thunk, Expr)]
getValues firstInFn thunk offsetExprs = (afterThunk, expr) : getValues False afterThunk offsetAfterExprs where
	(afterThunk,expr) = getValue firstInFn thunk offsetExprs
	Thunk code context = thunk
	Thunk after afterContext = afterThunk
	offsetAfterExprs =
		if afterContext == context
		then drop (cp after-cp code) offsetExprs
		else drop 1 $ exprsByOffset False afterThunk

-- Gets the arg lists from each possible offset
exprsByOffset :: Bool -> Thunk -> [[(Thunk, Expr)]]
exprsByOffset firstInFn (Thunk code context) =
	getValues firstInFn (Thunk code context) rest : rest where
		rest = exprsByOffset False (Thunk (nextOffset code) context)

getValue :: Bool -> Thunk -> [[(Thunk, Expr)]] -> (Thunk, Expr)
getValue firstInFn (Thunk code context) offsetExprs = fromMaybe fail $ msum $ map tryOp sortedOps where
	sortedOps = ops -- sortOn (\(_,b,_)-> -length b) ops
	fail = parseError "no matching op" $ Thunk code context
	tryOp (onlyAtBegin, lit, nib, op) = (if firstInFn || not onlyAtBegin then match code (lit, nib) else Nothing) >>= \afterOpCode -> let
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
		initExpr = Expr opRep $ Impl rt (HsAtom $ "("++hs++")") noArgsUsed
		fullExpr = (nextThunk, foldl applyExpr initExpr (convertAutos argList autos))
		finalExpr = convertMultRet $ convertPairToLet fullExpr

convertOp opRep afterOpThunk _ (Atom impl) = Just $ applyFirstClassFn $ impl opRep afterOpThunk

-- todo memoize the parse
-- todo could put in getValue if wanted to support real first class functions
-- todo could unify function calling with convertOp code
applyFirstClassFn :: (Thunk, Expr) -> (Thunk, Expr)
applyFirstClassFn (thunk, Expr rep (Impl (VFn from to) hs dep)) =
	convertMultRet $ convertPairToLet (nextThunk, applyExpr initExpr argsExpr) where
		initExpr = Expr rep $ Impl to hs dep
		(nextThunk, argsExpr) = getNArgsExpr from thunk
applyFirstClassFn x = x

getNArgsExpr argTypes thunk = (nextThunk, argsExpr) where
	args = take (length argTypes) $ getValuesMemo False $ thunk
	argValues = map snd args
	argValuesCoerced = zipWith coerceExpr argValues argTypes
	argsExpr = foldl1 makePair argValuesCoerced
	nextThunk = fst $ last args

coerceExpr (Expr rep (Impl et hs dep)) t = Expr rep (Impl t (app1 (coerceTo(t,et)) hs) dep)

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

convertMultRet (thunk, Expr rep (Impl (VMultRet t) hs dep)) =
	(thunk, Expr rep (Impl t hs dep))
convertMultRet a = a
