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
simplifyArgSpec (Fn numRets f) vts = Just $ ArgFnOf numRets $ f $ init vts
simplifyArgSpec (Cond _ f) vts = maybeMatch $ f vts -- last

maybeMatch b = if b then Just ArgMatches else Nothing

convertLambdas :: Thunk -> [(ArgMatchResult, (Thunk, Expr))] -> (Thunk, [Expr])
convertLambdas = mapAccumL convertLambda

-- todo mark rec snd pair as used since, it's already served a purpose

-- todo this is a better spot to checkMemoData because fns change parse after, so right now anything after a fn is broken since this still uses memoized list
-- but why is fn after a fn broken? memoized parse
convertLambda :: Thunk -> (ArgMatchResult, (Thunk, Expr)) -> (Thunk, Expr)
convertLambda (Thunk code origContext) (ArgMatches, result) = result
convertLambda (Thunk code origContext) (ArgFnOf numRets argType, _) = 
	(finalThunk, Expr rep (addLambda newArg body)) where
		(newArg, finalThunk, Expr rep body) = pushLambdaArg origContext argType bodyFn
		-- 0 is special case for letrec, this is a hacky way to replace the 3rd arg type with its real Fn type which can only be known after 2nd arg type is determined
		bodyFn = if numRets == 0
			then (\lambdaContext newArg -> let -- todo better dependent types
				[(c1,a),(c2,b)] = take 2 $ getValuesMemo $ Thunk code lambdaContext
				(Arg (Impl (VPair t VRec) hs d) dep LambdaArg) = newArg
				recType = VFn (flattenArg t) (getExprType b)
				recArg = Arg (Impl (VPair t recType) hs d) dep LambdaArg
				recContext = replaceArg newArg recArg lambdaContext
				(Thunk recCode _) = c2
				in [(c1,a),(c2,b)] ++ [head $ getValuesMemo $ Thunk recCode recContext])
			else (\lambdaContext _ ->
				take numRets $ getValuesMemo $ Thunk code lambdaContext)

flattenArg VTuple0 = []
flattenArg (VPair a b) = flattenArg a ++ flattenArg b
flattenArg a = [a]
replaceArg old new = map (\arg-> if arg == old then new else arg)

pushLambdaArg origContext argType f =
	(newArg, Thunk afterFnCode finalContext, Expr rep bodyWithLets) where
		(lambdaContext, newArg) = newLambdaArg origContext argType
		(Thunk afterFnCode afterFnContext, Expr rep body) = makePairs $ f lambdaContext newArg
		(finalContext, bodyWithLets) = popArg (getArgDepth newArg) afterFnContext body

makePairs :: [(Thunk, Expr)] -> (Thunk, Expr)
makePairs [one] = one
makePairs ((_, Expr firstRep (Impl fstT fstHs fstDep)):rest) =
		(restThunk, Expr (addRep firstRep restRep) (Impl (VPair fstT restT) newHs (min fstDep restDep)))
	where
		(restThunk, Expr restRep (Impl restT restHs restDep)) = makePairs rest
		--newHs = (HsApp (app1 "(curry id)" fstHs) restHs)
		newHs = HsAtom $ "(" ++ flatHs fstHs ++ "," ++ flatHs restHs ++ ")"
	

-- Gets the arg list
getValuesMemo :: Thunk -> [(Thunk, Expr)]
getValuesMemo = (head ) . exprsByOffset

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
-- 
-- Gets the arg lists from each possible offset
exprsByOffset :: Thunk -> [[(Thunk, Expr)]]
exprsByOffset (Thunk code context) =
	getValues (Thunk code context) rest : rest where
		rest = exprsByOffset (Thunk (nextOffset code) context)

sortedOps = ops -- sortOn (\(_,b,_)-> -length b) ops

getValue :: Thunk -> [[(Thunk, Expr)]] -> (Thunk, Expr)
getValue (Thunk code context) offsetExprs = fromMaybe fail $ msum $ map tryOp sortedOps where
	fail = parseError "no matching op" $ Thunk code context
	tryOp (onlyAtBegin, lit, nib, op) = (if False || not onlyAtBegin then match code (lit, nib) else Nothing) >>= \afterOpCode -> let
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
		finalExpr = convertMultRetToPair $ convertPairToLet fullExpr

convertOp opRep afterOpThunk _ (Atom impl) = Just $ applyFirstClassFn $ impl opRep afterOpThunk

-- todo memoize the parse
-- todo coerce args
-- todo could put in getValue if wanted to support real first class functions
-- todo could unify function calling with convertOp code
-- convertMultRetToPair
applyFirstClassFn :: (Thunk, Expr) -> (Thunk, Expr)
applyFirstClassFn (thunk, Expr rep (Impl (VFn from to) hs dep)) =
	(nextThunk, foldl applyExpr initExpr argValues) where
		initExpr = Expr rep $ Impl to hs dep
		args = take (length from) $ getValuesMemo $ thunk
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