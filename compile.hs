module Compile(compile) where

import Data.List(inits,intercalate)
import Control.Monad (msum)
import Data.Maybe
import State

import Polylib(coerceTo)
import Ops
import Types
import Expr
import Args
import Parse
import Hs

compile :: (VT -> String) -> String -> Code -> (Impl, [Int], String)
compile finishFn separator input = evalState doCompile $ blankRep (consumeWhitespace input) args where
	args =
		[ Arg (Impl undefined (hsAtom"_") 0 Nothing UsedArg:letArgs)
			(LetArg $ hsAtom $ "(undefined," ++ intercalate "," letDefs ++ ")")]
	mainLets =
		[ ("firstInt", VInt, "fromMaybe 100 $ at intList 0")
		, ("firstLine", vstr, "fromMaybe [] $ at strLines 0")
		, ("ints", VList [VInt], "intList")
		, ("secondInt", VInt, "fromMaybe 1000 $ at intList 1")
		, ("secondLine", vstr, "fromMaybe [] $ at strLines 1")
		, ("allInput", vstr, "input")
		]
	letArgs = map (\(name, vt, _) -> noArgsUsed {
		implType=vt, implCode=hsAtom name, implName=Just name, implUsed=OptionalArg
		}) mainLets
	letDefs = map (\(_, _, hsDef) -> hsDef) mainLets
	doCompile = do
		impl1 <- get1Value
		body <- if separator == "" then mainCombiner impl1
			else testCombiner $ finishIt impl1
		context <- gets pdContext
		impl <- popArg 0 body
		nib <- gets getNib
		lit <- gets getLit

		let [fstIntUsed,fstLineUsed,intsUsed,sndIntUsed,sndLineUsed,allInputUsed] =
			tail $ map ((==UsedArg).implUsed) $ argImpls $ last context

		let autoMap = if allInputUsed then ""
			else if sndLineUsed then "intercalate [10] $ flip map (listOr [[]] (reshape 2 strLines)) $ \\strLines -> "
			else if fstLineUsed then "intercalate [10] $ flip map (listOr [[]] (reshape 1 strLines)) $ \\strLines -> "
			else if intsUsed then "let intMatrix2 = if length intMatrix > 1 && (any ((>1).length) intMatrix) then intMatrix else [intList] in intercalate [10] $ flip map intMatrix2 $ \\intList ->"
			else if sndIntUsed then "intercalate [10] $ flip map (listOr [[]] (reshape 2 intList)) $ \\intList -> "
			else if fstIntUsed then "intercalate [10] $ flip map (listOr [[]] (reshape 1 intList)) $ \\intList -> "
			else ""
		let finalImpl = app1Hs ("let intMatrix=filter (not.null) (map (asInts.sToA) (lines $ aToS input));\
			\strLines=map sToA $ lines $ aToS input;\
			\intList=concat intMatrix;\
			\in "++autoMap) impl
		return (finalImpl, nib, lit)
	finishIt impl = (app1Hs (finishFn $ implType impl) impl) { implType = vstr }
	join2 impl1 impl2 = applyImpl (app1Hs ("(\\a b->a++sToA"++show separator++"++b)") impl1) impl2

	mainCombiner prev = do
		let finishedPrev = finishIt prev
		ParseData code _ _ _ <- get
		if empty code then return finishedPrev
		else do
			result <- case implType prev of
				VInt -> do
					(impl1,argsUsed) <- getLambdaValue 1 [VInt,VInt]
					if last argsUsed then do
						return $ (applyImpl (app1Hs "(\\a f -> foldr1 f [1..a])" prev) impl1) { implType = todoAssumeFst $ ret $ implType impl1 }
					else if argsUsed == [True,False] then do
						return $ (applyImpl (app1Hs "(\\a f -> map (flip f ()) [1..a])" prev) impl1) { implType = VList $ ret $ implType impl1 }
					else do
						return $ join2 finishedPrev (finishIt $ (app1Hs "(\\f->f()())" impl1) { implType = todoAssumeFst $ ret $ implType $ impl1 } )
				VList e | e /= [VChr] -> do
					(impl1,argsUsed) <- getLambdaValue (length e) (e++e)
					if or $ drop (length e) argsUsed then do
						return $ (applyImpl (app1Hs "foldr1" impl1) prev) { implType = todoAssumeFst $ ret $ implType impl1 }
					else if or $ take (length e) argsUsed then do
						return $ (applyImpl (app1Hs "(\\a f -> map (flip f ()) a)" prev) impl1) { implType = VList $ ret $ implType impl1 }
					else if todoAssumeFst (ret (implType impl1)) == vstr then do
						return $ ((applyImpl (app1Hs "(\\f a->intercalate (f()()) a)" impl1) (app1Hs ("map"++finishFn (todoAssumeFst e)) prev))) { implType = vstr }
					else do
						return $ join2 finishedPrev (finishIt $ (app1Hs "(\\f->f()())" impl1) { implType = todoAssumeFst $ ret $ implType $ impl1 } )
				_ -> do
					impl1 <- get1Value
					return $ join2 finishedPrev (finishIt impl1)
			
			mainCombiner result
	
	testCombiner prev = do
		ParseData code _ _ _ <- get
		if empty code then return prev
		else do
			impl1 <- get1Value
			testCombiner (join2 prev (finishIt impl1))

applyImpl :: Impl -> Impl -> Impl
applyImpl (Impl t1 hs1 d1 _ _) (Impl _ hs2 d2 _ _) = Impl t1 (hsApp hs1 hs2) (max d1 d2) undefined undefined
	
makePairs :: [VT] -> [Impl] -> Impl
makePairs fromTypes args = foldl applyImpl initImpl args where
	toTypes = map implType args
	initImpl = noArgsUsed { implType=VFn fromTypes toTypes, implCode=hsAtom pairMakerHs }
	-- todo instead of fold apply, build the (expr1, expr2), etc, cleaner hs
	pairMakerHs = if length args == 1 then "" else "("++replicate (length $ tail args) ','++")"
	
convertAutoType VAuto = VInt
convertAutoType t = t

convertAuto (Impl VAuto _ _ _ _) auto = noArgsUsed { implType=VInt, implCode=i $ fromIntegral auto }
convertAuto impl _ = impl

convertAutos :: [Impl] -> [Integer] -> [Impl]
convertAutos l autos = zipWith (\e a -> (convertAuto e a)) l (autos ++ repeat undefined)

simplifyArgSpecs :: [ArgSpec] -> [[VT] -> Maybe ArgMatchResult]
simplifyArgSpecs = map simplifyArgSpec where
	simplifyArgSpec (Exact VAuto) vts = maybeMatch $ VAuto == last vts
	simplifyArgSpec (Exact spec) vts = maybeMatch $ spec == convertAutoType (last vts)
	simplifyArgSpec (Fn f) _ = Just $ ArgFn (Fn f)
	simplifyArgSpec (Cond _ f) vts = maybeMatch $ f vts -- last
	maybeMatch b = if b then Just ArgMatches else Nothing

-- add the current rep to the partialFinalState
putAddRep :: ParseData -> ParseState ()
putAddRep (ParseData code context nib lit) = do
	appendRepH (nib,lit)
	modify $ \s -> s { pdCode=code, pdContext=context }

convertLambdas :: [VT] -> [(ArgMatchResult, (Impl, ParseData))] -> ParseState [Impl]
convertLambdas _ [] = return []
convertLambdas soFar (estArg:rest) = do
	impl <- convertLambda soFar estArg
	restConverted <- convertLambdas (soFar ++ [implType impl]) rest
	return $ impl : restConverted

-- -- todo mark rec snd pair as used since, it's already served a purpose
convertLambda :: [VT] -> (ArgMatchResult, (Impl, ParseData)) -> ParseState Impl
convertLambda _ (ArgMatches, (memoImpl, memoState)) = do
	putAddRep memoState
	return memoImpl
convertLambda argTypes (ArgFn (Fn fnFn), _) = do
	let (numRets, argType) = fnFn argTypes
	(lambdaFn, _) <- getLambdaValue numRets argType
	return lambdaFn	

getLambdaValue numRets argType = do
	(newArg,body) <- pushLambdaArg argType $ \newArg -> do
		-- 0 is special case for letrec, this is a hacky way to replace the 3rd arg type
		-- with its real Fn type which can only be known after 2nd arg type is determined.
		-- It would very tricky to allow the the 3rd argument to do things like auto pair
		-- without this (and do things like only add the recursive function to args for it.
		if numRets == 0 then do
			let nonRecImpls = init $ argImpls newArg
			modify $ \s -> s { pdContext=Arg nonRecImpls LambdaArg : tail (pdContext s) }
			a <- getValuesMemo 1
			bonus <- parseCountTuple
			b <- getValuesMemo (1+bonus)
			let from = map implType nonRecImpls
			let toType = map implType b
			let recType = VFn from toType
			let recImpl = (last $ argImpls newArg) { implType=recType }
			let recArg = Arg (nonRecImpls ++ [recImpl]) LambdaArg
			modify $ \s -> s { pdContext=recArg : tail (pdContext s) }
			c <- getNArgs toType
			return $ a++[makePairs argType b]++[makePairs argType c]
		else do
			bonus <- parseCountTuple
			getValuesMemo (bonus + numRets)
	return $ (addLambda newArg body, map (\impl->UsedArg==implUsed impl) $ argImpls newArg)

pushLambdaArg :: [VT] -> (Arg -> ParseState [Impl]) -> ParseState (Arg, Impl)
pushLambdaArg argType f = do
	newArg <- newLambdaArg argType
	depth <- gets pdContext >>= return.length
	rets <- f newArg
	let body = makePairs argType rets
	finalContext <- gets pdContext
	let newArgFinal = head $ filter (\arg -> argKind arg == LambdaArg) finalContext
	bodyWithLets <- popArg depth body
	return (newArgFinal, bodyWithLets)

get1Value :: ParseState Impl
get1Value = do
	v <- getValuesMemo 1
	return $ head v

-- Gets the arg list
getValuesMemo :: Int -> ParseState [Impl]
getValuesMemo n = do
	ParseData code context _ _ <- get
	let exprs = take n $ head $ exprsByOffset $ Thunk code context
	_ <- mapM (putAddRep.snd) exprs
	return $ map fst exprs

data Thunk = Thunk Code [Arg]

-- Gets the arg list (given an arg from each possible offset after this one)
getValues :: Thunk -> [[(Impl, ParseData)]] -> [(Impl, ParseData)]
getValues (Thunk code context) offsetExprs = (impl, after) : getValues afterThunk offsetAfterExprs where
	(impl, after@(ParseData afterCode afterContext _ _))=
		runState (getValue offsetExprs) $ blankRep code context
	afterThunk = Thunk afterCode afterContext
	offsetAfterExprs =
		if length afterContext == length context
		then drop (cp afterCode-cp code) offsetExprs
		else drop 1 $ exprsByOffset afterThunk

-- Gets the arg lists from each possible offset
exprsByOffset :: Thunk -> [[(Impl, ParseData)]]
exprsByOffset (Thunk code context) =
	getValues (Thunk code context) rest : rest where
		rest = exprsByOffset (Thunk (nextOffset code) context)

getValue :: [[(Impl, ParseData)]] -> ParseState Impl
getValue offsetExprs = do
	code <- gets pdCode
	let tryOp (lit, nib, op) = match code (lit, nib) >>= \afterOpCode -> let
		valList = head (drop (cp afterOpCode - cp code - 1) offsetExprs)
		in convertOp valList op >>= \f -> Just $
			appendRep (nib,lit) >> (modify $ \s -> s { pdCode=afterOpCode }) >> f
	if empty code then
		argImplicit 
	else
		fromMaybe (parseError "Parse Error: no matching op") $ msum $ map tryOp allOps

convertOp :: [(Impl, ParseData)] -> Operation -> Maybe (ParseState Impl)
convertOp valList (Op ats impl autos) = 
	if all isJust typeMatch then Just $ do
		let isFns = map fromJust typeMatch
		argList <- convertLambdas [] $ zip isFns valList
		let (rt, hs) = impl $ map implType argList
		let initImpl = noArgsUsed { implCode=hsParen $ hsAtom hs }
		let fullImpl = foldl applyImpl initImpl (convertAutos argList autos)
		convertPairToLet fullImpl rt
	else Nothing where	
		valTypes = map (implType.fst) valList
		typeMatch = zipWith id (simplifyArgSpecs ats) (tail $ inits valTypes)

convertOp _ (Atom impl) = Just $ do
	impl >>= applyFirstClassFn

-- todo memoize the parse
-- todo could put in getValue if wanted to support real first class functions
-- todo could unify function calling with convertOp code
applyFirstClassFn :: Impl -> ParseState Impl
applyFirstClassFn (Impl (VFn from to) hs dep _ _) = getNArgs from >>= \impls -> do
	let initImpl = Impl undefined hs dep undefined undefined
	convertPairToLet (foldl applyImpl initImpl impls) to
applyFirstClassFn x = return x

getNArgs :: [VT] -> ParseState [Impl]
getNArgs argTypes = do
	args <- getValuesMemo (length argTypes)
	return $ zipWith coerceImpl args argTypes

coerceImpl :: Impl -> VT -> Impl
coerceImpl (Impl et hs dep _ _) t = Impl t (hsApp (hsAtom$coerceTo(t,et)) hs) dep undefined undefined

convertPairToLet :: Impl -> [VT] -> ParseState Impl
convertPairToLet (Impl _ hs dep _ _) [t] = return $ Impl t hs dep undefined undefined
convertPairToLet impl implTypes = do
	context <- gets pdContext
	let letArg = newLetArg context impl implTypes
	modify $ \s -> s { pdContext=letArg:context }
	return $ head $ argImpls letArg
		