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
compile = compileH
	[ Arg (Impl undefined (hsAtom"_") 0 Nothing True:letArgs)
		(LetArg $ hsAtom $ "(undefined," ++ intercalate "," letDefs ++ ")")] where
	mainLets =
		[ ("firstInt", VInt, "fromMaybe 0 (at (asInts input) 0)")
		, ("firstLine", vstr, "sToA $ fromMaybe [] $ at (lines $ aToS input) 0")
		, ("ints", VList [VInt], "asInts (if length (asInts firstLine) < 2 then input else firstLine)")
		, ("secondInt", VInt, "fromMaybe 0 (at (asInts input) 1)")
		, ("secondLine", vstr, "sToA $ fromMaybe [] $ at (lines $ aToS input) 1")
		, ("allInput", vstr, "input")
		]
	letArgs = map (\(name, vt, _) -> noArgsUsed {
		implType=vt, implCode=hsAtom name, implName=Just name, implUsed=True
		}) mainLets
	letDefs = map (\(_, _, hsDef) -> hsDef) mainLets

compileH args finishFn separator input = evalState doCompile $ blankRep (consumeWhitespace input) args where
	doCompile = do
	impls <- getAllValues
	let finishedImpls = map (\impl -> app1Hs (finishFn $ implType impl) impl) impls
	let body = foldl1 (flip$applyImpl.app1Hs("++sToA"++show separator++"++")) finishedImpls
	impl <- popArg 0 body
	nib <- gets getNib
	lit <- gets getLit
	return (impl, nib, lit)
	
getAllValues = do
	ParseData code _ _ _ <- get
	if empty code then return [] else do
		impl1 <- getValuesMemo 1
		getAllValues >>= return.(impl1++)

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

convertAutos :: [Impl] -> [Int] -> [Impl]
convertAutos l autos = zipWith (\e a -> (convertAuto e a)) l (autos ++ repeat undefined)

simplifyArgSpecs :: [ArgSpec] -> [[VT] -> Maybe ArgMatchResult]
simplifyArgSpecs = map simplifyArgSpec where
	simplifyArgSpec (Exact VAuto) vts = maybeMatch $ VAuto == last vts
	simplifyArgSpec (Exact spec) vts = maybeMatch $ spec == convertAutoType (last vts)
	simplifyArgSpec (Fn numRets f) _ = Just $ ArgFn (Fn numRets f)
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
convertLambda argTypes (ArgFn (Fn numRets argTypeFn), _) = do
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
	return $ addLambda newArg body
	where argType = argTypeFn argTypes

pushLambdaArg :: [VT] -> (Arg -> ParseState [Impl]) -> ParseState (Arg, Impl)
pushLambdaArg argType f = do
	newArg <- newLambdaArg argType
	depth <- gets pdContext >>= return.length
	rets <- f newArg
	let body = makePairs argType rets
	bodyWithLets <- popArg depth body
	return (newArg, bodyWithLets)

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
		