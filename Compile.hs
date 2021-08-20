module Compile(compile,padToEvenNibbles) where

import Data.List(inits,intercalate)
import Control.Monad (msum)
import Data.Maybe
import State

import Polylib(coerceTo,fillAccums,join)
import Ops
import Types
import Expr
import Args
import Parse
import Hs
import SmartList

padToEvenNibbles :: [Int] -> [Int]
padToEvenNibbles s = s ++ replicate (length s `mod` 2) uselessOp

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
		, ("intListList", VList [VList [VInt]], "intMatrix")
		]
	letArgs = map (\(name, vt, _) -> noArgsUsed {
		implType=vt, implCode=hsAtom name, implName=Just name, implUsed=OptionalArg
		}) mainLets
	letDefs = map (\(_, _, hsDef) -> hsDef) mainLets
	doCompile = do
		impl1 <- get1Value
		(dat,body) <- if separator == "" then mainCombiner impl1
			else testCombiner $ finishIt impl1
		context <- gets pdContext
		impl <- popArg 0 body
		nib <- gets getNib
		lit <- gets getLit

		let [fstIntUsed,fstLineUsed,intsUsed,sndIntUsed,sndLineUsed,allInputUsed,allInputUsedAsInts] =
			getInputsUsedness context

		let autoMap = if allInputUsed || allInputUsedAsInts && not (isJust dat) then ""
			else if sndLineUsed then "intercalate [10] $ flip map (listOr [[]] (reshape 2 strLines)) $ \\strLines -> "
			else if fstLineUsed then "intercalate [10] $ flip map (listOr [[]] (reshape 1 strLines)) $ \\strLines -> "
			else if intsUsed then "let intMatrix2 = if length intMatrix > 1 && (any ((>1).length) intMatrix) then intMatrix else [intList] in intercalate [10] $ flip map intMatrix2 $ \\intList ->"
			else if sndIntUsed then "intercalate [10] $ flip map (listOr [[]] (reshape 2 intList)) $ \\intList -> "
			else if fstIntUsed then "intercalate [10] $ flip map (listOr [[]] (reshape 1 intList)) $ \\intList -> "
			else ""
		let finalImpl = app1Hs ("let intMatrix=filter (not.null) (map (asInts.sToA) (lines $ aToS input));\
			\strLines=map sToA $ lines $ aToS input;\
			\intList="++(fromMaybe "concat intMatrix" (dat>>=(\d->Just$"["++show d++"]"))) ++ ";\
			\in "++autoMap) impl
		return (finalImpl, nib, lit)
	finishIt impl = (app1Hs (finishFn $ implType impl) impl) { implType = vstr }
	join2 impl1 impl2 = applyImpl (app1Hs ("(\\a b->a++sToA"++show separator++"++b)") impl1) impl2

	mainCombiner :: Impl -> ParseState (Maybe Integer, Impl)
	mainCombiner prev = do
		let finishedPrev = finishIt prev
		ParseData code _ _ _ <- get
		if empty code then return (Nothing, finishedPrev)
		else do
		 context <- gets pdContext
		 let inputsUsed = getInputsUsedness context
		 case match code (["~"],[0]) of -- don't make this mean data unless they use it!
			Just nextCode | head inputsUsed -> do
				appendRep ([0],"~")
				modify $ \s -> s { pdCode=nextCode }
				dat <- parseDataExpr
-- 				error $ show dat
				return (Just dat,finishedPrev)
			otherwise -> do
			result <- case implType prev of
				VInt -> do
					(impl1,argsUsed) <- getLambdaValue 1 [VInt,VInt] OptionalArg
					if last argsUsed then do
						return $ (applyImpl (app1Hs ("("++foldr1Fn [VList [VInt], implType impl1]++")") (app1Hs "(\\x->[1..x])" prev)) impl1) { implType = VInt }
					else if argsUsed == [True,False] then do
						return $ (applyImpl (app1Hs "(\\a f -> map (flip f ()) [1..a])" prev) impl1) { implType = VList $ ret $ implType impl1 }
					else do
						rhsImpl <- convertPairToLet (app1Hs "(\\f->f()())" impl1) (ret $ implType impl1)
						return $ join2 finishedPrev (finishIt rhsImpl)
				VList e | e /= [VChr] -> do
					(impl1,argsUsed) <- getLambdaValue 1 (e++e) OptionalArg
					if or $ drop (length e) argsUsed then do
						convertPairToLet (applyImpl (applyImpl (noArgsUsed { implCode=hsParen $ hsAtom $ foldr1Fn [implType prev, implType impl1] }) prev) impl1) e
					else if or $ take (length e) argsUsed then do
						return $ (applyImpl (applyImpl (noArgsUsed { implCode=hsParen $ hsAtom $ mapFn [implType prev, implType impl1] }) prev) (app1Hs (fillAccums (length e) (2*length e)) impl1)) { implType = VList $ ret $ implType impl1 }
					else if ret (implType impl1) == [vstr] then do
						let jstr = app1Hs (fillAccums (2*length e) (2*length e)) impl1
						let (rt,f) = join (VList e)
						return $ (applyImpl (app1Hs f jstr) prev) { implType = rt }
					else do
						rhsImpl <- convertPairToLet (app1Hs (fillAccums (2*length e) (2*length e)) impl1) (ret $ implType impl1)
						return $ join2 finishedPrev (finishIt rhsImpl)
				otherwise -> do
					impl1 <- get1Value
					return $ join2 finishedPrev (finishIt impl1)
			
			mainCombiner result
	
	testCombiner prev = do
		ParseData code _ _ _ <- get
		if empty code then return (Nothing, prev)
		else do
			impl1 <- get1Value
			case impl1 of -- stupid work around because laziness causes infinite loop on error if
				(Impl _ _ _ _ _) -> testCombiner (join2 prev (finishIt impl1))
	
	getInputsUsedness context = tail $ map ((==UsedArg).implUsed) $ argImpls $ last context

applyImpl :: Impl -> Impl -> Impl
applyImpl (Impl t1 hs1 d1 _ _) (Impl _ hs2 d2 _ _) = Impl t1 (hsApp hs1 hs2) (max d1 d2) undefined undefined

makePairs :: [VT] -> [Impl] -> Impl
makePairs fromTypes args = foldl applyImpl initImpl args where
	toTypes = map implType args
	initImpl = noArgsUsed { implType=VFn fromTypes toTypes, implCode=hsAtom pairMakerHs }
	-- todo instead of fold apply, build the (expr1, expr2), etc, cleaner hs
	pairMakerHs = if length args == 1 then "" else "("++replicate (length $ tail args) ','++")"

-- add the current rep to the partialFinalState
putAddRep :: ParseData -> ParseState ()
putAddRep (ParseData code context nib lit) = do
	appendRepH (nib,lit)
	modify $ \s -> s { pdCode=code, pdContext=context }

tryArg :: ArgSpec ->
		[VT] -- prev types
		-> [SmartList Int] -- nib reps of args (for commutative order check)
		-> [(Impl, ParseData)] -- memoized args, parsedata after arg
		-> ParseState (Maybe ([(Impl, ParseData)], -- the memoized args after parsing arg
                    	      [Impl])) -- the arg implementation (or empty)
tryArg (Cond desc c) prevTypes nibs memoArgs = do
	let (impl,nextState) = head memoArgs
	state <- get
	code <- gets pdCode
	
	if (isNothing $ match code (["~"],[0])) -- check this to avoid passing autos with undefined type
		&& c (MatchTestData (prevTypes++[implType impl]) nibs state)
	then do
		putAddRep nextState
		return $ Just (tail memoArgs, [impl])
	else return Nothing

tryArg (ParseArg parser) _ _ _ = do
	(t, code) <- parser
	return $ Just (error"todo memo args", [noArgsUsed { implType=t, implCode=hsAtom code }])

tryArg (Auto binOnly) _ _ memoArgs = do
	code <- gets pdCode
	let lit = if binOnly then [] else ["~"]
	case match code (lit,[0]) of
		Just nextCode -> do
			-- todo build this into match
			modify $ \s -> s { pdCode = nextCode }
			appendRep ([0],concat lit)
			return $ Just (tail memoArgs, [])
		Nothing -> return Nothing

tryArg (Fn f) prevTs _ _ = do
	let (nRets, argT) = f prevTs
	(impl,_) <- getLambdaValue nRets argT UnusedArg
	return $ Just (error"memoized args cannot be used after fn", [impl])

tryArg (AutoDefault tspec v) prevTypes nibs memoArgs = do
	code <- gets pdCode
	case match code (["~"],[0]) of
		Just nextCode -> do
			-- todo build this into match
			modify $ \s -> s { pdCode = nextCode }
			appendRep ([0],"~")
			return $ Just (tail memoArgs, [noArgsUsed { implType=VInt, implCode=i $ fromIntegral v }])
		Nothing -> tryArg tspec prevTypes nibs memoArgs

-- get the args (possibly fail), ok to modify parse state and fail
getArgs :: [ArgSpec] -> [(Impl, ParseData)] -> ParseState (Maybe [Impl])
getArgs = getArgsH [] []
getArgsH :: [Impl] -> [SmartList Int] -> [ArgSpec] -> [(Impl, ParseData)] -> ParseState (Maybe [Impl])
getArgsH prevArgs _ [] _ = return $ Just prevArgs
getArgsH prevArgs prevNibs (spec:s) memoArgs = do
	arg <- tryArg spec prevArgTypes nibs memoArgs
	case arg of
		Nothing -> return Nothing
		Just (nextMemoizedArgs, impl) -> getArgsH (prevArgs++impl) nibs s nextMemoizedArgs
	where
		nibs = (prevNibs++[pdNib $ snd $ head memoArgs])
		prevArgTypes = map implType prevArgs

getLambdaValue numRets argType argUsedness = do
	(newArg,body) <- pushLambdaArg argType argUsedness $ \newArg -> do
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

pushLambdaArg :: [VT] -> ArgUsedness -> (Arg -> ParseState [Impl]) -> ParseState (Arg, Impl)
pushLambdaArg argType argUsedness f = do
	newArg <- newLambdaArg argType argUsedness
	depth <- gets pdContext >>= return.length
	rets <- f newArg
	let body = makePairs argType rets
	finalContext <- gets pdContext
	let newArgFinal = reverse finalContext !! (depth - 1) -- todo inefficient
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

-- Gets the arg lists from each possible offset (ParseData is what is after the Impl)
exprsByOffset :: Thunk -> [[(Impl, ParseData)]]
exprsByOffset (Thunk code context) =
	getValues (Thunk code context) rest : rest where
		rest = exprsByOffset (Thunk (nextOffset code) context)

getValue :: [[(Impl, ParseData)]] -> ParseState Impl
getValue memoArgOffsets = do
	code <- gets pdCode
	if empty code
	then argImplicit
	else getValueH allOps memoArgOffsets
getValueH [] _ = parseError "Parse Error: no matching op"
getValueH ((isPriority,lit,nib,op):otherOps) memoArgOffsets = do
	code <- gets pdCode
	let tryRest = getValueH otherOps memoArgOffsets

	-- Low priority extensions don't match if their snd nibble is in an extensions (and thus would be renamed).
	let reconstructedLit = dToList $ pdLit $ snd $ head $ head memoArgOffsets
	case code of
		(Nib _ _)
			| not isPriority
			&& concat lit !! 1 /= head reconstructedLit -> tryRest
-- 			check that second memo lit == snd lit
				 
		_ -> do
			case match code (lit, nib) of
				Nothing -> tryRest
				Just afterOpCode -> do
					origState <- get
					modify $ \s -> s {pdCode=afterOpCode}
					let valList = head (drop (cp afterOpCode - cp code - 1) memoArgOffsets)
					appendRep (nib,concat lit)
					attempt <- convertOp valList op code
					case attempt of
						Just impl -> return impl
						Nothing -> do
							put origState
							tryRest

convertOp :: [(Impl, ParseData)] -> Operation -> Code -> ParseState (Maybe Impl)
convertOp memoizedArgList (ats,impl) preOpCode = do
	maybeArgs <- getArgs ats memoizedArgList
	case maybeArgs of
		Nothing -> return Nothing
		Just args -> do
			afterArgsCode <- gets pdCode
		
			-- Temporarily put the code pointer back for useful op error messages
			modify $ \s -> s { pdCode=preOpCode }
			(rt, initImpl) <- impl $ map implType args
			afterImplCode <- gets pdCode
			if (cp preOpCode) /= (cp afterImplCode)
			then error "modifying code in op impl is not supported (but it could be)"
			else modify $ \s -> s { pdCode=afterArgsCode }
		
			let fullImpl = foldl applyImpl initImpl args
			unpairedFirst <- convertPairToLet fullImpl rt
			result <- applyFirstClassFn unpairedFirst
			return $ Just result

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
coerceImpl (Impl et hs dep _ _) t = Impl t (hsApp (hsAtom$coerceTo [t] [et]) hs) dep undefined undefined

convertPairToLet :: Impl -> [VT] -> ParseState Impl
convertPairToLet (Impl _ hs dep _ _) [t] = return $ Impl t hs dep undefined undefined
convertPairToLet impl implTypes = do
	context <- gets pdContext
	let letArg = newLetArg context impl implTypes
	modify $ \s -> s { pdContext=letArg:context }
	return $ head $ argImpls letArg
