{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

module Compile(compile,padToEvenNibbles) where

import Data.List(inits,intercalate)
import Control.Monad (msum)
import Data.Maybe
import State
import qualified Data.Set as Set

import Polylib(coerceTo,fillAccums,join,truthy,curryN,rotateTuple,flattenTuples,fullVectorize,baseElem,cidim,         promoteList,coerce)
import Ops
import Types
import Expr
import Args
import Parse
import Hs
import SmartList

padToEvenNibbles :: [Int] -> [Int]
padToEvenNibbles s = s ++ replicate (length s `mod` 2) uselessOp

compile :: (?isSimple::Bool) => (VT -> Bool -> String) -> String -> Code -> (Impl, [Int], String, [String])
compile finishFn separator input = evalState doCompile $ blankRep (consumeWhitespace input) args where
	args =
		[ Arg (Impl undefined (hsAtom"_") (Set.singleton 0) Nothing UsedArg:letArgs)
			(LetArg $ hsAtom $ "(undefined," ++ intercalate "," letDefs ++ ")")]
	mainLets =
		[ ("firstInt", VInt, "if datOverride then dat else fromMaybe 100 $ at intList 0")
		, ("firstLine", vstr, "fromMaybe printables $ at strLines 0")
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
			else testCombiner $ finishIt impl1 False
		context <- gets pdContext
		impl <- popArg 0 body
		nib <- gets getNib
		lit <- gets getLit
		dataUsed <- gets pdDataUsed

		let [fstIntUsed,fstLineUsed,intsUsed,sndIntUsed,sndLineUsed,allInputUsed,allInputUsedAsInts] =
			getInputsUsedness context
		
		let useDataInsteadOfFirstIntInput = isJust dat && not dataUsed

		-- todo idea: only auto map on snd (first is like a header)
		let autoMap = if ?isSimple then "" else
			if allInputUsed || allInputUsedAsInts then ""
			else if sndLineUsed then "let autoMapList = (listOr [[]] (chunksOf 2 strLines)) in intercalate [newli] $ flip map autoMapList $ \\strLines -> "
			else if fstLineUsed then "let autoMapList = (listOr [[]] (chunksOf 1 strLines)) in intercalate [newli] $ flip map autoMapList $ \\strLines -> "
			-- todo could also set a customer inner seperator
			else if intsUsed then "let autoMapList = if length intMatrix > 1 && (any ((>1).length) intMatrix) then intMatrix else [intList] in intercalate [newli] $ flip map autoMapList $ \\intList -> "
			else if sndIntUsed then "let autoMapList = (listOr [[]] (chunksOf 2 intList)) in intercalate [newli] $ flip map autoMapList $ \\intList -> "
			else if fstIntUsed && not useDataInsteadOfFirstIntInput then "let autoMapList = (listOr [[]] (chunksOf 1 intList)) in intercalate [newli] $ flip map autoMapList $ \\intList -> "
			else ""
		let finalImpl = app1Hs ("let intMatrix=filter (not.null) (map (asInts.sToA) (lines $ aToS input));\
			\strLines=map sToA $ lines $ aToS input;\
			\intList=concat intMatrix;\
			\datOverride="++show useDataInsteadOfFirstIntInput ++ ";\
			\dat="++show (fromMaybe 0 dat) ++";\
			\autoMapList=[];\
			\in "++autoMap++"let (firstSep,secondSep)=if length autoMapList > 1 then ([],[space]) else ([space],[newli]) in") impl
		warnings <- gets pdLitWarnings
		return (finalImpl, nib, lit, warnings)
	
	finishIt impl isLast = (app1Hs (finishFn (implType impl) isLast) impl) { implType = vstr }
	join2 impl1 impl2 = applyImpl (app1Hs ("(\\a b->a++sToA"++show separator++"++b)") impl1) impl2

	mainCombiner :: Impl -> ParseState (Maybe Integer, Impl)
	mainCombiner prev = do
		code <- gets pdCode
		let finishedPrev = finishIt prev (empty code)
		if empty code then return (Nothing, finishedPrev)
		else do
			context <- gets pdContext
			dataUsed <- gets pdDataUsed
			doData <- if dataUsed then
				return True
			else if head (getInputsUsedness context)  -- don't make this mean data unless they use it!
				then match tildaOp
			else return False
			
			if doData then do
				dat <- parseDataExpr
				return (Just dat,finishedPrev)
			else do
			result <- case implType prev of
				VInt | not ?isSimple -> do
					(impl1,argsUsed) <- getLambdaValue 1 [VInt,VInt] OptionalArg
					if last argsUsed then do
						return $ (applyImpl (app1Hs ("("++foldr1Fn [VList [VInt], implType impl1]++")") (app1Hs "(\\x->[1..x])" prev)) impl1) { implType = VInt }
					else if argsUsed == [True,False] then do
						return $ (applyImpl (app1Hs "(\\a f -> map (\\y->f (y,())) [1..a])" prev) impl1) { implType = VList $ ret $ implType impl1 }
					else do
						rhsImpl <- convertPairToLet UnusedArg (app1Hs (fillAccums 2 0) impl1) (ret $ implType impl1)
						afterCode <- gets pdCode
						return $ join2 finishedPrev (finishIt rhsImpl (empty afterCode))
				VList e | not ?isSimple && e /= [VChr] -> do
					(impl1,argsUsed) <- getLambdaValue 1 (e++e) OptionalArg
					if or $ drop (length e) argsUsed then do
						convertPairToLet UnusedArg (applyImpl (applyImpl (noArgsUsed { implCode=hsParen $ hsAtom $ foldr1Fn [implType prev, implType impl1] }) prev) impl1) e
					else if or $ take (length e) argsUsed then do
						return $ (applyImpl (applyImpl (noArgsUsed { implCode=hsParen $ hsAtom $ mapFn [implType prev, implType impl1] }) prev) (app1Hs (fillAccums (length e) (2*length e)) impl1)) { implType = VList $ ret $ implType impl1 }
					else if ret (implType impl1) == [vstr] then do
						let jstr = app1Hs (fillAccums (2*length e) (2*length e)) impl1
						let (rt,f) = join (VList e)
						return $ (applyImpl (app1Hs f jstr) prev) { implType = rt }
					else do
						rhsImpl <- convertPairToLet UnusedArg (app1Hs (fillAccums (2*length e) (2*length e)) impl1) (ret $ implType impl1)
						afterCode <- gets pdCode
						return $ join2 finishedPrev (finishIt rhsImpl (empty afterCode))
				otherwise -> do
					impl1 <- get1Value
					afterCode <- gets pdCode
					return $ join2 finishedPrev (finishIt impl1 (empty afterCode))
			
			mainCombiner result
	
	testCombiner prev = do
		code <- gets pdCode
		if empty code then return (Nothing, prev)
		else do
			impl1 <- get1Value
			case impl1 of -- stupid work around because laziness causes infinite loop on error if
				(Impl _ _ _ _ _) -> testCombiner (join2 prev (finishIt impl1 False))
	
	getInputsUsedness context = tail $ map ((==UsedArg).implUsed) $ argImpls $ last context

applyImpl :: Impl -> Impl -> Impl
applyImpl impl1 impl2 | elem (implType impl2) [OptionYes, OptionNo] = impl1
applyImpl (Impl t1 hs1 d1 _ _) (Impl t2 hs2 d2 _ _) =
	-- This type annotation isn't needed but add it to the haskell code to catch if we forget to correctly set them (i.e. Int versus Integer)
	let hs2annotated = case toHsType t2 of
		Just ts -> hsApp (hsParen hs2) $ hsAtom $ "::"++ts
		Nothing -> hsParen hs2 in
	Impl t1 (hsApp hs1 hs2annotated) (Set.union d1 d2) undefined undefined

makePairs :: [VT] -> [Impl] -> Impl
makePairs fromTypes args = foldl applyImpl initImpl args where
	toTypes = map implType args
	initImpl = noArgsUsed { implType=VFn fromTypes toTypes, implCode=hsAtom pairMakerHs }
	-- todo instead of fold apply, build the (expr1, expr2), etc, cleaner hs
	pairMakerHs = if length args == 1 then "" else "("++replicate (length $ tail args) ','++")"

-- todo fn name is a lie, also updates context
-- add the current rep to the partialFinalState
putAddRep :: ParseData -> ParseState ()
putAddRep (ParseData code context nib lit dataUsed warnings) = do
	appendRepH (nib,lit)
	dataUsed1 <- gets pdDataUsed
	context1 <- gets pdContext
	warnings1 <- gets pdLitWarnings
	modify $ \s -> s { pdCode=code, pdContext=unionUsed context1 context, pdDataUsed=dataUsed1 || dataUsed, pdLitWarnings = warnings ++ warnings1 }

unionUsed :: [Arg] -> [Arg] -> [Arg]
unionUsed lhs rhs =
	let (newOnes,oldOnes) = splitAt (length rhs - length lhs) rhs
	in newOnes ++ (zipWith (\a b->b { argImpls=zipWith unionUsed1 (argImpls a) (argImpls b) } ) lhs oldOnes)
	where unionUsed1 lhs rhs =
		rhs { implUsed = if implUsed rhs==UsedArg then implUsed rhs else implUsed lhs }

tryArg :: (?isSimple::Bool) => ArgSpec ->
		[VT] -- prev types
		-> [SmartList Int] -- nib reps of args (for commutative order check)
		-> [(Impl, ParseData)] -- memoized args, parsedata after arg
		-> ParseState (
			Either
				([(Impl, ParseData)], -- the memoized args after parsing arg
             [Impl]) -- the arg implementation (or empty)
            (Maybe Impl)) -- a constant resulting from unused args of a fn
tryArg (Cond desc c) prevTypes nibs memoArgs = do
	let (impl,nextState) = head memoArgs
	state <- get
	code <- gets pdCode
	
	if (isNothing $ onlyCheckMatch code tildaOp) -- check this to avoid passing autos with undefined type
		&& c (MatchTestData (prevTypes++[implType impl]) nibs state)
	then do
		putAddRep nextState
		return $ Left (tail memoArgs, [impl])
	else return $ Right Nothing

tryArg (ParseArg _ parser) _ _ _ = do
	(t,code) <- parser
	return $ Left (error"todo memo args", [noArgsUsed { implType=t, implCode=hsAtom code }])

tryArg (Auto binOnly) _ _ memoArgs = do
	let lit = if binOnly then [] else snd tildaOp
	matched <- match (fst tildaOp, lit)
	return $ if matched then Left (tail memoArgs, []) else Right Nothing

-- todo create another one called UnusedLeftOver which acts more like a normal Cond
tryArg (Fn reqArgUse argUsedness f) prevTs _ _ = do
	let (nRets, argT) = f prevTs
	-- todo make fn0 cleaner here?
	(impl,used) <- getLambdaValue nRets argT argUsedness
	if reqArgUse && not (or used) then
		return $ Right $ Just impl
	else
		return $ Left (error"memoized args cannot be used after fn", [impl])

tryArg (OptionalFn f) prevTs _ memoArgs = do
	let (nRets, argT) = f prevTs
	(impl,used) <- getLambdaValue nRets argT UnusedArg
	code <- gets pdCode
	context <- gets pdContext
	if not (or used) then
		return $ Left ((impl, blankRep code context):error "cannot used memo args after the one after optional fn", [noArgsUsed { implType=ItWasAConstant}])
	else
		return $ Left (head $ exprsByOffset $ Thunk code context, [impl])

tryArg (OrAuto _ nonAutoSpec) prevTs nibs memoArgs = do
	matched <- match tildaOp
	if matched 
	then return $ Left $ (tail memoArgs, [noArgsUsed { implType=OptionYes }])
	else tryArg nonAutoSpec prevTs nibs memoArgs


tryArg (AutoNot fn) prevTs _ _ = do
	matched <- match tildaOp
	afterArg <- tryArg fn prevTs (error"impossible 43") (error"impossible 44")
	return $ case afterArg of
		Left (memo,[impl]) -> 
			let
				truthyImpl = app1Hs ((truthy $ ret $ implType impl)++".") impl
				modifiedImpl = if matched then app1Hs "not." truthyImpl else truthyImpl
			in Left (memo,[modifiedImpl])
		Right _ -> error "can't use not of const fn yet"

tryArg (AutoOption desc) prevTs nibs memoArgs = do
	matched <- match tildaOp
	return $ Left $ if matched
		then (tail memoArgs, [noArgsUsed { implType=OptionYes }])
		else (     memoArgs, [noArgsUsed { implType=OptionNo }])

tryArg (AutoDefault tspec v) prevTypes nibs memoArgs = do
	matched <- match tildaOp
	if matched
	then return $ Left (tail memoArgs, [noArgsUsed { implType=VInt, implCode=i v }])
	else tryArg tspec prevTypes nibs memoArgs

tryArg (AutoData tspec) prevTypes nibs memoArgs = do
	matched <- match tildaOp
	if matched
	then do
		modify $ \s -> s { pdDataUsed = True }
		return $ Left (tail memoArgs, [noArgsUsed { implType=VInt, implCode=hsAtom"dat" }])
	else tryArg tspec prevTypes nibs memoArgs

tryArg ZipMode prevTypes _ memoArgs = do
	impl <- parse1Nibble "zip mode" $ zip [0..] (specialZips prevTypes)
	return $ Left (error"memoized args cannot be used after zip mode (but could be)", [impl])

createImplMonad t hs = return $ noArgsUsed { implType=t, implCode=hs }
createSpecialFn :: (([VT], String)) -> ParseState Impl
createSpecialFn (ts,hs) = createImplMonad (VFn undefined ts) (hsParen $ hsAtom hs)

createZipFromBinOp :: Char -> [VT] -> ParseState Impl
createZipFromBinOp c a@[a1,a2] =
	let (t,hs,(lhsNeed,rhsNeed))=binOp c $ map baseElem a
	    lhsDim = cidim [a1]
	    rhsDim = cidim [a2]
	    (vec,extra) = fullVectorize (lhsDim-lhsNeed-1) (rhsDim-rhsNeed-1)
	    vt = iterate (VList.(:[])) (t) !! extra
	in createImplMonad (VFn undefined [vt]) (hsParen $ hsAtom $ vec ++ "(" ++ hs ++ ")")

-- todo option for a zip3?
-- also allow remap of ! "abc" 2 = to be a flipped version of an op? since that is pointless as is (just use non vec version

specialZips :: (?isSimple::Bool) => [VT] -> [(Char, ParseState Impl)]
specialZips a@[a1,a2] = let 
		[l1,l2] = map (length.elemT) a
		flatten = flattenTuples l1 l2
		flatT = concatMap elemT a
	in
		[('~',getLambdaValue 1 flatT UnusedArg >>= return . (app1Hs $ "(\\f a b->f $ "++flatten++"(a,b))") . fst)
		,(':', let -- todo combine with append op, but first need to make anyT not a fn
			(ap,apFn) = promoteList (elemT a1)
			(coercedType, coerceFnA, coerceFnB) = coerce [ap] (elemT a2)
		in
			createSpecialFn (coercedType, "\\a b->("++coerceFnA++"$"++apFn++"a)++"++coerceFnB++"b"))
		,(',',createSpecialFn (flatT, "\\a b->"++flatten++" $ (a,b)"))] ++ 
		map (\c->(c,createZipFromBinOp c a)) "+*-/%^][!=?"
		-- two more possible


-- get the args (possibly fail), ok to modify parse state and fail
getArgs :: (?isSimple::Bool) => [ArgSpec] -> [(Impl, ParseData)] -> ParseState (Maybe ([Impl]))
getArgs = getArgsH [] []
getArgsH :: (?isSimple::Bool) => [Impl] -> [SmartList Int] -> [ArgSpec] -> [(Impl, ParseData)] -> ParseState (Maybe [Impl])
getArgsH prevArgs _ [] _ = return $ Just prevArgs
getArgsH prevArgs prevNibs (spec:s) memoArgs = do
	arg <- tryArg spec prevArgTypes nibs memoArgs
	case arg of
		Right _ -> return Nothing
		Left (nextMemoizedArgs, impl) -> getArgsH (prevArgs++impl) nibs s nextMemoizedArgs
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

get1Value :: (?isSimple::Bool) => ParseState Impl
get1Value = do
	v <- getValuesMemo 1
	return $ head v

toThunk state = Thunk (pdCode state) (pdContext state)

-- Gets the arg list
getValuesMemo :: (?isSimple::Bool) => Int -> ParseState [Impl]
getValuesMemo n = do
	state <- get
	let exprs = take n $ head $ exprsByOffset $ toThunk state
	mapM (putAddRep.snd) exprs
	return $ map fst exprs

data Thunk = Thunk Code [Arg]

-- Gets the arg list (given an arg from each possible offset after this one)
getValues :: (?isSimple::Bool) => Thunk -> [[(Impl, ParseData)]] -> [(Impl, ParseData)]
getValues (Thunk code context) offsetExprs = (impl, after) : getValues afterThunk offsetAfterExprs where
	(impl, after) = runState (getValue offsetExprs) $ blankRep code context
	afterThunk = Thunk (pdCode after) (pdContext after)
	offsetAfterExprs =
		if length (pdContext after) == length context
		then drop (cp (pdCode after) - cp code) offsetExprs
		else drop 1 $ exprsByOffset afterThunk

-- Gets the arg lists from each possible offset (ParseData is what is after the Impl)
exprsByOffset :: (?isSimple::Bool) => Thunk -> [[(Impl, ParseData)]]
exprsByOffset (Thunk code context) =
	getValues (Thunk code context) rest : rest where
		rest = exprsByOffset (Thunk (nextOffset code) context)

getValue :: (?isSimple::Bool) => [[(Impl, ParseData)]] -> ParseState Impl
getValue memoArgOffsets = do
	let ops = if ?isSimple then simpleOps else allOps
	code <- gets pdCode
	if empty code
	then argImplicit
	else getValueH ops memoArgOffsets
getValueH [] _ = parseError "Parse Error: no matching op"
getValueH ((isPriority,lit,nib,op):otherOps) memoArgOffsets = do
	code <- gets pdCode
	let tryRest = getValueH otherOps memoArgOffsets
	-- Low priority extensions don't match if their snd nibble is in an extensions (and thus would be renamed).
	let reconstructedLit = dToList $ pdLit $ snd $ head $ head memoArgOffsets
	origState <- get
	match <- match (nib,lit)
	if not match then tryRest
	else if not isPriority && isBinary code && concat lit !! 1 /= head reconstructedLit
	then do
		put origState
		tryRest
	else do
		afterOpCode <- gets pdCode
		let valList = head (drop (cp afterOpCode - cp code - 1) memoArgOffsets)
		attempt <- convertOp valList op code
		case attempt of
			Just impl -> return impl
			Nothing -> do
				put origState
				tryRest

convertOp :: (?isSimple::Bool) => [(Impl, ParseData)] -> Operation -> Code -> ParseState (Maybe Impl)
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
			-- todo, might need to do swapping elsewhere if the first arg could have been a tuple too, this assumes it was 1 that is why rotating is a true swap
			unpairedFirst <- convertPairToLet (implUsed initImpl) fullImpl rt
			result <- applyFirstClassFn unpairedFirst
			return $ Just result

-- todo memoize the parse
-- todo could put in getValue if wanted to support real first class functions
-- todo could unify function calling with convertOp code
applyFirstClassFn :: (?isSimple::Bool) => Impl -> ParseState Impl
applyFirstClassFn (Impl (VFn from to) hs dep _ _) = getNArgs from >>= \impls -> do
	let initImpl = Impl undefined hs dep undefined undefined
	convertPairToLet UnusedArg (foldl applyImpl (app1Hs (curryN (length impls)) initImpl) impls) to
applyFirstClassFn x = return x

getNArgs :: (?isSimple::Bool) => [VT] -> ParseState [Impl]
getNArgs argTypes = do
	args <- getValuesMemo (length argTypes)
	return $ zipWith coerceImpl args argTypes

coerceImpl :: Impl -> VT -> Impl
coerceImpl (Impl et hs dep _ _) t = Impl t (hsApp (hsAtom$coerceTo [t] [et]) hs) dep undefined undefined

convertPairToLet :: ArgUsedness -> Impl -> [VT] -> ParseState Impl
convertPairToLet _ (Impl _ hs dep _ _) [t] = return $ Impl t hs dep undefined undefined
convertPairToLet argUsedness impl implTypes = do
	context <- gets pdContext
	let letArg = newLetArg argUsedness context impl implTypes
	modify $ \s -> s { pdContext=letArg:context }
	return $ head $ argImpls letArg
