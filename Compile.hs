{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

module Compile(compile,padToEvenNibbles,charClassesDefs) where

import Data.List(inits,intercalate)
import Data.List.Split(splitOn)
import Data.Maybe
import State
import qualified Data.Set as Set

import Polylib(coerceTo,fillAccums,join,truthy,curryN,rotateTuple,flattenTuples,fullVectorize,baseElem,cidim,promoteListRepeat,promoteList,coerce,minForType,maxForType,defaultValue)
import Ops
import Types
import Expr
import Args
import Parse
import Hs
import SmartList

padToEvenNibbles :: [Int] -> [Int]
padToEvenNibbles s = s ++ replicate (length s `mod` 2) uselessOp

compile :: (?isSimple::Bool) => (VT -> Bool -> String) -> String -> [(String, VT, String)] -> Code -> (Impl, [Int], String, [String])
compile finishFn separator cArgs input = evalState doCompile $ blankRep (consumeWhitespace input) args where
	
	args =
		[ Args (Impl undefined (hsAtom"_") (Set.singleton 0) Nothing UsedArg:letArgs)
			(LetArg $ hsAtom $ "(undefined," ++ intercalate "," letDefs ++ ")") "inputs"]
	mainLets = cArgs ++
		[ ("fstInt", VInt, "if datOverride then dat else fromMaybe 100 $ at intList 0")
		, ("fstLine", vstr, "fromMaybe printables $ at strLines 0")
		, ("ints", VList [VInt], "intList")
		, ("sndInt", VInt, "fromMaybe 1000 $ at intList 1")
		, ("sndLine", vstr, "fromMaybe [] $ at strLines 1")
		, ("allInput", vstr, "input")
		, ("intMatrix", VList [VList [VInt]], "intMatrix_")
		, ("allLines", VList [vstr], "strLines")
		]
	letArgs = zipWith (\(name, vt, _) ind -> noArgsUsed {
		implType=vt, implCode=hsAtom name, implName=Just name, implUsed=if ind>length cArgs then OptionalArg else UnusedArg
		}) mainLets [1..]
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

		let [fstIntUsed,fstLineUsed,intsUsed,sndIntUsed,sndLineUsed,allInputUsed,allInputUsedAsInts,allInputUsedAsLines] =
			drop (length cArgs) $ getInputsUsedness context
		
		let useDataInsteadOfFirstIntInput = isJust dat && not dataUsed

		let autoMapCode = (if separator == "," then "in intercalate [newli]" else "in concat $ map finishLn")++" $ flip map autoMapList $ \\"

		-- todo idea: only auto map on snd (first is like a header)
		let autoMap = if ?isSimple then "" else
			if allInputUsed || allInputUsedAsInts || allInputUsedAsLines then ""
			else if sndLineUsed then "let autoMapList = (listOr [[]] (chunksOf 2 strLines)) "++autoMapCode++"strLines -> "
			else if fstLineUsed then "let autoMapList = (listOr [[]] (chunksOf 1 strLines)) "++autoMapCode++"strLines -> "
			-- todo could also set a customer inner seperator
			else if intsUsed then "let autoMapList = if length intMatrix_ > 1 && (any ((>1).length) intMatrix_) then intMatrix_ else [intList] "++autoMapCode++"intList -> "
			else if sndIntUsed then "let autoMapList = (listOr [[]] (chunksOf 2 intList)) "++autoMapCode++"intList -> "
			else if fstIntUsed && not useDataInsteadOfFirstIntInput then "let autoMapList = (listOr [[]] (chunksOf 1 intList)) "++autoMapCode++"intList -> "
			else ""
		let finalImpl = app1Hs ("let intMatrix_=filter (not . null) (map (asInts.sToA) (lines $ aToS input));\
			\strLines=map sToA $ lines $ aToS input;\
			\intList=concat intMatrix_;\
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
				let changeFoldrToFoldl = intercalate "(foldl1.flip)" . splitOn "foldr1"
				result <- case implType prev of
					VInt | not ?isSimple -> do
						(impl1,argsUsed) <- getLambdaValue 1 [VInt,VInt] OptionalArg "implicit op int"
						if last argsUsed then do
							return $ (applyImpl (app1Hs (changeFoldrToFoldl $ "("++foldr1Fn [VList [VInt], implType impl1]++")") (app1Hs "(\\x->[1..x])" prev)) impl1) { implType = VInt }
						else if argsUsed == [True,False] then do
							return $ (applyImpl (app1Hs "(\\a f -> map (\\y->f (y,())) [1..a])" prev) impl1) { implType = VList $ ret $ implType impl1 }
						else do
							rhsImpl <- convertPairToLet UnusedArg (app1Hs (fillAccums 2 0) impl1) (ret $ implType impl1) "tuple in possible implicit op"
							afterCode <- gets pdCode
							return $ join2 finishedPrev (finishIt rhsImpl (empty afterCode))
					VList e | not ?isSimple && e /= [VChr] -> do
						(impl1,argsUsed) <- getLambdaValue 1 (e++e) OptionalArg "implicit op list"
						if or $ drop (length e) argsUsed then do
							convertPairToLet UnusedArg (applyImpl (applyImpl (noArgsUsed { implCode=hsParen $ hsAtom $ changeFoldrToFoldl $ foldr1Fn [implType prev, implType impl1] }) prev) impl1) e "tuple in implicit op foldl"
						else if or $ take (length e) argsUsed then do
							return $ (applyImpl (applyImpl (noArgsUsed { implCode=hsParen $ hsAtom $ mapFn [implType prev, implType impl1] }) prev) (app1Hs (fillAccums (length e) (2*length e)) impl1)) { implType = VList $ ret $ implType impl1 }
						-- this is moderately annoying
-- 						else if ret (implType impl1) == [vstr] then do
-- 							let jstr = app1Hs (fillAccums (2*length e) (2*length e)) impl1
-- 							let (rt,f) = Polylib.join (VList e)
-- 							return $ (applyImpl (app1Hs f jstr) prev) { implType = rt }
						else do
							rhsImpl <- convertPairToLet UnusedArg (app1Hs (fillAccums (2*length e) (2*length e)) impl1) (ret $ implType impl1) "tuple in possible implicit op"
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
	
	getInputsUsedness context = tail $ map ((==UsedArg).implUsed) $ argsImpls $ last context

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
-- arg is the snd thing (monad state is the first)
putAddRep :: ParseData -> ParseState ()
putAddRep (ParseData code context nib lit dataUsed implicitArgUsed warnings) = do
	appendRepH (nib,lit)
	dataUsed1 <- gets pdDataUsed
	context1 <- gets pdContext
	warnings1 <- gets pdLitWarnings
	modify $ \s -> s { pdCode=code, pdContext=unionUsed context1 context, pdDataUsed=dataUsed1 || dataUsed, pdLitWarnings = warnings ++ warnings1, pdImplicitArgUsed=implicitArgUsed }

unionUsed :: [Args] -> [Args] -> [Args]
unionUsed lhs rhs =
	let (newOnes,oldOnes) = splitAt (length rhs - length lhs) rhs
	in newOnes ++ (zipWith (\a b->b { argsImpls=zipWith unionUsed1 (argsImpls a) (argsImpls b) } ) lhs oldOnes)
	where unionUsed1 lhs rhs =
		rhs { implUsed = if implUsed rhs==UsedArg then implUsed rhs else implUsed lhs }

data ArgAttemptResult =
	Success
		[(Impl, ParseData)] -- the memoized args after parsing arg
		[Impl] -- the arg implementation (or empty)
	| FailConstFn Impl -- a constant resulting from unused args of a fn
	| FailTypeMismatch String -- reason for failure (aka "arg 2 is int")

tryArg :: (?isSimple::Bool) => ArgSpec
		-> String -- op being tried (for debug purposes only)
		-> [VT] -- prev types
		-> [SmartList Int] -- nib reps of args (for commutative order check)
		-> [(Impl, ParseData)] -- memoized args, parsedata after arg
		-> ParseState ArgAttemptResult
tryArg (Cond desc c) _ prevTypes nibs memoArgs = do
	let (impl,nextState) = head memoArgs
	state <- get
	code <- gets pdCode
	
	if (isNothing $ onlyCheckMatch code tildaOp) -- check this to avoid passing autos with undefined type
		&& c (MatchTestData (prevTypes++[implType impl]) nibs state)
	then do
		putAddRep nextState
		return $ Success (tail memoArgs) [impl]
	else return $ FailTypeMismatch $ "arg " ++ show (length prevTypes+1) ++ " is " ++ show (implType impl)

tryArg (ParseArg _ parser) _ _ _ _ = do
	(t,code) <- parser
	return $ Success (error"todo memo args") [noArgsUsed { implType=t, implCode=hsAtom code }]

tryArg Auto _ _ _ memoArgs = do
	matched <- match tildaOp
	return $ if matched then Success (tail memoArgs) [] else FailTypeMismatch "arg isn't ~"

tryArg (FakeAuto _) _ _ _ memoArgs = do
	return $ Success memoArgs []

tryArg (BinCode b) _ _ _ memoArgs = do
	matched <- match ([b], [])
	code <- gets pdCode
	context <- gets pdContext
	let regenedArgs = head $ exprsByOffset $ Thunk code context
	return $ if matched then Success (if isBinary code then regenedArgs else memoArgs) [] else FailTypeMismatch "arg bincode mismatch"

tryArg NotEOF _ _ _ memoArgs = do
	code <- gets pdCode
	implicitArgUsed <- gets pdImplicitArgUsed
	return $ if not (isBinary code) && empty code && implicitArgUsed
		then FailTypeMismatch "EOF cannot be here since this op needs part of its binary representation after the arg (try not using implicit args)"
		else Success memoArgs []

tryArg (LitCode l) _ _ _ memoArgs = do
	matched <- match ([], [[l]])
	code <- gets pdCode
	context <- gets pdContext
	let regenedArgs = head $ exprsByOffset $ Thunk code context
	return $ if matched then Success (if not $ isBinary code then regenedArgs else memoArgs) [] else FailTypeMismatch "arg litcode mismatch"

tryArg AnyS from prevTs _ _ =
	tryArg (Fn ReqDontCare UnusedArg (const (1,[]))) from prevTs undefined undefined

-- todo create another one called UnusedLeftOver which acts more like a normal Cond
tryArg (Fn reqArgUse argUsedness f) from prevTs _ _ = do
	let (nRets, argT) = f prevTs
	-- todo make fn0 cleaner here?
	-- fn0 doesn't use memoArgs but could, this makes things like ::::1 1 1 1 1 exponentially slow, because singleton needs to parse fully before seeing that second arg is ~
	(impl,used) <- getLambdaValue nRets argT argUsedness from
	let success = Success (error"memoized args cannot be used after fn")
	return $ case reqArgUse of
		ReqArg | not (or used) && not ?isSimple -> FailConstFn impl
		ReqConst -> if (or used) && not ?isSimple
			then FailTypeMismatch $ "arg " ++ show (length prevTs+1) ++ " is not a const"
			else success [app1Hs (fillAccums (length argT) (length argT)) impl]
		otherwise -> success [impl]

tryArg (OptionalFn f) from prevTs _ memoArgs = do
	let (nRets, argT) = f prevTs
	(impl,used) <- getLambdaValue nRets argT UnusedArg from
	code <- gets pdCode
	context <- gets pdContext
	if not (or used) then
		return $ Success ((impl, blankRep code context):error "cannot used memo args after the one after optional fn") [noArgsUsed { implType=ItWasAConstant}]
	else
		return $ Success (head $ exprsByOffset $ Thunk code context) [impl]

tryArg (OrAuto _ nonAutoSpec) from prevTs nibs memoArgs = do
	matched <- match tildaOp
	if matched 
	then return $ Success (tail memoArgs) [noArgsUsed { implType=OptionYes }]
	else tryArg nonAutoSpec from prevTs nibs memoArgs


tryArg (AutoNot fn) from prevTs _ _ = do
	matched <- match tildaOp
	afterArg <- tryArg fn from prevTs (error"impossible 43") (error"impossible 44")
	return $ case afterArg of
		Success memo [impl] -> 
			let
				truthyImpl = app1Hs ((truthy $ ret $ implType impl)++".") impl
				modifiedImpl = if matched then app1Hs "not." truthyImpl else truthyImpl
			in Success memo [modifiedImpl]
		FailConstFn impl -> FailConstFn impl

tryArg (AutoOption desc) _ prevTs nibs memoArgs = do
	matched <- match tildaOp
	return $ if matched
		then Success (tail memoArgs) [noArgsUsed { implType=OptionYes }]
		else Success       memoArgs  [noArgsUsed { implType=OptionNo }]

tryArg (AutoDefault tspec v) from prevTypes nibs memoArgs = do
	matched <- match tildaOp
	if matched
	then return $ Success (tail memoArgs) [noArgsUsed { implType=VInt, implCode=i v }]
	else tryArg tspec from prevTypes nibs memoArgs

tryArg (AutoData tspec) from prevTypes nibs memoArgs = do
	matched <- match tildaOp
	if matched
	then do
		modify $ \s -> s { pdDataUsed = True }
		return $ Success (tail memoArgs) [noArgsUsed { implType=VInt, implCode=hsAtom"dat" }]
	else tryArg tspec from prevTypes nibs memoArgs

tryArg CharClassMode _ _ _ memoArgs = do
	impl <- parse1Nibble "char class mode" $ zip [0..] charClasses
	return $ Success (error"memoized args cannot be used after char class mode (but could be)") [impl]

tryArg ZipMode _ [t1,VFn _ [t2]] _ memoArgs = do
	impl <- parse1Nibble "zip mode" $ zip [0..] (specialZips [t1,t2])
	return $ Success (error"memoized args cannot be used after zip mode (but could be)") [impl]

tryArg FoldMode _ prevTypes _ memoArgs = do
	impl <- parse1Nibble "fold mode" $ zip [0..] (specialFolds prevTypes)
	return $ Success (error"memoized args cannot be used after fold mode (but could be)") [impl]

createImplMonad t hs = return $ noArgsUsed { implType=t, implCode=hs }

createSpecialFn :: (([VT], String)) -> ParseState Impl
createSpecialFn (ts,hs) = createImplMonad (VFn undefined ts) (hsParen $ hsAtom hs)

createZipFromBinOp :: Char -> [VT] -> Int -> Int -> ParseState Impl
createZipFromBinOp c a@[a1,a2] lhsNeed rhsNeed =
	let lhsDim = cidim [a1]
	    rhsDim = cidim [a2]
	    (t,hs)=binOp c [head $ iterate ((:[]).VList) [baseElem a1] !! lhsNeed,baseElem a2]
	    (vec,extra) = fullVectorize (lhsDim-lhsNeed) (rhsDim-rhsNeed)
	    vt = iterate ((:[]).VList) t !! extra
	in createImplMonad (VFn undefined vt) (hsParen $ hsAtom $ vec ++ "(" ++ hs ++ ")")

-- todo option for a zip3?
-- also allow remap of ! "abc" 2 = to be a flipped version of an op? since that is pointless as is (just use non vec version

specialZips :: (?isSimple::Bool) => [VT] -> [(Char, ParseState Impl)]
specialZips a@[a1,a2] = let
		(a2p,ap2Fn) = promoteListRepeat [a2]
		[l1,l2] = map (length.elemT) [a1,a2p]
		flatten = flattenTuples l1 l2
		flatT = concatMap elemT [a1,a2p]
		(a1p,ap1Fn) = promoteList (elemT a1)
		(coercedType, coerceFnA, coerceFnB) = coerce [a1p] (elemT a2p)
	in
		-- if add then add to Quickref.hs !!!!!!!!!!
	
		[('~',getLambdaValue 1 flatT UnusedArg "zip with by" >>= \(impl,_) -> return $ app1Hs  ("(zipWith . \\f a b->f $ "++flatten++"(a,b))") impl { implType=VFn undefined [VList $ ret $ implType impl] })
		,(':', createSpecialFn ([VList coercedType], "\\a1 b1->zipWith (\\a b->("++coerceFnA++"$ "++ap1Fn++"a)++"++coerceFnB++"b) a1 ("++ap2Fn++"b1)"))
		,(',',createSpecialFn ([VList flatT], "\\a1 b1->zipWith (\\a b->"++flatten++" $ (a,b)) a1 ("++ap2Fn++"b1)"))
		] ++ -- ,('z',toImpl $ zipImpl a)] ++ 
		map (\c->(c,createZipFromBinOp c a 0 0)) "+*-/%^][!" ++
		map (\c->(c,createZipFromBinOp c a 1 0)) "=?"
		-- two more possible

createFoldFromBinOp :: (Char,String) -> VT -> ParseState Impl
-- todo handle tuples
createFoldFromBinOp (c,initValue) (VList [t]) =
	let (vt,hs)=binOp c [t,t]
	in createImplMonad (VFn undefined vt) (hsParen $ hsAtom $ "\\(foldType,initFn) a->if null a then initFn $ "++initValue++" else foldType (" ++ hs ++ ") a")

-- todo vectorize
specialFolds :: (?isSimple::Bool) => [VT] -> [(Char, ParseState Impl)]
specialFolds [a1] =
	(map (\deets->(fst deets,createFoldFromBinOp deets a1)) 

		-- if add then add to Quickref.hs !!!!!!!!!!

		[(']',minForType $ elemT a1)
		,('[',maxForType $ elemT a1)
		,('|',defaultValue $ elemT a1)
		,('&',maxForType $ elemT a1)
		,('+',"0")
		,('*',"1")
		,('-',"0")
		,('/',"1")
		,('%',"1")
		,('^',"1")])++
		[('>', takeAnotherOrderBy ">")
		,('<', takeAnotherOrderBy "<")
		-- todo: these only work in scan (would be useless in fold though) better error message at least
		,(':', createImplMonad (VFn undefined [a1]) (hsAtom $ "\\foldType a -> tail $ inits a"))
		,(';', createImplMonad (VFn undefined [a1]) (hsAtom $ "\\foldType a -> init $ tails a"))
		-- todo add more. bit ops? flipped ops?
		] where
	takeAnotherOrderBy fName = do
		getLambdaValue 1 (elemT a1) UnusedArg ("special fold "++fName) >>= (\(impl,_) -> return $ app1Hs ("(\\f (foldType,initFn) a->if null a then initFn $ "++defaultValue (elemT a1)++" else foldType (onToSelectBy ("++fName++") f) a)") impl { implType=VFn undefined (elemT a1) } )

charClasses :: (?isSimple::Bool) => [(Char, ParseState Impl)]
charClasses = map (\(c,hs)->(c,createImplMonad (VFn undefined [VInt]) (hsParen $ hsAtom hs))) charClassesDefs
charClassesDefs =
	[('a',"isAlpha") -- todo regular alpha (is letter)
	,('A',"not.isAlpha")
	,('n',"isAlphaNum")
	,('N',"not.isAlphaNum")
	,('s',"isSpace")
	,('S',"not.isSpace")
	,('l',"isLower")
	,('L',"not.isLower")
	,('u',"isUpper")
	,('U',"not.isUpper")
	,('p',"isPrint") -- 95 - no newline
	,('P',"not.isPrint") -- aka isControl
	,('d',"isDigit")
	,('D',"not.isDigit")
	,('$',"isSym")
	,('!',"not.isSym")]

-- If using BinCode, add some checks to the argument before BinCode to ensure there isn't an EOF in the lit there, if there was then the binary code would be invalid since the BinCode would be 1 arg too early.
addEofChecks (a:BinCode c:as) = a:NotEOF:BinCode c:addEofChecks as
addEofChecks (a:as) = a:addEofChecks as
addEofChecks [] = []

-- get the args (possibly fail with string reason), ok to modify parse state and fail
getArgs :: (?isSimple::Bool) => [ArgSpec] -> String -> [(Impl, ParseData)] -> ParseState (Either [Impl] String)
getArgs argSpecs = getArgsH [] [] (addEofChecks argSpecs)
getArgsH :: (?isSimple::Bool) => [Impl] -> [SmartList Int] -> [ArgSpec] -> String -> [(Impl, ParseData)] -> ParseState (Either [Impl] String)
getArgsH prevArgs _ [] from _ = return $ Left prevArgs
getArgsH prevArgs prevNibs (spec:s) from memoArgs = do
	arg <- tryArg spec from prevArgTypes nibs memoArgs
	case arg of
		FailConstFn _ -> return $ Right "lambda fn does not use its argument but is required to"
		FailTypeMismatch reason -> return $ Right reason
		Success nextMemoizedArgs impl -> getArgsH (prevArgs++impl) nibs s from nextMemoizedArgs
	where
		afterState = snd $ head memoArgs
		theseNibs = pdNib $ afterState -- this could cause a bug if true args after parsed arg aren't the memoized one, i.e. if you use BinCode arg type, but for now just avoid this, if really needed we could refactor code to return the actual bin rep.
		nibs = (prevNibs++if pdImplicitArgUsed afterState then [newSmartList []] else [theseNibs])
		prevArgTypes = map implType prevArgs

getLambdaValue numRets argType argUsedness from = do
	(newArg,body) <- pushLambdaArg argType argUsedness from $ \newArg -> do
		-- 0 is special case for letrec, this is a hacky way to replace the 3rd arg type
		-- with its real Fn type which can only be known after 2nd arg type is determined.
		-- It would very tricky to allow the the 3rd argument to do things like auto pair
		-- without this (and do things like only add the recursive function to args for it.
		if numRets == 0 then do
			let nonRecImpls = init $ argsImpls newArg
			modify $ \s -> s { pdContext=Args nonRecImpls LambdaArg "recursive fn" : tail (pdContext s) }
			match <- match tildaOp
			aaa <- getValuesMemo 1
			let aa = head aaa
			let truthyF = truthy [implType aa]
			let a = app1Hs ((if match then "not$" else "")++truthyF) aa { implType = InvalidType }
			bonus <- parseCountTuple
			b <- getValuesMemo (1+bonus)
			let from = map implType nonRecImpls
			let toType = map implType b
			let recType = VFn from toType
			let recImpl = (last $ argsImpls newArg) { implType=recType }
			let recArg = Args (nonRecImpls ++ [recImpl]) LambdaArg "recursive fn"
			modify $ \s -> s { pdContext=recArg : tail (pdContext s) }
			c <- getNArgs toType
			return $ [a]++[makePairs argType b]++[makePairs argType c]
		else do
			bonus <- parseCountTuple
			getValuesMemo (bonus + numRets)
	return $ (addLambda newArg body, map (\impl->UsedArg==implUsed impl) $ argsImpls newArg)

pushLambdaArg :: [VT] -> ArgUsedness -> String -> (Args -> ParseState [Impl]) -> ParseState (Args, Impl)
pushLambdaArg argType argUsedness from f = do
	newArg <- newLambdaArg argType argUsedness from
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

data Thunk = Thunk Code [Args]

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

data FailedMatch = FailedMatch {
	opName :: String,
	matchFailureReason :: String,
	expectedTypes :: [String]
}

getValue :: (?isSimple::Bool) => [[(Impl, ParseData)]] -> ParseState Impl
getValue memoArgOffsets = do
	let ops = if ?isSimple then simpleOps else allOps
	code <- gets pdCode
	identifier <- getArgByName
	if empty code
	then argImplicit
	else if isJust $ identifier then return $ fromMaybe undefined identifier
	else getValueH ops memoArgOffsets []
getValueH [] _ failedMatches = parseError $ "Parse Error: no matching op" ++ if null attempts then " (no matching identifier)" else ", tried: " ++ attempts
	where attempts = flip concatMap failedMatches $
		\(FailedMatch opName matchFailureReason expectedTypes) ->
			"\n"++opName++" "++unwords expectedTypes++", "++matchFailureReason
getValueH ((isPriority,lit,nib,op):otherOps) memoArgOffsets failedMatches = do
	code <- gets pdCode
	let tryRest newFailedMatches = getValueH otherOps memoArgOffsets newFailedMatches
	-- Low priority extensions don't match if their snd nibble is in an extension (and thus would be renamed).
	let reconstructedLit = dToList $ pdLit $ snd $ head $ head memoArgOffsets
	origState <- get
	match <- match (nib,lit)
	if not match then tryRest failedMatches
	else if not isPriority && isBinary code && concat lit !! 1 /= head reconstructedLit
	then do
		put origState
		tryRest failedMatches
	else do
		afterOpCode <- gets pdCode
		let valList = head (drop (cp afterOpCode - cp code - 1) memoArgOffsets)
		attempt <- convertOp valList (concat lit) op code
		case attempt of
			OpMatch impl -> return impl
			OpLitWarn msg -> do
				put origState
				parseLitWarning msg
				tryRest failedMatches
			OpTypeMismatch reason -> do
				put origState
				let opName = concat lit
				    expectedTypes = catMaybes $ map typeToStr $ fst op
				tryRest $ (FailedMatch opName reason expectedTypes) : failedMatches

data OpMatchResult = OpMatch Impl | OpTypeMismatch String | OpLitWarn String

-- returns impl or arg types that caused no match
convertOp :: (?isSimple::Bool) => [(Impl, ParseData)] -> String -> Operation -> Code -> ParseState OpMatchResult
convertOp memoizedArgList opName (ats,behavior) preOpCode = do
	maybeArgs <- getArgs ats opName memoizedArgList
	case (maybeArgs, behavior) of
		(Right reason,_) -> return $ OpTypeMismatch reason
		(Left args, LitWarn msg) -> do
			return $ OpLitWarn msg
		(Left args, CodeGen impl) -> do
			afterArgsCode <- gets pdCode
		
			-- Temporarily put the code pointer back for useful op error messages
			modify $ \s -> s { pdCode=preOpCode }
			parseCoordinates <- getParseCoordinates
			(rt, initImpl) <- impl $ map implType args
			afterImplCode <- gets pdCode
			if (cp preOpCode) /= (cp afterImplCode)
			then error "modifying code in op impl is not supported (but it could be)"
			else modify $ \s -> s { pdCode=afterArgsCode }
		
			let fullImpl = foldl applyImpl initImpl args
			-- todo, might need to do swapping elsewhere if the first arg could have been a tuple too, this assumes it was 1 that is why rotating is a true swap
			unpairedFirst <- convertPairToLet (implUsed initImpl) fullImpl rt $ opName ++ " " ++ parseCoordinates
			result <- applyFirstClassFn unpairedFirst
			return $ OpMatch result

-- todo memoize the parse
-- todo could put in getValue if wanted to support real first class functions
-- todo could unify function calling with convertOp code
applyFirstClassFn :: (?isSimple::Bool) => Impl -> ParseState Impl
applyFirstClassFn (Impl (VFn from to) hs dep _ _) = getNArgs from >>= \impls -> do
	let initImpl = Impl undefined hs dep undefined undefined
	convertPairToLet UnusedArg (foldl applyImpl (app1Hs (curryN (length impls)) initImpl) impls) to "first class fn application"
applyFirstClassFn x = return x

getNArgs :: (?isSimple::Bool) => [VT] -> ParseState [Impl]
getNArgs argTypes = do
	args <- getValuesMemo (length argTypes)
	return $ zipWith coerceImpl args argTypes

coerceImpl :: Impl -> VT -> Impl
coerceImpl (Impl et hs dep _ _) t = Impl t (hsApp (hsAtom$coerceTo [t] [et]) hs) dep undefined undefined

convertPairToLet :: ArgUsedness -> Impl -> [VT] -> String -> ParseState Impl
convertPairToLet _ (Impl _ hs dep _ _) [t] _ = return $ Impl t hs dep undefined undefined
convertPairToLet argUsedness impl implTypes from = do
	context <- gets pdContext
	let letArg = newLetArg argUsedness context impl implTypes from
	modify $ \s -> s { pdContext=letArg:context }
	return $ head $ argsImpls letArg

getParseCoordinates :: ParseState String
getParseCoordinates = do
	c <- gets pdCode
	return $ case c of
		(Nib _ _) -> ""
		(Lit f _ cp) ->
			let (lineno,charno) = getLitParseCoordinates f cp in
				"(line:"++show lineno++",char:"++show charno++")"
