-- Polymorphic functions for conversion at compile time
module Polylib(
	truthy,
	inspect,
	finish,
	join,
	vectorize,
	coerce,
	coerceEither,
	coerce2, coerceTo, -- test only
	curryN,
	flattenTuples,
	promoteList,
	promoteListRepeat,
	appTuple,
	firstOf,
	flatten,
	appFst,
	unzipTuple,
	rotateTuple,
	fillAccums,
	defaultValue,
	fullVectorize,
	baseElem,
	cidim,
	tupleLambda,
	minForType,
	maxForType,
	lazyOr,lazyAnd) where

import Types
import Data.List

truthy [VInt] = "(>0)"
truthy [VChr] = "(\\c->c>0 && not (isSpace$myChr c))"
truthy [VList _] = "(not.null)"
truthy (xs@(x:_)) = "("++truthy [x]++"."++firstOf xs++")" 

minForType ts | length ts > 1 = "("++intercalate "," (map (minForType.(:[])) ts) ++ ")"
minForType [VList _] = "[]"
minForType _ = "(-2^128)"

maxForType ts | length ts > 1 = "("++intercalate "," (map (maxForType.(:[])) ts) ++ ")"
maxForType [VList a] = "(repeat " ++ maxForType a ++ ")"
maxForType _ = "(2^128)"

inspect VInt = "(sToA.show.confirmInt)"
inspect VChr = "(sToA.show.myChr.confirmInt)"
inspect (VList [VChr]) = "(sToA.show.aToS.confirmList)"
inspect (VList et) = "(\\v -> (sToA \"[\") ++ (intercalate (sToA \",\") (map "++inspectElem et++" v)) ++ (sToA \"]\"))"
inspect a = error $ "unhandled type in inspect: " ++ show a
inspectElem [et] = inspect et
inspectElem ts = tupleLambda (length ts) $ \varNames -> "sToA \"(\"++"++
	(intercalate "++sToA \",\"++" $ zipWith (\t v->inspect t ++ v) ts varNames)
	++"++sToA \")\""

firstOf xs = tupleLambda (length xs) $ head

varNamesN n = map (\tn -> "a"++show tn) [1..n]

curryN n = "(\\f "++intercalate " " varNames++"->f "++toTuple varNames++")"
	where varNames = varNamesN n

tupleLambda n f = "(\\"++toTuple varNames++"->"++f varNames++")"
	where varNames = varNamesN n

appFst :: [VT] -> String -> String
appFst t op = tupleLambda (length t) $ \args -> toTuple ((op++" "++head args) : tail args)

unzipTuple :: VT -> ([VT], String)
unzipTuple (VList ts) = (map (VList.(:[])) ts, "(\\a->"++(toTuple $ map (\i->
	"map "++tupleLambda (length ts) (\args -> args!!i)++"a"
	) [0..length ts-1]) ++ ")")

rotateTuple :: Int -> String
rotateTuple n = tupleLambda n (toTuple.rotate) where rotate (a:as) = as++[a]

-- These ~ are because tuples aren't considered irrefutable in Haskell pattern matching, terrible blunder on Haskell's part in my opinion. Without this, things like iterateWhileUniq isn't lazy (that is it will compute the repeated value even if it isn't used). https://stackoverflow.com/questions/67995144/is-pattern-matching-tuples-not-fully-lazy/67995445#67995445
flattenTuples :: Int -> Int -> [Char]
flattenTuples t1 t2 = "(\\(~"++varsFrom 1 t1++", ~"++varsFrom (1+t1) (t1+t2)++")->("++varsFrom 1 (t1+t2)++"))"
	where varsFrom a b = toTuple $ map (\tn->"a"++show tn) [a..b]

finish :: VT -> Bool -> String
finish (VList tt) isLast
	| d >= 3 = joinC "[]"
	| d == 2 = joinC "firstSep"
	-- consider not adding space after thing? grand scheme?
	| d == 1 = (if isLast then id else compose1 "(++secondSep)") $ joinC "secondSep" -- todo might not want that newline for empty list? like unlines
	where
		d = sdim tt
		joinC s = compose1 (finish jt isLast) $ app1 js s where (jt,js) = join (VList tt)
finish t _ = toStr t

compose1 a b = "(" ++ a ++ "." ++ b ++ ")"
app1 a b = "(" ++ a ++ b ++ ")"

toStr VInt = inspect VInt
toStr VChr = "(:[])"
toStr (VList [VChr]) = " id "
toStr a = error $ "(internal) can't to str from " ++ show a

joinH t 0 = (vstr, "(const"++toStr t++")")
joinH (VList [e]) 1 = (vstr, "(\\a b->intercalate a $ map "++toStr e++" b)")
joinH (VList [VList e]) depth = (VList [rt], "(map."++ej++")") where (rt, ej)=joinH (VList e) (depth - 1)
joinH (VList (ts@(_:_))) 2 = (VList [vstr], "(\\a b->map (intercalate a . "++tuple2List (length ts) ++ "."++(appTuple $ map toStr ts) ++ ") b)")
joinH (VList (ts@(_:_))) depth = (VList rts, "(\\a b->map ("++appTuple rfsa++") b)") where
	(rts, rfs)=unzip $ map (\t->joinH t (depth-2)) ts
	rfsa = map (\f->f++" a ") rfs
joinH ts _ = (ts, "(\\_ b->b)")

join t = joinH t (sdim $ elemT t)

tuple2List n = tupleLambda n $ \args -> "["++intercalate "," args++"]"

vectorize :: String -> ([VT] -> VT) -> [VT] -> (VT, String)
vectorize op rtf [t1, VList t2] =
	(VList rts, "(\\a->map "++appTuple (map (\rop->"("++rop++"a)") rops)++")") where
		(rts, rops) = unzip $ map (\t->vectorize op rtf [t1,t]) t2
vectorize op rtf [t1, t2] = (rtf [t1, t2], "("++op++")")

fullVectorize :: Int -> Int -> (String, Int)
-- Int's are the number of extra dimensions than needed (0 = exact)
fullVectorize lhsDim rhsDim = let
	extra = max lhsDim rhsDim
	lhsRepeat = cycleN "repeat" "a" (extra - lhsDim)
	rhsRepeat = cycleN "repeat" "b" (extra - rhsDim)
	cycleN s init n = " " ++ (if n < 0 then init
		else iterate (\inner->"(" ++ s ++ inner ++ ")") (" "++init) !! n)
	in
		("(\\op a b->"++cycleN "zipWith" "op" extra ++ lhsRepeat ++ rhsRepeat ++")", extra)

isBaseElemChr VChr = True
isBaseElemChr (VList [e]) = isBaseElemChr e
isBaseElemChr _ = False

coerce2 :: [VT] -> [VT] -> [VT]
coerce2 [VChr] [VChr] = [VChr]
coerce2 [VInt] [VInt] = [VInt]
coerce2 [a] [b] | isNum a && isNum b = [vstr]
coerce2 [a] [VList [VChr]]
	| VChr == a || (not $ isBaseElemChr a) = [vstr]
	| otherwise = [a]
coerce2 [VList [VChr]] [a] = coerce2 [a] [vstr]
coerce2 [VList a] [b] | isNum b = [VList (coerce2 a [b])]
coerce2 [b] [VList a] | isNum b = coerce2 [VList a] [b]
coerce2 [VList a] [VList b] = [VList $ coerce2 a b]
-- definitely could do something fancier for imbalance tuple coerce, but should be very rare
coerce2 [a] (b:_) = coerce2 [a] [b]
coerce2 (a:_) [b] = coerce2 [a] [b]
coerce2 (a:as) (b:bs) = coerce2 [a] [b] ++ coerce2 as bs

-- || (baseElem a == VInt && cidim [a] == cidim [b])
coerceToH (a, b) | a==b || (isNum a && isNum b) = "(id)"
coerceToH (VList [VInt], VList [VChr]) = "(id)"
coerceToH (VList [VChr], VInt) = "(sToA.show)"
coerceToH (VInt, VList [VChr]) = "(fromMaybe 0.readMaybe.aToS)"
coerceToH (VInt, VList a) = "(sum.(map"++coerceTo [VInt] a++"))" -- questionable choice here but maybe more useful
coerceToH (VChr, VList a) = "(head."++coerceTo [VChr] a++")"

coerceToH (a, VList b) | csdim [a] < csdim [VList b] = "(concatMap"++coerceTo [a] b++")"
coerceToH (VList a, b) | baseElem (head a) == VInt && cidim [VList a] > cidim [b] = "((:[])."++coerceTo a [b]++")"
coerceToH (VList a, b) | baseElem (head a) == VChr && csdim [VList a] > csdim [b] = "((:[])."++coerceTo a [b]++")"
coerceToH (VList a, VList b) -- | csdim a == csdim b -- (sorta, not quite since difference in base types
	= "(map"++coerceTo a b++")"

coerceTo :: [VT] -> [VT] -> String
coerceTo to from = tupleLambda (length from) $ \args -> toTuple $
	zipWith3 (\t f a->"("++coerceToH(t,f)++" "++a++")") to from args ++ defaults
	where defaults = map defaultValue1 $ drop (length from) to

defaultValue1 (VInt) = "0"
defaultValue1 (VChr) = "space"
defaultValue1 (VList _) = "[]"
defaultValue = toTuple . map defaultValue1

baseElem (VList e) = baseElem $ head e
baseElem t = t

cidim [VInt] = 0
cidim [VChr] = 0
cidim [VList a] = 1+cidim a
cidim (t:_:_) = cidim [t]

sdim :: [VT] -> Int
sdim [VInt] = 1
sdim [VChr] = 0
sdim [VList a] = 1+sdim a
sdim (t:t2:ts) = 1+ maximum (map (sdim.(:[])) (t:t2:ts))

csdim [VInt] = 1
csdim [VChr] = 0
csdim [VList a] = 1+csdim a
csdim (t:_:_) = csdim [t]

-- todo consider only applying on first to guarantee it works
-- ["(+1)", "(+2)"] -> "(\(x, y) -> ((+1) x, (+2) y)"
appTuple :: [String] -> String
appTuple ops = tupleLambda (length ops) $ \varNames -> (toTuple $ zipWith (++) ops varNames)

coerce :: [VT] -> [VT] -> ([VT], String, String)
coerce leftType rightType =
	let
		coercedType = coerce2 leftType rightType
		coerceFn = coerceTo coercedType
	in (coercedType, coerceFn leftType, coerceFn rightType)

coerceEither :: [VT] -> [VT] -> ([VT], String)
coerceEither leftType rightType =
	let
		coercedType = coerce2 leftType rightType
		coerceFn = coerceTo coercedType
	in
		(coercedType, "(either "++coerceFn leftType++coerceFn rightType++")")


-- promote all arg to a list if not a list
promoteList :: [VT] -> (VT, String)
promoteList [VList t] = (VList t, " id ")
promoteList t = (VList t, "(:[])")

promoteListRepeat :: [VT] -> (VT, String)
promoteListRepeat [VList t] = (VList t, " id ")
promoteListRepeat t = (VList t, " repeat ")

flatten :: VT -> String
flatten (VList [a]) = "concat." ++ flatten a
flatten _ = "(:[])"

-- Easier would be to modify the lambda if we had that structure still
-- e.g. 2 4 = "(\\f (a1,a2) ->f (a1,a2,(),()))"
fillAccums c n = "(\\f "++parenNonNothing (intercalate","varNames)++"->f "++parenNonNothing(intercalate","rhs)++")"
	where
		varNames = varNamesN (n-c)
		rhs = varNames ++ replicate c "()"
		parenNonNothing "" = ""
		parenNonNothing a = "("++a++")"

lazyOr :: [VT] -> [VT] -> ([VT], String)
lazyOr a1 a2 = (a1, "\\a b->if "++truthy a1++" a then a else "++coerceTo a1 a2++"b")
lazyAnd :: [VT] -> [VT] -> ([VT], String)
lazyAnd a1 a2 = (a2, "\\a b->if "++truthy a1++" a then b else "++coerceTo a2 a1++"a")
