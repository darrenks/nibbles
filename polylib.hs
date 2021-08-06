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
	uncurryN,
	curryN,
	flattenTuples,
	promoteList,
	appTuple,
	flatten) where

import Types
import Data.List

truthy VInt = "(>0)"
truthy VChr = "(not.isSpace.chr)"
truthy (VList _) = "(not.null)"

inspect VInt = "(sToA.show)"
inspect VChr = "(sToA.show.chr.fromIntegral)"
inspect (VList [VChr]) = "(sToA.show.aToS)"
-- inspect (VList [t]) = "(sToA.show.aToS)"
inspect (VList et) = "(\\v -> (sToA \"[\") ++ (intercalate (sToA \",\") (map "++inspectElem et++" v)) ++ (sToA \"]\"))"
inspect a = error $ show a
inspectElem [et] = inspect et
-- inspectElem [et1,et2] = "(\\(a1,a2)->sToA \"(\" ++"++inspect et1++"a1++sToA \",\"++" ++ inspect et2 ++ "a2 ++ sToA \")\")"
inspectElem ts = "(\\("++intercalate "," varNames++")->sToA \"(\"++"++
	(intercalate "++sToA \",\"++" $ zipWith (\t v->inspect t ++ v) ts varNames)
	++"++sToA \")\")" where
	varNames = map (\tn -> "a"++show tn) [1..length ts]


uncurryN n = "(\\f z->let "++recParen (reverse varNames)++"=z in f "++intercalate " " varNames ++ ")"
	where
		recParen [a] = a
		recParen (a:as) = "("++recParen as++","++a++")"
		varNames = map (\tn -> "a"++show tn) [1..n]
-- 
-- uncurryN n = "(\\f ("++intercalate "," varNames++")->f "++intercalate " " varNames ++ ")"
-- 	where varNames = map (\tn -> "a"++show tn) [1..n]
curryN n = "(\\f "++intercalate " " varNames++"->f ("++intercalate "," varNames ++ "))"
	where varNames = map (\tn -> "a"++show tn) [1..n]

flattenTuples :: Int -> Int -> [Char]
flattenTuples t1 t2 = "(\\(("++varsFrom 1 t1++"),"++varsFrom (1+t1) (t1+t2)++")->("++varsFrom 1 (t1+t2)++"))"
	where varsFrom a b = intercalate "," $ map (\tn->"a"++show tn) [a..b]

finishH :: VT -> String
finishH (VList tt)
	| d >= 3 = joinC "[]"
	| d == 2 = joinC "[32]"
	| d == 1 = compose1 "(++[10])" $ joinC "[10]" -- todo might not want that newline for empty list? like unlines
	where
		t = todoAssumeFst tt
		d = sdim t
		joinC s = compose1 (finishH jt) $ app1 js s where (jt,js) = join [t] -- todo
finishH (VList [VChr]) = "(id)"
finishH VInt = inspect VInt
finishH VChr = "(:[])"
-- finishH a = error $ show a
finish = finishH
-- = composez finishH removePairs

-- composez a b t = compose1 (a t2) s where (t2, s) = b t
compose1 a b = "(" ++ a ++ "." ++ b ++ ")"
app1 a b = "(" ++ a ++ b ++ ")"

-- removePairs (VPair a b) = (a, "(fst)")
-- removePairs (VList a) = (VList rt, app1 "map" rs) where (rt, rs) = removePairs a
-- removePairs a = (a, "(id)")

-- todo
join [VInt] = (vstr, "(\\a b->intercalate a (map "++inspect VInt++" b))")
join [VList [VChr]] = (vstr, "intercalate")
join [VList e] = (VList [rt], "(map."++ej++")") where (rt, ej)=join e

vectorize :: String -> ([VT] -> VT) -> [VT] -> (VT, String)
vectorize op rtf [t1, VList t2] = (VList [rt], rop) where
	(rt, rop) = vectorize ("(\\a1->map ("++op++" a1))") rtf [t1, todoAssumeFst t2]
vectorize op rtf [t1, t2] = (rtf [t1, t2], op)

baseElem (VList e) = baseElem $ todoAssumeFst e
baseElem t = t

coerce2 :: (VT, VT) -> VT
coerce2(VChr, VChr) = VChr
coerce2(a, b) | isNum a && isNum b = VInt
coerce2(a, VList [VChr])
	| VChr == a || (VInt == baseElem a) = vstr
	| otherwise = a
coerce2(VList [VChr], a) = coerce2(a, vstr)
coerce2(VList a, b) | isNum b = VList [coerce2(todoAssumeFst a, b)]
coerce2(b, VList a) | isNum b = coerce2(VList a, b)
coerce2(VList a, VList b) = VList [coerce2(todoAssumeFst a, todoAssumeFst b)]

coerceToH (a, b) | a==b || (baseElem a == VInt && dim a == dim b) || (isNum a && isNum b) = "(id)"
coerceToH (VList [VChr], VInt) = "(sToA.show)"
coerceToH (VInt, VList [VChr]) = "((fromMaybe 0).readMaybe.aToS)"
coerceToH (VInt, VList a) = "(sum.(map"++coerceToH(VInt,todoAssumeFst a)++"))"
-- coerceTo (VList VInt, VList VChr) = "(id)"
coerceToH (VChr, VList a) = "(head."++coerceToH(VChr,todoAssumeFst a)++")"
coerceToH (VList a, b) | sdim (VList a) > sdim b = "((:[])."++coerceToH(todoAssumeFst a, b)++")"
coerceToH (VList a, VList b) | sdim (todoAssumeFst a) == sdim (todoAssumeFst b) = "(map"++coerceToH(todoAssumeFst a, todoAssumeFst b)++")"
coerceToH (a, VList b) | sdim a < sdim (VList b) = "(concatMap"++coerceToH(a, todoAssumeFst b)++")"

coerceTo :: [VT] -> [VT] -> String
coerceTo to from = appTuple (zipWith (curry coerceToH) to from)

dim VInt = 1
dim VChr = 1
dim (VList a) = 1+dim (todoAssumeFst a)

sdim VInt = 1
sdim VChr = 0
sdim (VList a) = 1+sdim (todoAssumeFst a)

-- ["(+1)", "(+2)"] -> "(\(x, y) -> ((+1) x, (+2) y)"
appTuple :: [String] -> String
appTuple ops = "(\\"++recParen (reverse varNames)++"->"++recParen (reverse $ zipWith (++) ops varNames)++")"
	where
		n = length ops
		recParen [a] = a -- todo reuse this above and add in reverse
		recParen (a:as) = "("++recParen as++","++a++")"
		varNames = map (\tn -> "a"++show tn) [1..n]

coerce :: [VT] -> [VT] -> ([VT], String, String)
coerce leftType rightType =
	let
		coercedType = zipWith (curry coerce2) leftType rightType
		coerceFn = coerceTo coercedType
	in (coercedType, coerceFn leftType, coerceFn rightType)

coerceEither :: [VT] -> [VT] -> ([VT], String)
coerceEither leftType rightType =
	let
		coercedType = zipWith (curry coerce2) leftType rightType
		coerceFn = coerceTo coercedType
	in
		(coercedType, "(either "++coerceFn leftType++coerceFn rightType++")")


-- promote all arg to a list if not a list
promoteList :: VT -> (VT, String)
promoteList (VList t) = (VList t, " id ")
promoteList t = (VList [t], "(:[])")

flatten :: VT -> String
flatten (VList [a]) = "concat." ++ flatten a
flatten _ = "(:[])"
