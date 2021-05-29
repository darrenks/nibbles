-- Polymorphic functions for conversion at compile time
module Polylib(
	truthy,
	inspect,
	finish,
	join,
	vectorize,
	coerce,
	coerce2, coerceTo, -- test only
	composeOp,
	promoteList) where

import Data.Char
import Types

truthy VInt = "(>0)"
truthy VChr = "(>0)"
truthy (VList _) = "(not null)"

inspect VInt = "(sToA.show)"
inspect VChr = "(sToA.show.chr.fromIntegral)"
inspect (VList VChr) = "(sToA.show.aToS)"
inspect (VList et) = "(\\v -> (sToA \"[\") ++ (intercalate (sToA \",\") (map "++inspect et++" v)) ++ (sToA \"]\"))"

finish :: VT -> String
finish (VList t)
	| d >= 3 = joinC "[]"
	| d == 2 = joinC "[32]"
	| d == 1 = compose1 "(++[10])" $ joinC "[10]"
	where
		d = sdim t
		joinC s = compose1 (finish jt) $ app1 js s where (jt,js) = join t
		compose1 a b = "(" ++ a ++ "." ++ b ++ ")"
		app1 a b = "(" ++ a ++ b ++ ")"
finish (VList VChr) = "(id)"
finish VInt = inspect VInt
finish VChr = "(:[])"

join VInt = (vstr, "(\\a b->intercalate a (map "++inspect VInt++" b))")
join (VList VChr) = (vstr, "intercalate")
join (VList e) = (VList rt, "(map."++ej++")") where (rt, ej)=join e

vectorize :: String -> ([VT] -> VT) -> [VT] -> (VT, String)
vectorize op rtf [t1, VList t2] = (VList rt, rop) where
	(rt, rop) = vectorize ("(\\a1->map ("++op++" a1))") rtf [t1, t2]
vectorize op rtf [t1, t2] = (rtf [t1, t2], op)

coerce2 :: (VT, VT) -> VT
coerce2(VChr, VChr) = VChr
coerce2(a, b) | isNum a && isNum b = VInt
coerce2(a, VList VChr)
	| VChr == a || (VInt == baseElem a) = vstr
	| otherwise = a
coerce2(VList VChr, a) = coerce2(a, VList VChr)
coerce2(VList a, b) | isNum b = VList $ coerce2(a, b)
coerce2(b, VList a) | isNum b = coerce2(VList a, b)
coerce2(VList a, VList b) = VList $ coerce2(a, b)

coerceTo (a, b) | a==b || (baseElem a == VInt && dim a == dim b) || (isNum a && isNum b) = "(id)"
coerceTo (VList VChr, VInt) = "(sToA.show)"
coerceTo (VInt, VList VChr) = "((fromMaybe 0).readMaybe.aToS)"
coerceTo (VInt, VList a) = "(sum.(map"++coerceTo(VInt,a)++"))"
-- coerceTo (VList VInt, VList VChr) = "(id)"
coerceTo (VChr, VList a) = "(head."++coerceTo(VChr,a)++")"
coerceTo (VList a, b) | sdim (VList a) > sdim b = "((:[])."++coerceTo(a, b)++")"
coerceTo (VList a, VList b) | sdim a == sdim b = "(map"++coerceTo(a, b)++")"
coerceTo (a, VList b) | sdim a < sdim (VList b) = "(concatMap"++coerceTo(a, b)++")"

dim VInt = 1
dim VChr = 1
dim (VList a) = 1+dim a

sdim VInt = 1
sdim VChr = 0
sdim (VList a) = 1+sdim a

coerce :: String -> [Int] -> (VT -> VT) -> [VT] -> (VT, String)
coerce op coerceArgIndices rtf vts = (rtf coercedType, rop) where
	coercedType = foldr1 (curry coerce2) [vts!!j | j <- coerceArgIndices]
	r = [0..length vts-1]
	rop = "(\\" ++ concatMap ((" c"++).show) r ++ " -> " ++ op ++ concatMap (\n->"("++ctn n++"c"++show n++")") r ++")"
	ctn n
		| elem n coerceArgIndices = coerceTo (coercedType, vts !! n)
		| otherwise = ""

-- not a true compose as assumes return type of b is just the one thing (same for all)
composeOp :: (String -> VT -> (VT, String)) -> ([VT] -> (VT, String)) -> [VT] -> (VT, String)
composeOp a b vts = a op rts where
	(rts, op) = b vts

-- promote all args to lists from a type t
-- todo hardcoded to be 2 args
promoteList :: String -> VT -> (VT, String)
promoteList op (VList t) = (VList t, op)
promoteList op t = (VList t, "(\\x y->"++op++" [x] [y])")
	