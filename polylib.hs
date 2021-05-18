-- Polymorphic functions for conversion at compile time
module Polylib where

import Data.Char
import Types

truthy VInt = "(>0)"
truthy VChr = "(>0)"
truthy (VList _) = "(not null)"

inspect VInt = "(sToA.show)"
inspect VChr = "(sToA.show.chr.fromIntegral)"
inspect (VList VChr) = "(sToA.show.aToS)"
inspect (VList et) = "(\\v -> (sToA \"[\") ++ (intercalate (sToA \",\") (map "++inspect et++" v)) ++ (sToA \"]\"))"

-- todo this could be cleaner with join?
finish'' VInt = "(aToS."++inspect VInt++")"
finish'' VChr = "((:[]).chr.fromIntegral)"
finish'' (VList VChr) = "aToS"
finish'' (VList e) = "(concatMap " ++ finish'' e ++ ")"
finish' (VList VChr) = finish'' vstr
finish' (VList e) = "(unwords . (map " ++ finish'' e ++ "))"
finish' e = finish'' e
finish (VList VChr) = finish'' vstr
finish (VList e) = "(unlines . (map " ++ finish' e ++ "))"
finish e = finish'' e

-- finish t
-- 	| sdim t > 2 = app1 (finish (lt t)) (join 

join VInt = (vstr, "(\\a b->intercalate a (map "++inspect VInt++" b))")
join (VList VChr) = (vstr, "intercalate")
join (VList e) = (VList rt, "(map."++ej++")") where (rt, ej)=join e

vectorize :: String -> ([VT] -> VT) -> [VT] -> (VT, String)
vectorize op rtf [t1, VList t2] = (VList rt, rop) where
	(rt, rop) = vectorize ("(\\a1->map ("++op++" a1))") rtf [t1, t2]
vectorize op rtf [t1, t2] = (rtf [t1, t2], op)

coerce2 :: (VT, VT) -> VT
coerce2(NoType, b) = b -- not used yet todo
coerce2(VInt, VInt) = VInt
coerce2(a, b) | isNum a && isNum b = VChr -- actually I think Int would be better
coerce2(a, VList VChr) | isNum a = vstr
coerce2(VList VChr, a) | isNum a = vstr
coerce2(VList a, b) | isNum b = VList $ coerce2(a, b)
coerce2(b, VList a) | isNum b = coerce2(VList a, b)
coerce2(VList a, VList b) = VList $ coerce2(a, b)

coerceTo (VList VChr, VInt) = "(sToA.show)"
coerceTo (VList a, VInt) = "(\\x->[x])"
--todo need more, think through
coerceTo _ = ""

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
	