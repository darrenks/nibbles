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
