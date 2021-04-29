-- Polymorphic functions for conversion at compile time
module Polylib where

import Data.Char
import Types

truthy f (VInt _) = "("++f++"."++"(>0)"++")"


truthy2 f (VInt _) = "("++f++"."++"((>0).)"++")"

inspect (VInt False) = "(sToA.show)"
inspect (VInt True) = "(sToA.show.chr.fromIntegral)"
inspect (VList (VInt True)) = "(sToA.show.aToS)"
inspect (VList et) = "(\\v -> (sToA \"[\") ++ (intercalate (sToA \",\") (map "++inspect et++" v)) ++ (sToA \"]\"))"

-- todo this could be cleaner with join?
finish'' (VInt False) = "(aToS."++inspect int++")"
finish'' (VInt True) = "((:[]).chr.fromInteger)"
finish'' (VList (VInt True)) = "aToS"
finish'' (VList e) = "(concatMap " ++ finish'' e ++ ")"
finish' (VList (VInt True)) = finish'' str
finish' (VList e) = "(unwords . (map " ++ finish'' e ++ "))"
finish' e = finish'' e
finish (VList (VInt True)) = finish'' str
finish (VList e) = "(unlines . (map " ++ finish' e ++ "))"
finish e = finish'' e

-- finish t
-- 	| sdim t > 2 = app1 (finish (lt t)) (join 

-- join (VInt False) = ("(\\a->intercalate a $map "++inspect [int]++")", str)
join (VInt False) = (str, "(\\a b->intercalate a (map "++inspect int++" b))")
join (VList (VInt True)) = (str, "intercalate")
join (VList e) = (VList rt, "(map."++ej++")") where (rt, ej)=join e
