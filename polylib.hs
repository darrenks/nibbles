-- Polymorphic functions for conversion at compile time
module Polylib where

import Data.Char

data VT = VInt Bool | VList VT | VPair VT VT | VMaybe (Maybe VT) deriving (Show, Eq)
-- nil = VMaybe Nothing

vint = VInt False
vchr = VInt True
vstr = VList vchr

inspect (VInt False) = "(sToA.show)"
inspect (VInt True) = "(sToA.show.chr.fromInteger)"
inspect (VList (VInt True)) = "(sToA.show.aToS)"
inspect (VList et) = "(\\v -> (sToA \"[\") ++ (intercalate (sToA \",\") (map "++inspect et++" v)) ++ (sToA \"]\"))"

-- todo this could be cleaner with join?
finish'' (VInt False) = "(aToS."++inspect vint++")"
finish'' (VInt True) = "((:[]).chr.fromInteger)"
finish'' (VList (VInt True)) = "aToS"
finish'' (VList e) = "(concatMap " ++ finish'' e ++ ")"
finish' (VList (VInt True)) = finish'' vstr
finish' (VList e) = "(unwords . (map " ++ finish'' e ++ "))"
finish' e = finish'' e
finish (VList (VInt True)) = finish'' vstr
finish (VList e) = "(unlines . (map " ++ finish' e ++ "))"
finish e = finish'' e

-- finish t
-- 	| sdim t > 2 = app1 (finish (lt t)) (join 

join (VInt False) = ("(\\a->intercalate a $map "++inspect vint++")", const $ vstr)
join (VList (VInt True)) = ("intercalate", const $ vstr)
join (VList e) = ("(map."++ej++")", const $ VList (rt undefined)) where (ej,rt)=join e
