module Stdlib where

import Data.List
import Data.Char

--- STDLIB
step n a = map fst $ filter ((==0).(`mod`n).snd) (zip a [0..])
reshape n [] = []
reshape n a = row : (reshape n rest) where (row, rest) = splitAt n a

class Truthy t where truthy :: t -> Bool
instance Truthy Integer where truthy = (>0)
instance Truthy [t] where truthy = not.null


iff :: Truthy t => t -> a -> a -> a
iff c b1 b2 = if truthy c then b1 else b2
-- todo should char have a different truthy? like is not space?

-- STDLIB w type todo move to another file

data VT = VInt Bool | VList VT | VPair VT VT | VMaybe (Maybe VT) deriving (Show, Eq)
-- nil = VMaybe Nothing

vint = VInt False
vchr = VInt True
vstr = VList vchr

sToA = (map$toInteger.ord)
aToS = (map$chr.fromInteger)

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

join (VInt False) = ("(\\a b->intercalate a $map "++inspect vint++" b)", const $ vstr)
join (VList (VInt True)) = ("intercalate", const $ vstr)
join (VList e) = ("(map."++ej++")", const $ VList (rt undefined)) where (ej,rt)=join e
