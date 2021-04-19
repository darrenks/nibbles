module Header where

import Data.List
import Data.Char
import Data.Maybe
import Data.List.Split -- needs cabal install -lib split


sToA = map$toInteger.ord
aToS = map$chr.fromInteger

step n a = map fst $ filter ((==0).(`mod`n).snd) (zip a [0..])
reshape n [] = []
reshape n a = row : (reshape n rest) where (row, rest) = splitAt n a

class Truthy t where truthy :: t -> Bool
instance Truthy Integer where truthy = (>0)
instance Truthy [t] where truthy = not.null


iff :: Truthy t => t -> a -> a -> a
iff c b1 b2 = if truthy c then b1 else b2
-- todo should char have a different truthy? like is not space?
