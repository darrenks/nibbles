module Header where

import Data.List
import Data.Char
import Data.Maybe
import Data.Tuple
import Data.List.Split -- needs cabal install -lib split
import Text.Read (readMaybe)
import Types -- only for test

safeChr = chr.(`mod`256)
sToA = map ord
aToS = map$safeChr.fromIntegral
bToI b = if b then 1 else 0

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:xs) n
 | n < 0     = Nothing
 | n == 0    = Just x
 | otherwise = at xs (n-1)

step n a = map fst $ filter ((==0).(`mod`n).snd) (zip a [0..])
-- todo unused, use split lib
reshape n a = takeWhile (not.null) $ unfoldr (Just.(splitAt n)) a

iff :: Bool -> a -> a -> a
iff c b1 b2 = if c then b1 else b2
-- todo should char have a different truthy? like is not space?
