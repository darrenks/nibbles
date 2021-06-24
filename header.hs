module Header where

import Data.List
import Data.Char (chr,ord,isAlpha,isDigit)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.List.Split (splitOn,splitWhen) -- needs cabal install --lib split
import Text.Read (readMaybe)
import Data.Function (fix)
import System.IO

safeChr = chr.(`mod`256)
sToA = map ord
aToS = map$safeChr.fromIntegral
bToI b = if b then 1 else 0

at :: Integral i => [a] -> i -> Maybe a
at [] _ = Nothing
at (x:xs) n
 | n < 0     = Nothing
 | n == 0    = Just x
 | otherwise = at xs (n-1)

step n a = map fst $ filter ((==0).(`mod`n).snd) (zip a [0..])
reshape n a = takeWhile (not.null) $ unfoldr (Just.(splitAt n)) a

iff :: Bool -> a -> a -> a
iff c b1 b2 = if c then b1 else b2

finishLn "\n" = "\n"
finishLn "" = "\n"
finishLn (x:xs) = x:finishLn xs

lazyAtMod :: [a] -> Integer -> a
lazyAtMod a i = fromMaybe
 (a!!fromIntegral(i`mod`fromIntegral (length a)))
 (at a i)