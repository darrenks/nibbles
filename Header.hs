module Header where

import Data.List
import Data.Char (chr,ord,isAlpha,isDigit,isSpace)
import Data.Maybe (fromMaybe,catMaybes)
import Data.Tuple (swap)
import Data.List.Split -- needs cabal install --lib split
import Text.Read (readMaybe)
import Data.Function (fix)
import System.IO

-- for Hash
import qualified Data.ByteString.Lazy as B8
import Data.Digest.Pure.MD5 -- needs cabal install --lib pureMD5
import qualified Data.ByteString.Char8 as C8

sToA :: String -> [Integer]
sToA = map (fromIntegral.ord)

aToS :: Integral i => [i] -> String
aToS = map$chr.fromIntegral

bToI :: Bool -> Integer
bToI b = if b then 1 else 0

-- Check these types so we don't accidentally not catch type errors in tests which rely on show
confirmInt :: Integral a => a -> a
confirmInt = id
confirmList :: [a] -> [a]
confirmList = id

at :: Integral i => [a] -> i -> Maybe a
at [] _ = Nothing
at (x:xs) n
 | n < 0     = Nothing
 | n == 0    = Just x
 | otherwise = at xs (n-1)

step :: Integral i => i -> [a] -> [a]
step n a = map fst $ filter ((==0).(`mod`n).snd) (zip (if n<0 then reverse a else a) [0..])

-- returns [(split by, non split by)], remaining split by
mySplitWhen :: Eq a => (a -> Bool) -> [a] -> ([([a],[a])],[a])
mySplitWhen f a =
	let r = chunksOf 2 $ (split.condense.whenElt) (not.f) a		
	in (map (\[a,b]->(a,b)) (init r), head $ last r)

iff :: Bool -> a -> b -> Either a b
iff c b1 b2 = if c then Left b1 else Right b2

finishLn :: String -> String
finishLn "\n" = "\n"
finishLn "" = "\n"
finishLn (x:xs) = x:finishLn xs

lazyAtMod :: Integral i => [a] -> i -> a
lazyAtMod a i = fromMaybe
 (a!!fromIntegral(i`mod`fromIntegral (length a)))
 (at a i)

asInts :: [Integer] -> [Integer]
asInts = map (read::String->Integer) . asIntsH . aToS
asIntsH :: String -> [String]
asIntsH "" = []
asIntsH "-" = []
asIntsH ('-':c:rest)
	| isDigit c = ('-':c:num) : asIntsH after
	| otherwise = asIntsH rest
	where
		(num,after) = span isDigit rest
asIntsH (c:rest)
	| isDigit c = (c:num) : asIntsH after
	| otherwise = asIntsH rest
	where
		(num,after) = span isDigit rest

fromBase :: (Integral a, Integral b) => b -> [a] -> Integer
fromBase b a = foldl (\x y->x*(toInteger b)+(toInteger y)) (0::Integer) a
-- toBase _ 0 = [0]
toBase :: (Integral a, Integral b) => b -> Integer -> [a]
toBase b n = reverse $ map (fromIntegral.flip mod b) $ takeWhile (>0) $ iterate (flip div b) (fromIntegral n)

hlist :: Integral i => [i] -> Integer
hlist a = fromBase 256 $ map (fromIntegral.ord) $ C8.unpack $ md5DigestBytes $ md5 $ B8.pack $ map fromIntegral $ concatMap (toBase 256) (map fromIntegral a)

listOr :: [a] -> [a] -> [a]
listOr defaultResult [] = defaultResult
listOr _ nonEmpty = nonEmpty
