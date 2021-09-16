module Header where

import Data.Function.Memoize -- needs cabal install --lib memoize
import Data.List
import Data.Char (chr,ord,isAlpha,isDigit,isSpace,toLower)
import Numeric (showHex)
import Data.Maybe (fromMaybe,catMaybes,isJust)
import Data.Tuple (swap)
import Data.Bits
import Data.List.Split -- needs cabal install --lib split
import Text.Read (readMaybe)
import Data.Function (fix)
import System.IO
import qualified Data.Set as Set

-- for Hash
import qualified Data.ByteString.Lazy as B8
import Data.Digest.Pure.MD5 -- needs cabal install --lib pureMD5
import qualified Data.ByteString.Char8 as C8

charList = ' ':['a'..'z']++".,!?_\n"++['A'..'Z']++['0'..'9']
	++"-+:;\"'~`@#$%^&*()[]{}<>\\/=|"
	++['\0'..'\9']++"\127"++['\11'..'\31']
inverseCharList :: [Integer]
inverseCharList = map fromIntegral $ catMaybes $ map (flip elemIndex charList) ['\0'..'\127']
newli = inverseCharList !! ord '\n'
space = inverseCharList !! ord ' '

myChr :: Integral i => i -> Char
myChr i | i < 0 = myChr $ i `mod` 96
        | i < 128 = charList !! fromIntegral i
        | otherwise = chr $ fromIntegral i
myOrd :: Char -> Integer
myOrd c | c < '\128' =  fromIntegral $ inverseCharList !! ord c
        | otherwise = fromIntegral $ ord c

sToA :: String -> [Integer]
sToA = map (fromIntegral.myOrd)

aToS :: Integral i => [i] -> String
aToS = map$myChr.fromIntegral

bToI :: Bool -> Integer
bToI b = if b then 1 else 0

onToBy :: Eq a => (t -> a) -> t -> t -> Bool
onToBy f x y = f x == f y

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
hlist a = fromBase 256 $ map (fromIntegral.ord) $ C8.unpack $ md5DigestBytes $ md5 $ B8.pack $ map fromIntegral $ concatMap (toBase 256) (map (\e->fromIntegral$ord$myChr e) a)

listOr :: [a] -> [a] -> [a]
listOr defaultResult [] = defaultResult
listOr _ nonEmpty = nonEmpty

parseNum :: [Integer] -> Integer -> Integer
parseNum strI base = if base > 36 then error "parseNum base must be <= 36" else
	case findIndex isJust digitIndices of
		Just i -> let value = fromBase base $ catMaybes $ takeWhile isJust $ drop i digitIndices
		              sign = if isSuffixOf "-" (take i str) then -1 else 1 in 
		              sign * value
		otherwise -> 0
	where
		str = map toLower $ aToS strI
		digits = genericTake base $ ['0'..'9']++['a'..'z']
		digitIndices = map (flip elemIndex digits) str

iterateWhileUniq :: Ord a => (a -> a) -> a -> ([a], [a])
iterateWhileUniq f i =
	let uniqPart = iterateWhileUniqH Set.empty f i
	    Just repeatedIndex = elemIndex (f (last uniqPart)) uniqPart
	in (uniqPart, drop repeatedIndex uniqPart)
iterateWhileUniqH been f i =
	if Set.member i been then []
	else i : iterateWhileUniqH (Set.insert i been) f (f i)

