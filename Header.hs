module Header where

import Data.Function.Memoize -- needs cabal install --lib memoize
import Data.List
import Data.Char
import Numeric (showHex)
import Data.Maybe (fromMaybe,catMaybes,isJust)
import Data.Tuple (swap)
import Data.Bits
import Data.List.Split -- needs cabal install --lib split
import Text.Read (readMaybe)
import Data.Function (fix)
import System.IO
import qualified Data.Set as Set
import System.Environment

-- for Hash
import qualified Data.ByteString as B8
import qualified Data.Digest.Murmur64 as Murmur -- needs cabal install --lib murmur-hash

newli = myOrd '\n'
space = myOrd ' '

printables = sToA $ ' ':['a'..'z']++".,!?_\n"++['A'..'Z']++['0'..'9']
	++"-+:;\"'~`@#$%^&*()[]{}<>\\/=|"
unprintables = filter (\c->not$elem c printables) $ sToA ['\0'..'\127']

myChr :: Integral i => i -> Char
myChr i = chr $ fromIntegral i
myOrd :: Char -> Integer
myOrd c = fromIntegral $ ord c

sToA :: String -> [Integer]
sToA = map (fromIntegral.myOrd)

aToS :: Integral i => [i] -> String
aToS = map$myChr.fromIntegral

bToI :: Bool -> Integer
bToI b = if b then 1 else 0

onToBy :: Eq a => (t -> a) -> t -> t -> Bool
onToBy f x y = f x == f y

-- onToOrderBy :: Ord a => (t -> a) -> t -> t -> Ordering
-- onToOrderBy f x y = compare (f x) (f y)
-- onToSelectBy :: Ord a => (t -> a) -> t -> t -> Ordering
onToSelectBy op f x y = if op (f x) (f y) then x else y

safeDiv :: Integer -> Integer -> Integer
safeDiv a 0 = signum a*(2^128)
safeDiv a b = a `div` b
safeMod :: Integer -> Integer -> Integer
safeMod a 0 = 0
safeMod a b = a `mod` b
safeDivMod :: Integer -> Integer -> (Integer,Integer)
safeDivMod a b = (safeDiv a b, safeMod a b)

-- Check these types so we don't accidentally not catch type errors in tests which rely on show
confirmInt :: Integer -> Integer
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
hlist a = fromIntegral $ Murmur.asWord64 $ Murmur.hash64 $ B8.pack $ map fromIntegral $ concatMap (toBase 256)
	 -- (map (\e->fromIntegral$ord$myChr e) a)
	 (map fromIntegral a)

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

appendUntilNull :: [a] -> ([a] -> [a]) -> [a]
-- appendUntilNull a f = a ++ appendUntilNullH a f
-- appendUntilNullH a f = case f a of
-- 	[] -> []
-- 	r -> r ++ appendUntilNullH (a++r) f
-- appendUntilNull a f = case f a of
-- 	[] -> reverse a
-- 	r -> appendUntilNull (reverse r++a) f
appendUntilNull a f = a ++ appendUntilNullH (reverse a) f
appendUntilNullH a f = case f a of
	[] -> []
	r -> r ++ appendUntilNullH (reverse r++a) f


iterateWhileUniq :: Ord a => (a -> a) -> a -> [a]
iterateWhileUniq f i =
	let uniqPart = iterateWhileUniqH Set.empty f i
	    Just repeatedIndex = elemIndex (f (last uniqPart)) uniqPart
	in (uniqPart) --, drop repeatedIndex uniqPart)
iterateWhileUniqH been f i =
	if Set.member i been then []
	else i : iterateWhileUniqH (Set.insert i been) f (f i)

subsequencesN :: Integral i => i -> [a] -> [[a]]
subsequencesN _ []     = [[]]
subsequencesN 0 _      = [[]]
subsequencesN n xs | n >= genericLength xs = [xs]
subsequencesN n (x:xs) = (map (x:) $ subsequencesN (n-1) xs) ++ subsequencesN n xs

repeatedSubsequencesN :: Integral i => i -> [a] -> [[a]]
repeatedSubsequencesN _ []     = []
repeatedSubsequencesN 1 xs      = map (:[]) xs
repeatedSubsequencesN n (x:xs) = (map (x:) $ repeatedSubsequencesN (n-1) (x:xs)) ++ repeatedSubsequencesN n xs

nthRoot :: Integer -> Integer -> Integer
nthRoot n 0 = 0
nthRoot n x = nthRootGuess n x x
nthRootGuess n guess x = case ((x `div` guess^(n-1)) + guess*(n-1)) `div` n of
	r | r>=guess -> guess
	  | otherwise -> nthRootGuess n r x

-- from https://stackoverflow.com/questions/40097116/get-all-permutations-of-a-list-in-haskell
permutationsSaneOrder :: Eq a => [a] -> [[a]]
permutationsSaneOrder [] = [[]]
permutationsSaneOrder as = do
	a <- as
	let l = delete a as
	ls <- permutationsSaneOrder l
	return $ a : ls
