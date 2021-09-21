-- todo inversable parser techniques could simplify this
-- todo hide Lit to enforce whitespace always consumed
-- todo use monad for parseInt, etc
-- convention is parse consumes up until next non ignorable code
module Parse(
	intParser,
	strParser,
	chrParser,
	parseDataExpr,
	parseCountTuple,
	cp,
	tildaOp,
	onlyCheckMatch,
	match,
	parseError,
	parseLitWarning,
	nextOffset,
	consumeWhitespace,
	litDigit,
	empty,
	fromByte,
	toByte,
	parse1Nibble) where

import Expr
import Types
import Hs
import Header (at,fromBase,toBase)

import Data.Char
import Numeric (showOct)
import Data.Maybe (fromMaybe)
import State

import Text.ParserCombinators.ReadP (gather, readP_to_S)
import Text.Read.Lex as Lex (readDecP, lex, Lexeme(String), Lexeme(Char))

import Data.List

toByte :: [Int] -> Char
toByte (a:b:_)=chr $ 16 * a + b


fromByte b=[ord b `div` 16, ord b `mod` 16]

sLit :: String -> String -> Int -> Code
sLit f s cp = consumeWhitespace $ Lit f s cp

-- use as needle in match to match int digit
litDigit = ""

tildaOp = ([0],["~"])

parseInt (Nib (0:rest) cp) = (10, Nib rest (cp+1))
parseInt (Nib s cp) = (n, rest) where
	(n,rest) = parseNibInt s 0 cp
	parseNibInt [] _ _ = error "unterminated number" -- todo make last digit base 16, but this is non trivial due to padding odd nibble lengths into bytes
	parseNibInt(f:s) a cp
		| f>=8 = (c,Nib s (cp+1))
		| otherwise = parseNibInt s c (cp+1)
		where c=a*8+toInteger f`mod`8
parseInt (Lit f ('0':s) cp) = (0, sLit f s (cp+1))
-- Thanks Jon Purdy for the readP info!
parseInt (Lit f s cp) = case readP_to_S (gather readDecP) s of
	[((used, n), rest)] -> (n, sLit f rest (cp+length used))
	_ -> error $ "unparsable int: " ++ s

intToNib :: Integer -> [Int]
intToNib 10 = [0]
intToNib n = init digits ++ [last digits + 8]
	where digits = map digitToInt $ showOct n ""

parseStr :: Code -> ([String], Code)
parseStr(Nib [] cp) = error "unterminated string" -- todo make this string the new default separator instead of " ", but this is also non trivial due to padding odd nibble lengths into bytes
parseStr(Nib (a:s) cp)
	| a8==0 = cont '\n' 1
	| a8==1 = cont ' ' 1
	| c==32 = if a<8
		then ([""], Nib (tail s) (cp+2))
		else let (r2,code2) = parseStr $ Nib (drop 1 s) (cp+2)
			in ("":r2,code2)
	| c==127 = cont (toByte $ tail s) 4
	| otherwise = cont (chr c) 2
	where
		a8=a`mod`8
		c=a8*16+head s
		cont ch used = if a>=8
			then ([[ch]], after)
			else let (rest:rest2, rs)=parseStr after in ((ch:rest):rest2, rs)
			where after = Nib (drop (used-1) s) (cp+used)

parseStr (Lit f s cp) = 
	case readP_to_S (gather Lex.lex) s of
	[((used, Lex.String str), rest)] ->
		let after = sLit f rest (cp+length used) in
		case rest of
			('"':_) ->
				let (strRest, after2) = parseStr after
				in (str:strRest, after2)
			_ -> ([str], after)
	_ -> error $ "unparsable string: " ++ s

strToNib :: [String] -> [Int]
strToNib [""] = [2,0]
strToNib [s] = (concatMap (\(c,last)->let oc = ord c in case c of
	'\n' -> [last]
	' ' -> [1+last]
	c | oc > 126 || oc < 32 -> [last+7, 15, div oc 16, mod oc 16]
	_ -> [last+div oc 16, mod oc 16]
	) (zip s $ take (length s - 1) (repeat 0) ++ [8]))

strToNib ("":rest) = [10,0] ++ strToNib rest
strToNib (s:rest) =
	let nib1 = strToNib [s++" "]
	in init nib1 ++ [10,0] ++ strToNib rest

specialChars = "\n /.,`a@A0"
-- This would be good with custom ascii table
--specialChars = "\n -.,azAZ0"
specialChars127n = '\127':tail specialChars
swap127 '\n' = '\127'
swap127 '\127' = '\n'
swap127 c = c
-- 10 special chars
-- 10 first op nibbles to denote them, otherwise normal 2 nibble char
-- if you encoded a normal char that is a special char, it denotes beginning of nonprintable char (of which there are 256-96)
parseChr :: Code -> (Char, Code)
parseChr(Nib [] cp) = error "unterminated char"
parseChr(Nib (c:rest) cp)
	| c < 2 = (specialChars !! c, Nib rest (cp+1))
	| c > 7 = (specialChars !! (c-6), Nib rest (cp+1))
parseChr(Nib [_] cp) = error "unterminated char"
parseChr(Nib (a:b:rest) cp) = 
	case elemIndex c specialChars127n of
	Just i -> case uncons rest of
		Nothing -> error "unterminated binary char"
		Just (b2,rest2) -> (swap127 $ toByte [if i < 2 then i else i+6,b2], Nib rest2 (cp+3))
	Nothing -> (c, Nib rest (cp+2))
	where c = toByte [a,b]
parseChr (Lit f s cp) = case readP_to_S (gather Lex.lex) s of
	[((used, Lex.Char char), rest)] -> (char, sLit f rest (cp+length used))
	_ -> error $ "unparsable char: " ++ s

chrToNib :: Char -> [Int]
chrToNib c =
	case elemIndex c specialChars of
	Just i -> [if i < 2 then i else i + 6]
	Nothing -> if c >= chr 32 && c < chr 127
		then fromByte c
		else let [a,b] = fromByte (swap127 c)
			in fromByte (specialChars127n!!(if a<2 then a else a-6))++[b]

-- Consume rest of program as an integer (efficient binary packing)
parseData :: Code -> Integer
parseData l@(Lit _ _ _) = if empty rest then n else error "program must be empty after storing ~ integer data. See https://nibbles.golf/tutorial_minutiae.html#data " where
	(n,rest)=parseInt l
	
parseData (Nib b _) = fromBase 16 (map fromIntegral $ padSafeDat b)

parseCountTuple :: ParseState Int
parseCountTuple = do
	matched <- match tildaOp
	if matched then fmap (+1) parseCountTuple else return 0

cp (Lit _ _ cp) = cp
cp (Nib _ cp) = cp
empty (Nib [op] _) | op == uselessOp = True
empty (Nib [] _) = True
empty (Lit _ [] _) = True
empty _ = False
nextOffset (Nib (c:s) cp) = Nib s (cp+1)
nextOffset (Lit f (c:s) cp) = Lit f s (cp+1)

onlyCheckMatch :: Code -> ([Int],[String]) -> Maybe Code
onlyCheckMatch (Nib s cp) (needle, _) = if isPrefixOf needle s
	then Just $ Nib (drop (length needle) s) (cp+length needle)
	else Nothing
onlyCheckMatch lit@(Lit _ _ _) (_, []) = Just lit
onlyCheckMatch lit@(Lit _ _ _) (_, needle:s) = match1Lit lit needle >>= flip onlyCheckMatch (undefined, s)
match1Lit lit@(Lit f s cp) needle
	| null s = Nothing
	| needle == "" = if isDigit (head s) then Just lit else Nothing
	| needle == "\"" && '"' == head s = Just lit
	| needle == "\'" && '\'' == head s = Just lit
	| isPrefixOf needle s = Just $ sLit f (drop (length needle) s) (cp+length needle)
	| otherwise = Nothing

match :: ([Int],[String]) -> ParseState Bool
match (nib,lit) = do
	code <- gets pdCode
	case onlyCheckMatch code (nib,lit) of
		Just nextCode -> do
			modify $ \s -> s { pdCode = nextCode }
			appendRep (nib,concat lit)
			return True
		Nothing -> return False

generateErrorMsg msg = do
	s <- gets pdCode
	return $ msg ++ "\n" ++ case s of
		Lit f s cp -> literateError f cp
		Nib s cp -> "at nibble #" ++ show cp ++ "\nnext nibble is" ++ fromMaybe "" (at s 0>>=Just . show)

parseLitWarning :: String -> ParseState ()
parseLitWarning msg = generateErrorMsg msg >>= addLitWarning

parseError :: String -> ParseState a
parseError msg = do
	generateErrorMsg msg >>= errorWithoutStackTrace

literateError s cp =
	"at line: " ++ show lineno
		++ ", char: " ++ show (charno+1)
		++ "\n" ++ arrows ++ "v"
		++ "\n" ++ line
		++ "\n" ++ arrows ++ "^"
	where
		prev = take (cp+1) s
		lineno = length $ lines prev
		line = lines s !! (lineno-1)
		charno = (fromMaybe (cp+1) $ elemIndex '\n' $ reverse prev) - 1
		arrows = replicate charno ' '

consumeWhitespace :: Code -> Code
consumeWhitespace n@(Nib _ _) = n
consumeWhitespace l@(Lit _ [] _) = l
consumeWhitespace (Lit f (c:s) cp)
	| c=='#' = sLit f rest (cp+1+length comment)
	| isSpace c = sLit f s (cp+1)
	| otherwise = Lit f (c:s) cp
		where (comment, rest) = break (=='\n') s

intParser :: ParseState (VT, String)
intParser = do
	(n, rest) <- gets $ parseInt . pdCode
	modify $ \st -> st { pdCode=rest }
	genLit <- gets pdLit
	appendRep (intToNib n," "++show n) -- only need to prepend " " if last was digit, but that would be expensive to check with dlist data structure
	return (VInt, flatHs $ i n)

strParser :: ParseState (VT,String)
strParser = do
	(ss, rest) <- gets $ parseStr . pdCode
	modify $ \st -> st { pdCode=rest }
	appendRep (strToNib ss, tail $ concat $ map show ss)
	
	return $ case ss of
		[s] -> (vstr, "sToA " ++ show s)
		_ -> (VList [vstr], "map sToA " ++ show ss)

chrParser :: ParseState (VT,String)
chrParser = do
	(s, rest) <- gets $ parseChr . pdCode
	modify $ \st -> st { pdCode=rest }
	appendRep (chrToNib s,tail $ show s)
	return (VChr, "myOrd " ++ show s)

parseDataExpr :: ParseState Integer
parseDataExpr = do
	dat <- gets $ parseData . pdCode
	let datNibs = map fromIntegral (toBase 16 dat)
	progNibs <- gets pdNib
	appendRep (padSafeDat datNibs,show dat)
	return dat

-- reverse (so leading digit is last) and swap useless op with 0 so that when it is padded to even nibbles the value isn't changed
padSafeDat = reverse . map (\e ->
	if e == uselessOp then 0
	else if e == 0 then uselessOp
	else e)

parse1Nibble :: String -> [(Int, (Char, ParseState a))] -> State ParseData a
parse1Nibble name [] = parseError $ "symbol not found in " ++ name
parse1Nibble name ((nib,(lit,impl)):rest) = do
	match <- match ([nib],[[lit]])
	if match then impl else parse1Nibble name rest
