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
	parse1Nibble,
	parserToImpl,
	cp,
	match,
	parseError,
	nextOffset,
	consumeWhitespace,
	litDigit,
	empty,
	fromByte,
	toByte) where

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

parseInt (Nib (0:rest) cp) = (10, Nib rest (cp+1))
parseInt (Nib s cp) = (n, rest) where
	(n,rest) = parseNibInt s 0 cp
	parseNibInt [] _ _ = error "unterminated number" -- todo auto range map (or add 0)
	parseNibInt(f:s) a cp
		| f>=8 = (c,Nib s (cp+1))
		| otherwise = parseNibInt s c (cp+1)
		where c=a*8+toInteger f`mod`8
parseInt (Lit f ('0':s) cp) = (0, sLit f s (cp+1))
-- Thanks Jon Purdy for the readP info!
parseInt (Lit f s cp) = case readP_to_S (gather readDecP) s of
	[((used, n), rest)] -> (n, sLit f rest (cp+length used))
	_ -> error $ "unparsable int: " ++ s

parseStr(Nib [] cp) = error "unterminated string" -- todo auto join (or add newline)
parseStr(Nib (a:s) cp)
	| a8==0 = cont '\n' 1
	| a8==1 = cont ' ' 1
	| c==32 = ("", Nib (tail s) (cp+2))
	| c==127 = cont (toByte $ tail s) 4
	| otherwise = cont (chr c) 2
	where
		a8=a`mod`8
		c=a8*16+head s
		cont ch used = if a>=8
			then ([ch], after)
			else case parseStr after of (rest, rs) -> (ch:rest, rs)
			where after = Nib (drop (used-1) s) (cp+used)
parseStr (Lit f s cp) = case readP_to_S (gather Lex.lex) s of
	[((used, Lex.String str), rest)] -> (str, sLit f rest (cp+length used))
	_ -> error $ "unparsable string: " ++ s

specialChars = " \n,.-0a" -- todo more (A)
parseChr :: Code -> (Char, Code)
parseChr(Nib [] cp) = error "unterminated char"
parseChr(Nib (c:rest) cp)
	| c == 8 = (toByte rest, Nib (drop 2 rest) (cp+3))
	| c > 8 = (specialChars !! (c-9), Nib rest (cp+1))
parseChr(Nib [_] cp) = error "unterminated char"
parseChr(Nib (a:b:rest) cp) = (toByte [a,b], Nib rest (cp+2))
parseChr (Lit f s cp) = case readP_to_S (gather Lex.lex) s of
	[((used, Lex.Char char), rest)] -> (char, sLit f rest (cp+length used))
	_ -> error $ "unparsable char: " ++ s

-- Consume rest of program as an integer (efficient binary packing)
parseData :: Code -> Integer
parseData l@(Lit _ _ _) = if empty rest then n else error "program must be empty after storing ~ integer data. See https://nibbles.golf/tutorial_minutiae.html#data " where
	(n,rest)=parseInt l
	
parseData (Nib b _) = fromBase 16 (map fromIntegral $ padSafeDat b)

parseCountTuple :: ParseState Int
parseCountTuple = do
	code <- gets pdCode
	case code of
		(Nib (0:rest) cp) -> do
			modify $ \st -> st { pdCode=Nib rest (cp+1) }
			doAppendRep 
			parseCountTuple >>= return.(+1)
		(Lit f ('~':rest) cp) -> do
			modify $ \st -> st { pdCode=sLit f rest (cp+1) }
			doAppendRep 
			parseCountTuple >>= return.(+1)
		_ -> return 0
	where doAppendRep = appendRep ([0],"~")

cp (Lit _ _ cp) = cp
cp (Nib _ cp) = cp
empty (Nib [op] _) | op == uselessOp = True
empty (Nib [] _) = True
empty (Lit _ [] _) = True
empty _ = False
nextOffset (Nib (c:s) cp) = Nib s (cp+1)
nextOffset (Lit f (c:s) cp) = Lit f s (cp+1)

match :: Code -> ([String], [Int]) -> Maybe Code
match (Nib s cp) (_, needle) = if isPrefixOf needle s
	then Just $ Nib (drop (length needle) s) (cp+length needle)
	else Nothing
match lit@(Lit _ _ _) ([], _) = Just lit
match lit@(Lit _ _ _) (needle:s, _) = match1Lit lit needle >>= flip match (s, undefined)
match1Lit lit@(Lit f s cp) needle
	| null s = Nothing
	| needle == "" = if isDigit (head s) then Just lit else Nothing
	| needle == "\"" && '"' == head s = Just lit
	| needle == "\'" && '\'' == head s = Just lit
	| isPrefixOf needle s = Just $ sLit f (drop (length needle) s) (cp+length needle)
	| otherwise = Nothing

parseError :: String -> ParseState a
parseError msg = do
	s <- gets pdCode
	return $ errorWithoutStackTrace $ msg ++ "\n" ++ case s of
		Lit f s cp -> literateError f cp
		Nib s cp -> "at nibble #" ++ show cp ++ "\nnext nibble is" ++ fromMaybe "" (at s 0>>=Just . show)

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

intParser :: (VT, ParseState String)
intParser = (VInt, do
	(n, rest) <- gets $ parseInt . pdCode
	modify $ \st -> st { pdCode=rest }
	genLit <- gets pdLit
	appendRep (intToNib n," "++show n) -- only need to prepend " " if last was digit, but that would be expensive to check with dlist data structure
	return $ flatHs $ i n)

strParser :: (VT, ParseState String)
strParser = (vstr, do
	(s, rest) <- gets $ parseStr . pdCode
	modify $ \st -> st { pdCode=rest }
	appendRep (strToNib s,tail $ show s)
	return $ "sToA " ++ show s)

chrParser :: (VT, ParseState String)
chrParser = (VChr, do
	(s, rest) <- gets $ parseChr . pdCode
	modify $ \st -> st { pdCode=rest }
	appendRep (chrToNib s,tail $ show s)
	return $ "fromIntegral$ord " ++ show s)

parserToImpl (t,parser) = do
	hs <- parser
	return $ noArgsUsed { implType=t, implCode=hsParen $ hsAtom hs }

parse1Nibble :: String -> [(Int, (Char, ParseState Impl))] -> State ParseData Impl
parse1Nibble name [] = parseError $ "symbol not found in " ++ name
parse1Nibble name ((nib,(lit,impl)):rest) = do
	code <- gets pdCode
	case match code ([[lit]], [nib]) of
		Just nextCode -> do
			modify $ \s -> s { pdCode = nextCode }
			appendRep ([nib],[lit])
			impl
		Nothing -> parse1Nibble name rest

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

intToNib :: Integer -> [Int]
intToNib 10 = [0]
intToNib n = init digits ++ [last digits + 8]
	where digits = map digitToInt $ showOct n ""

strToNib :: String -> [Int]
strToNib "" = [2,0]
strToNib s = (concatMap (\(c,last)->let oc = ord c in case c of
	'\n' -> [last]
	' ' -> [1+last]
	c | oc > 126 || oc < 32 -> [last+7, 15, div oc 16, mod oc 16]
	_ -> [last+div oc 16, mod oc 16]
	) (zip s $ take (length s - 1) (repeat 0) ++ [8]))

chrToNib :: Char -> [Int]
chrToNib c
	| c >= chr 128 = 8 : fromByte c
	| otherwise = case elemIndex c specialChars of
	Just i -> [9 + i]
	Nothing -> fromByte c
