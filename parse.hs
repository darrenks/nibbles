-- todo inversable parser techniques could simplify this
-- todo hide Lit to enforce whitespace always consumed
-- todo use monad for parseInt, etc
-- convention is parse consumes up until next non ignorable code
module Parse(
	parseIntExpr,
	parseStrExpr,
	parseChrExpr,
	parseDataExpr,
	parseCountTuple,
	cp,
	match,
	parseError,
	nextOffset,
	consumeWhitespace,
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
toByte [a,b]=chr $ 16 * a + b

fromByte b=[ord b `div` 16, ord b `mod` 16]

sLit :: String -> String -> Int -> Code
sLit f s cp = consumeWhitespace $ Lit f s cp

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

specialChars = " \n,.-0a"
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
	
parseData (Nib b _) = fromBase 16 (map fromIntegral b)

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

match (Nib s cp) (_, needle) = if isPrefixOf needle s
	then Just $ Nib (drop (length needle) s) (cp+length needle)
	else Nothing
match (Lit f s cp) (needle, _)
	| null s = error "Error! Expected: another expression, found: EOF"
	| needle == " " && isDigit (head s) = Just $ Lit f s cp
	| needle == "\"" && '"' == head s = Just $ Lit f s cp
	| needle == "\'" && '\'' == head s = Just $ Lit f s cp
	| isPrefixOf needle s = Just $ sLit f (drop (length needle) s) (cp+length needle)
	| otherwise = Nothing

parseError :: String -> ParseState Impl
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

parseIntExpr :: ParseState Impl
parseIntExpr = do
	(n, rest) <- gets $ parseInt . pdCode
	modify $ \st -> st { pdCode=rest }
	appendRep (intToNib n,show n)
	return $ noArgsUsed { implType=VInt, implCode=i n }

parseStrExpr :: ParseState Impl
parseStrExpr = do
	(s, rest) <- gets $ parseStr . pdCode
	modify $ \st -> st { pdCode=rest }
	appendRep (strToNib s,tail $ show s)
	return $ noArgsUsed { implType=vstr, implCode=hsParen $ hsAtom $ "sToA " ++ show s }

parseChrExpr :: ParseState Impl
parseChrExpr = do
	(s, rest) <- gets $ parseChr . pdCode
	modify $ \st -> st { pdCode=rest }
	appendRep (chrToNib s,tail $ show s)
	return $ noArgsUsed { implType=VChr, implCode=hsParen $ hsAtom $ "ord " ++ show s }

parseDataExpr :: ParseState Integer
parseDataExpr = do
	dat <- gets $ parseData . pdCode
	appendRep (map fromIntegral (toBase 16 dat),show dat)
	return dat

intToNib :: Integer -> [Int]
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
