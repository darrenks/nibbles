-- Todo hide Lit to enforce whitespace always consumed
-- convention is parse consumes up until next non ignorable code
module Parse(
	parseIntExpr,
	parseStrExpr,
	parseChrExpr,
	cp,
	match,
	parseError,
	nextOffset,
	consumeWhitespace,
	nextHex,
	empty,
	fromByte,
	toByte) where

import Expr
import Types

import Data.Char
import Numeric (showOct, readDec, readHex, showHex)

import Text.ParserCombinators.ReadP (gather, readP_to_S)
import Text.Read.Lex as Lex (readDecP, lex, Lexeme(String), Lexeme(Char))

import Data.List

toByte :: [Int] -> Char
toByte [a,b]=chr $ 16 * a + b

fromByte b=[ord b `div` 16, ord b `mod` 16]

sLit = (consumeWhitespace .) . Lit

_parseNibInt [] _ _ = error "unterminated number" -- todo auto range map
_parseNibInt(f:s) a cp
	| f>=8 = (c,Nib s (cp+1))
	| otherwise = _parseNibInt s c (cp+1)
	where c=a*8+toInteger f`mod`8

parseInt(Nib (0:s) cp) = (-1, Nib s (cp+1))
parseInt(Nib s cp) = _parseNibInt s 0 cp
parseInt (Lit ('-':'1':s) cp) = (-1, sLit s (cp+2))
-- Thanks Jon Purdy for the readP info!
parseInt (Lit s cp) = case readP_to_S (gather readDecP) s of
	[((used, n), rest)] -> (n, sLit rest (cp+length used))
	otherwise -> error $ "unparsable int: " ++ s
	
parseStr(Nib [] cp) = error "unterminated string" -- todo auto join
parseStr(Nib (a:s) cp)
	| a8==0 = cont '\n' 1
	| a8==1 = cont ' ' 1
	| c==32 = ("", Nib (tail s) 2)
	| c==127 = cont (toByte $ tail s) 4
	| otherwise = cont (chr c) 2
	where
		a8=a`mod`8
		c=a8*16+head s
		cont ch used = if a>=8
			then ([ch], after)
			else case parseStr after of (rest, rs) -> (ch:rest, rs)
			where after = Nib (drop (used-1) s) (cp+used)
parseStr (Lit s cp) = case readP_to_S (gather Lex.lex) s of
	[((used, Lex.String str), rest)] -> (str, sLit rest (cp+length used))
	otherwise -> error $ "unparsable string: " ++ s

specialChars = " \n,.-0a"
parseChr :: Code -> (Char, Code)
parseChr(Nib [] cp) = error "unterminated char"
parseChr(Nib (c:rest) cp)
	| c == 8 = (toByte rest, Nib (drop 2 rest) (cp+3))
	| c > 8 = (specialChars !! (c-9), Nib rest (cp+1))
parseChr(Nib [_] cp) = error "unterminated char"
parseChr(Nib (a:b:rest) cp) = (toByte [a,b], Nib rest (cp+2))
parseChr (Lit s cp) = case readP_to_S (gather Lex.lex) s of
	[((used, Lex.Char char), rest)] -> (char, sLit rest (cp+length used))
	otherwise -> error $ "unparsable char: " ++ s
cp (Nib _ cp) = cp
cp (Lit _ cp) = cp

empty (Nib [uselessOp] _) = True
empty (Nib [] _) = True
empty (Lit [] _) = True
empty _ = False
nextOffset (Nib (c:s) cp) = Nib s (cp+1)
nextOffset (Lit (c:s) cp) = Lit s (cp+1)
nextHex (Nib (c:s) cp) = (c, Nib s (cp+1))
nextHex (Lit (c:s) cp) = (h, sLit s (cp+1)) where [(h, _)] = readHex [c]

shead (s:rest) = s

match (Nib s cp) (_, needle) = if isPrefixOf needle s
	then Just $ Nib (drop (length needle) s) (cp+length needle)
	else Nothing

match (Lit s cp) (needle, _)
	| needle == "0-9" && (isDigit (shead s) || isPrefixOf "-1" s) = Just $ Lit s cp
	| needle == "\"" && '"' == head s = Just $ Lit s cp
	| needle == "\'" && '\'' == head s = Just $ Lit s cp
	| isPrefixOf needle s = Just $ sLit (drop (length needle) s) (cp+length needle)
	| otherwise = Nothing

parseError msg (Thunk (Lit s _) context) = error $ msg ++ "\n" ++ head (lines s)
parseError msg (Thunk (Nib s _) context) = error $ msg

consumeWhitespace :: Code -> Code
consumeWhitespace (Nib n cp) = Nib n cp
consumeWhitespace (Lit [] cp) = Lit [] cp
consumeWhitespace (Lit (c:s) cp)
	| c=='#' = sLit rest (cp+length comment)
	| isSpace c = sLit s (cp+1)
	| otherwise = Lit (c:s) cp
		where (comment, rest) = break (=='\n') s

parseIntExpr :: Expr -> Thunk -> (Code, Expr)
parseIntExpr (Expr _ b _ _) (Thunk code _) =
	(rest, Expr int (b ++ intToNib n) (' ':show n) (i n))
		where (n, rest) = parseInt code

parseStrExpr :: Expr -> Thunk -> (Code, Expr)
parseStrExpr (Expr _ b _ _) (Thunk code _) =
	(rest, Expr str (b ++ strToNib s) (show s) (app1 "sToA" (show s)))
		where (s, rest) = parseStr $ code

parseChrExpr :: Expr -> Thunk -> (Code, Expr)
parseChrExpr (Expr _ b _ _) (Thunk code _) =
	(rest, Expr char (b ++ chrToNib s) (show s) (app1 "ord" (show s)))
		where (s, rest) = parseChr $ code

intToNib :: Integer -> [Nibble]
intToNib (-1)=[0]
intToNib n=init digits ++ [last digits + 8]
	where digits = map digitToInt $ showOct n ""

strToNib :: String -> [Nibble]
strToNib "" = [2,0]
strToNib s = (concatMap (\(c,last)->let oc = ord c in case c of
	'\n' -> [last]
	' ' -> [1+last]
	c | oc > 126 || oc < 32 -> [last+7, 15, div oc 16, mod oc 16]
	otherwise -> [last+div oc 16, mod oc 16]
	) (zip s $ take (length s - 1) (repeat 0) ++ [8]))

-- todo optimizations for space and newline/etc
chrToNib :: Char -> [Nibble]
chrToNib c
	| c >= chr 128 = 8 : fromByte c
	| otherwise = case elemIndex c specialChars of
	Just i -> [9 + i]
	Nothing -> fromByte c
