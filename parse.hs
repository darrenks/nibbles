module Parse where

import Expr
import Types

import Data.Char
import Numeric (showOct, readDec, readHex, showHex)

import Text.ParserCombinators.ReadP (gather, readP_to_S)
import Text.Read.Lex as Lex (readDecP, lex, Lexeme(String))

import Data.List

-- The Int is the number of characters consumed so far
-- convention cp (code pointer) = this number

zeroNibVal = 1
quoteNibVal = 2

toByte :: [Int] -> Char
toByte [a,b]=chr $ 16 * a + b

fromByte b=[ord b `div` 16, ord b `mod` 16]

_parseNibInt [] _ _ = error "unterminated number" -- todo auto range map
_parseNibInt(f:s) a cp
	| f>=8 = (c,Nib s (cp+1))
	| otherwise = _parseNibInt s c (cp+1)
	where c=a*8+toInteger f`mod`8

parseInt(Nib (0:s) cp) = (-1, Nib s (cp+1))
parseInt(Nib s cp) = _parseNibInt s 0 cp
parseInt (Lit ('-':'1':s) cp) = (-1, Lit s (cp+2))
-- Thanks Jon Purdy for the readP info!
parseInt (Lit s cp) = case readP_to_S (gather readDecP) s of
	[((used, n), rest)] -> (n, Lit rest (cp+length used))
	otherwise -> error $ "unparsable int: " ++ s
	
parseStr(Nib [] cp) = error "unterminated string" -- todo auto join
parseStr(Nib (a:s) cp)
	| a8==0 = cont '\n' 1
	| a8==1 = cont ' ' 1
	| c==32 = ("", Nib s 2)
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
	[((used, Lex.String str), rest)] -> (str, Lit rest (cp+length used))
	otherwise -> error $ "unparsable string: " ++ s
cp (Nib _ cp) = cp
cp (Lit _ cp) = cp
nextOffset (Nib (c:s) cp) = Nib s (cp+1)
nextOffset (Lit (c:s) cp) = Lit s (cp+1)
nextHex (Nib (c:s) cp) = (c, Nib s (cp+1))
nextHex (Lit (c:s) cp) = (h, Lit s (cp+1)) where [(h, _)] = readHex [c]

match (Nib s cp) (_, needle) = if isPrefixOf needle s
	then Just $ Nib (drop (length needle) s) (cp+length needle)
	else Nothing

match s (needle, _) = if needle == start
	then Just rest
	else Nothing
	where (start,rest) = takeInput (length needle) s

parseError msg (Thunk (Lit s _) context) = error $ msg ++ "\n" ++ head (lines s)
-- 	nextInstruction (NibLit s) = NibLit $ snd $ takeInput 1 s -- todo...
parseError msg (Thunk (Nib s _) context) = error $ msg

next :: Code -> (Char, Code)
next (Lit (c:s) cp)
	| c=='#' = next $ Lit rest (cp+length comment+1)
	| isDigit c || c:take 1 s == "-1" = ('0', Lit (c:s) cp)
	| c=='"' = ('"', Lit (c:s) cp)
	| isSpace c = next $ Lit s (cp+1)
	| otherwise = (c, Lit s (cp+1))
		where (comment,'\n':rest) = break (=='\n') s

takeInput :: Int -> Code -> (String, Code)
takeInput 0 code = ([], code)
takeInput n code = (ch:taken, dropn) where
		(ch, rest) = next code
		(taken, dropn) = takeInput (n-1) rest

--todo can stop them from hardcoding their nib/lit to (by making default expr)
parseIntExpr :: Code -> (Code, Expr)
parseIntExpr code = (rest, Expr int (intToNib n) (' ':show n) (i n))
	where (n, rest) = parseInt code

parseStrExpr :: Code -> (Code, Expr)
parseStrExpr code = (rest, Expr str (strToNib s) (show s) (app1 "sToA" (show s)))
	where (s, rest) = parseStr code

intToNib :: Integer -> [Nibble]
intToNib (-1)=[zeroNibVal, 0]
intToNib n=zeroNibVal:(init digits ++ [last digits + 8])
	where digits = map digitToInt $ showOct n ""

strToNib :: String -> [Nibble]
strToNib "" = [quoteNibVal,2,0]
strToNib s = quoteNibVal: (concatMap (\(c,last)->let oc = ord c in case c of
	'\n' -> [last]
	' ' -> [1+last]
	c | oc > 126 || oc < 32 -> [last+7, 15, div oc 16, mod oc 16]
	otherwise -> [last+div oc 16, mod oc 16]
	) (zip s $ take (length s - 1) (repeat 0) ++ [8]))


