module Parse(parseIntExpr, parseStrExpr, toByte, fromByte) where

import InputCode
import Expr
import Types

import Data.Char
import Numeric (showOct, readDec, readHex, showHex)
import Data.List

zeroNibVal = 1
quoteNibVal = 2

toByte :: [Int] -> Char
toByte [a,b]=chr $ 16 * a + b

fromByte b=[ord b `div` 16, ord b `mod` 16]

_parseNibInt [] _ = error "unterminated number" -- todo auto range map
_parseNibInt(f:s) a
	| f>=8 = (c,Nibbles s)
	| otherwise = _parseNibInt s c
	where c=a*8+toInteger f`mod`8

instance InputCode Nibbles where
	parseInt(Nibbles (0:s)) = (-1, Nibbles s)
	parseInt(Nibbles s) = _parseNibInt s 0
 	parseStr(Nibbles []) = error "unterminated string" -- todo auto join
	parseStr(Nibbles (a:s))
		| a8==0 = cont '\n' s
		| a8==1 = cont ' ' s
		| c==32 = ("", Nibbles s1)
		| c==127 = cont (toByte s1) (drop 2 s1)
		| otherwise = cont (chr c) s1
		where
			a8=a`mod`8
			s1=tail s
			c=a8*16+head s
			cont ch s' = if a>=8
				then ([ch], Nibbles s')
				else case parseStr (Nibbles s') of (rest, rs) -> (ch:rest, rs)
	empty(Nibbles s) = null s
	nextHex (Nibbles s) = (head s, Nibbles $ tail s)
	match (Nibbles s) (_, needle) = isPrefixOf needle s
	parseError msg (Thunk (Nibbles s) context) = error $ msg
	nextInstruction (Nibbles s) = Nibbles $ drop 1 s

instance InputCode NibLit where
	parseStr (NibLit s) = case (reads::ReadS String) s of
	   [(str, rest)] -> (str, NibLit rest)
	   otherwise -> error $ "unparsable string: " ++ s
	parseInt (NibLit ('-':'1':s)) = (-1, NibLit s)
	parseInt (NibLit s) = (n, NibLit rest) where (n, rest) = head $ readDec s
	empty (NibLit s) = undefined -- todo, not as simple as null s since could be space
	nextHex (NibLit s) = (h, NibLit $ tail s) where
		[(h, _)] = readHex $ take 1 s
	match (NibLit s) (needle, _) = needle == fst (takeInput (length needle) s)
	parseError msg (Thunk (NibLit s) context) = error $ msg ++ "\n" ++ head (lines s)
	nextInstruction (NibLit s) = NibLit $ snd $ takeInput 1 s -- todo...

next :: String -> (Char, String)
next (c:s)
	| c=='#' = next $ (tail $ dropWhile (/='\n') s)
	| isDigit c || c:take 1 s == "-1" = ('0', c:s)
	| c=='"' = ('"', (c:s))
	| isSpace c = next $ s
	| otherwise = (c, s)

takeInput :: Int -> String -> (String, String)
takeInput 0 code = ([], code)
takeInput n code = (ch:take, drop) where
		(ch, rest) = next code
		(take, drop) = takeInput (n-1) rest

--todo can stop them from hardcoding their nib/lit to (by making default expr)
parseIntExpr :: InputCode ic => ic -> (ic, Expr)
parseIntExpr code = (rest, Expr int (intToNib n) (' ':show n) (i n))
	where (n, rest) = parseInt code

parseStrExpr :: InputCode ic => ic -> (ic, Expr)
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


