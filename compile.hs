module Compile where

import Data.List
import Data.Char
import Numeric (showOct, readDec)
import Header
import Polylib

compile :: InputCode ic => ic -> Expr
compile input=snd $ getValue(input, [], 0, "", [])

type HsCode = String
type OldNibLit = String
newtype NibLit = NibLit String deriving Show
type Nibble = Int
newtype Nibbles = Nibbles [Int] deriving Show
type Code = [Int]
data Expr = Expr VT [Nibble] OldNibLit HsCode
data E = E [Nibble] OldNibLit HsCode Op
data Op = Binary Expr Expr ((VT,VT)->VT) | Unary Expr (VT->VT) | Nilary VT


intToNib :: Integer -> [Nibble]
intToNib (-1)=[0]
intToNib n=init digits ++ [last digits + 8]
	where digits = map digitToInt $ showOct n ""

strToNib :: String -> [Nibble]
strToNib "" = [2,0]
strToNib s=concatMap (\(c,last)->let oc = ord c in case c of
	'\n' -> [last]
	' ' -> [1+last]
	c | oc > 126 || oc < 32 -> [last+7, 15, div oc 16, mod oc 16]
	otherwise -> [last+div oc 16, mod oc 16]
	) (zip s $ take (length s - 1) (repeat 0) ++ [8])

strToExpr :: InputCode ic => (String, ic) -> (ic, Expr)
strToExpr (s, nbs) = (nbs, Expr vstr (cpind '"' : strToNib s) (show s) (app1 "sToA" $ show s))
intToExpr :: InputCode ic => (Integer, ic) -> (ic, Expr)
intToExpr (n, nbs) = (nbs, Expr vint (cpind '0' : intToNib n) (' ':show n) $ i n)

class InputCode c where
	parseStr :: c -> (String, c)
	parseInt :: c -> (Integer, c)
	next :: c -> (Nibble, c)

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
	next(Nibbles s) = (t, Nibbles rest) where Just (t, rest) = uncons s

instance InputCode NibLit where
	parseStr (NibLit s) = case (reads::ReadS String) s of
	   [(str, rest)] -> (str, NibLit rest)
	   otherwise -> error $ "unparsable string: " ++ s
	parseInt (NibLit ('-':'1':s)) = (-1, NibLit s)
	parseInt (NibLit s) = (n, NibLit rest) where (n, rest) = head $ readDec s
	next (NibLit (c:s))
		| c=='#' = next $ NibLit (tail $ dropWhile (/='\n') s)
		| isDigit c || c:take 1 s == "-1" = (cpind '0', NibLit (c:s))
		| c=='"' = (cpind '"', NibLit (c:s))
		| isSpace c = next $ NibLit s
		| otherwise = (cpind c, NibLit s)

takeInput :: InputCode ic => Int -> ic -> [Nibble]
takeInput 0 code = []
takeInput n code = (ch: takeInput (n-1) rest)
	where (ch, rest) = next code

dropInput :: InputCode ic => Int -> ic -> ic
dropInput 0 code = code
dropInput n code = dropInput (n-1) rest
	where (ch, rest) = next code


retT (Expr t _ _ _) = t
nbOf (Expr _ _ n _) = n
hsOf (Expr _ _ _ hs) = hs

toExpr (c, (E b0 nb hs (Binary (Expr rt1 b1 nb1 hs1) (Expr rt2 b2 nb2 hs2) rtf))) =
	(c, Expr (rtf (rt1,rt2)) (b0++b1++b2) (nb++nb1++nb2) (app2 hs (hs1,hs2)))
toExpr (c, (E b0 nb hs (Unary (Expr rt1 b1 nb1 hs1) rtf))) =
	(c, Expr (rtf rt1) (b0++b1) (nb++nb1) (app1 hs hs1))
toExpr (c, (E b0 nb hs (Nilary vt))) = (c, Expr vt b0 nb hs)

toNilary (c, Expr vt b0 nb hs) = (c, E b0 nb hs $ Nilary vt)

codepage = "~0\"$@`;:+-*/%,^?p"
cpind c = case elemIndex c codepage of
	Just i -> i
	Nothing -> error $ "invalid symbol: " ++ [c]

i s = "("++show s++"::Integer)"

toByte :: Code -> Char
toByte [a,b]=chr $ 16 * a + b

fromByte b=[ord b `div` 16, ord b `mod` 16]

list1 (VList t) = ("", VList t)
list1 t = ("(:[])", VList t)

flipCoerce(a, b) = (af, bf, ct) where (bf, af, ct) = coerce(b, a)
coerce(VInt a, VInt b) = ("","", VInt (a||b))
coerce(VList (VInt True), VInt False) = ("", inspect vint, vstr)
coerce(VInt False, VList (VInt True)) = flipCoerce(vint, vstr)
coerce(VList a, VList b) = (af, bf, VList ct) where (af, bf, ct)=coerce(a, b)
-- this is broken vvv :%,5%,5 2 1
coerce(VInt a, VList b) = ("(:[])"++af++")", bf, ct) where (af, bf, ct) = coerce(VList (VInt a), VList b)
coerce(VList a, VInt b) = flipCoerce(VList a, VInt b)

coercez (c, (E b0 nb hs (Binary (Expr rt1 b1 nb1 hs1) (Expr rt2 b2 nb2 hs2) rtf))) =
	(c, E b0 nb hs (Binary (Expr crt b1 nb1 (app1 chs1 hs1)) (Expr crt b2 nb2 (app1 chs2 hs2)) rtf))
		where (chs1, chs2, crt) = coerce(rt1, rt2)

-- data Expr = Expr VT [Nibble] OldNibLit HsCode
-- data E = E [Nibble] OldNibLit HsCode Op

-- so bad, need tap fns
m1 f (E b0 nb hs (Binary (Expr rt1 b1 nb1 hs1) (Expr rt2 b2 nb2 hs2) rtf)) =
	E b0 nb hs (Binary (Expr mrt b1 nb1 (app1 mhs hs1)) (Expr rt2 b2 nb2 hs2) rtf)
		where (mhs, mrt) = f rt1
m2 f (E b0 nb hs (Binary (Expr rt1 b1 nb1 hs1) (Expr rt2 b2 nb2 hs2) rtf)) =
	E b0 nb hs (Binary (Expr rt1 b1 nb1 hs1) (Expr mrt b2 nb2 (app1 mhs hs2)) rtf)
		where (mhs, mrt) = f rt2
mboth f = (m1 f).(m2 f)

-- todo these aren't used yet
dim (VList e) = 1 + dim e
dim _ = 0
-- dim where string is a scalar
sdim (VList (VInt True)) = 0
sdim (VList e) = 1 + sdim e
sdim _ = 0

-- first arg is vectorized and coerced to match dims
-- todo add nested vectorizing and coercing char
vectorize (c, (E b0 nb hs (Binary (Expr rt1 b1 nb1 hs1) e2 rtf))) =
	(c, (E b0 nb "map" (Binary (Expr rt1 b1 nb1 (app1 hs hs1)) e2 (\(a,b)->rtf(VList a, b)))))

--vectorize 2nd arg once (won't work for multi dim lists todo)
-- vectorize f op (a1, a2, rt) =
-- 	f (\a -> map (u.(op a).a2)) (a1, listArg, VList rt)

argStr n = "arg" ++ show n
argLen = sum . (map argsize)

--todo what about pairs of pairs
argn :: [VT] -> Int -> (VT, HsCode)
argn [] _ = error $ "negative arg index"
argn (VPair a b:r) 0 = (b, argStr $ argLen r+1)
argn (VPair a b:r) 1 = (a, argStr $ argLen r)
argn (a:r) 0 = (a, argStr $ argLen r)
argn (a:r) n = argn r $ n - argsize a

argsize (VPair a b) = argsize a + argsize b
argsize _ = 1

argLhs depth (VPair a b) = "(" ++ argLhs depth a ++ "," ++ argLhs (depth+1) b ++ ")"
argLhs depth _ = argStr depth

--todo uncurry if type is pair?
app1 op hs1 = "(" ++ op ++ " " ++ hs1 ++ ")"
app2 op (hs1, hs2) = "(" ++ op ++ " " ++ hs1 ++ " " ++ hs2 ++ ")"

appArgs (arg1hs, arg2hs) (arg1f, arg2f) = (app1 arg1f arg1hs, app1 arg2f arg2hs)

getValueF :: InputCode ic => (ic, [VT], Int, VT, String) -> (ic, Expr)
getValueF(code, vt, depth, argT, caller) = (s2f, Expr rt2f b2f nb2f $ ("(\\"++argLhs depth argT++"->"++hs2f++")"))
	where (s2f, Expr rt2f b2f nb2f hs2f) = getValue(code, argT:vt, depth+argsize argT, caller, [])

intExpr n = (vint, i n)
e2 = intExpr 2
en1 = intExpr (-1)
e1 = intExpr 1
e0 = intExpr 0

auto "+" _ _ = e1
auto "-" [] (VList _) =  e2
auto "-" _ _ = e1
auto "*" [_] _ = e2
auto "*" _ _ = en1
auto "/" _ (VList _) = e1
auto "/" _ _ = e2
auto "%" _ (VList _) = e1
auto "%" _ _ = e2
auto "^" [] _ = intExpr 10
auto "^" [VInt _] _ = e2
auto "^" [VList _] _ = e1
-- auto _ _ = (vauto, undefined) -- for using in a more custom way
--auto "?" _ =(vint, i 2) --would like it to be ||

getValue :: InputCode ic => (ic, [VT], Int, String, [VT]) -> (ic, Expr)
getValue(scode, vt, depth, caller, lhsTs) = toExpr $ case (cch, rt1, rt2) of
-- 	(c, VMaybe m, _) -> "\acase a of (Just b) -> op b rhs; Nothing -> rhs"
	('~', _, _) -> makeConst $ auto caller lhsTs rt1
	('0', _, _) -> toNilary $ intToExpr $ parseInt code
	('"', _, _) -> toNilary $ strToExpr $ parseStr code
	('$', _, _) -> nilary 0 $ argn vt 0
	('@', _, _) -> nilary 0 $ argn vt 1
	('`', _, _) -> nilary 1 $ argn vt (nextCh +2)
	(';', t, _) -> unaryf t "flip id" id
	(':', _, _) -> (let (cz,e) = coercez $ binary "(++)" fst in (cz,mboth list1 e))
-- :~ = list1, ~: = to_s
	('+', VInt _, VInt _) -> coercez $ binary "(+)" fst
	('+', VInt _, VList et) -> vectorize $ binary "(+)" snd
	('+', VList (VInt True), VList (VInt True)) -> binary "(flip$(filter (/=[]).).splitOn)" $ VList . fst
	('+', VList (VInt True), VList e) -> uncurry binary (join e)
	('+', VList (VInt False), _) -> unary "sum" $ const $ vint
	('+', VList (VList e), _) -> unary "concat" $ const $ VList e
	('-', VInt _, VInt _) -> coercez $ binary "(-)" fst
	('-', VInt False, VList e) -> binary "step" snd
	('-', VInt True, VList e) -> error "todo" -- not sure yet, chr list
	('-', VList e, _) -> unaryf e "(flip$filter.(truthy.))" $ const $ VList e
	('*', VInt _, VInt _) -> coercez $ binary "(*)" fst
	-- todo * for either one being a char...
	('*', VInt _, VList (VInt True)) -> error "todo" -- not sure yet, str int/chr (atoi+, ~ = 0, so it does atoi
	('*', VInt _, VList et) -> vectorize $ binary "(*)" snd
	-- todo ^ * chr list
	('*', VList e, _) -> unaryf (VPair e e) "(flip$foldr1.curry)" id
	('/', VInt _, VInt _) -> coercez $ binary "div" fst
	-- todo ^ chr div?
 	('/', VInt _, VList e) -> binary "(take.fromInteger)" snd
 	-- todo ^ chr take
	('/', VList _, _) -> unary "reverse" id
 	('%', VInt _, VList e) -> binary "(drop.fromInteger)" snd
 	-- todo ^ chr drop
	('%', VInt _, VInt _) -> coercez $ binary "mod" fst
	-- todo ^ chr mod - actually that makes sense
	('%', VList e, _) -> unaryf e "(flip map)" VList
	(',', VList _, _) -> unary "(toInteger.length)" $ const vint
	(',', VInt _, _) -> unary "(\\x->[1..x])" $ VList
	('^', VInt _, VInt _) -> coercez $ binary "(^)" fst
	-- todo ^ chr pow...
	('^', VInt _, VList e) -> binary "((concat.).(replicate.fromInteger))" snd
	('^', VList e, VInt _) -> binary "(\\a i->a!!(fromInteger (i-1)`mod`length a))" $ const e
	-- todo ^ chr !!
	('^', VList e, VList e2) -> binary "zip" $ const $ VList $ VPair e e2
-- 	('?', VInt _, _) -> coercez $ E (cch:nbOf e1) ("iff "++hsOf e1) (Binary e2 e3 fst)
	('?', VList e, e2) | e==e2 -> binary "(\\a e->1+toInteger(fromMaybe (-1) $ elemIndex e a))" snd --todo if not eq then subarray index
	('?', VList e, VList e2) | e==e2 -> binary ("(\\\\)") fst
	('p', rt1, _) -> unary (inspect rt1) $ const vstr
	(c, _, _) -> error $ "no instruction for " ++ show c
	
	{- pass where it's from and arg num in getValue
	+~_ = 1+
	+list~ = tbd (max 0)
	+int = tbd (max 0)
	-~_ = 0
	-_~ = 1
	-list~ = cycle[[]/0] (remove all empty/false)
	?_~ = nil
	?~_ = cond val
	%List~ = head (instead of take 1)
	-}
	where
		(ch, code) = next scode
		(nextCh, _) = next code
		cch = codepage !! ch
		(c1,e1) = getValue(code, vt, depth, [cch], [])
		(c2,e2) = getValue(c1, vt, depth, [cch], [rt1])
		(c3,e3) = getValue(c2, vt, depth, [cch], [rt1, rt2])
		rt1 = retT(e1)
		rt2 = retT(e2)
		binary op rt = (c2, E [ch] [cch] op (Binary e1 e2 rt))
		unary op rt = (c1, E [ch] [cch] op (Unary e1 rt))
		unaryf et op rt = (c2f, E [ch] [cch] op (Binary e1 e2f (rt.snd)))
			where (c2f, e2f) = getValueF(c1, vt, depth, et, [cch])
		nilary extra (rt, rhs) =  toNilary $ (dropInput extra code, Expr rt (ch:takeInput extra code) (cch:map (codepage!!) (takeInput extra code)) rhs)
		makeConst(t, val) = toNilary (code, Expr t [ch] [cch] $ val)

-- Expr VT [Nibble] OldNibLit HsCode