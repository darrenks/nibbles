module Compile where

import Data.List
import Data.Char
import Stdlib

type HsCode = String
type NibLit = String
type Code = [Int]
data Expr = Expr Code VT NibLit HsCode
data E = E NibLit HsCode Op
data Op = Binary Expr Expr ((VT,VT)->VT) | Unary Expr (VT->VT) | Nilary VT Code

nextCode (Expr c _ _ _) = c
retT (Expr _ t _ _) = t
nbOf (Expr _ _ n _) = n
hsOf (Expr _ _ _ hs) = hs

toExpr (E nb hs (Binary (Expr c1 rt1 nb1 hs1) (Expr c2 rt2 nb2 hs2) rtf)) =
	Expr c2 (rtf (rt1,rt2)) (nb++nb1++nb2) (app2 hs (hs1,hs2))
toExpr (E nb hs (Unary (Expr c1 rt1 nb1 hs1) rtf)) =
	Expr c1 (rtf rt1) (nb++nb1) (app1 hs hs1)
toExpr (E nb hs (Nilary vt code)) = Expr code vt nb hs
toNilary (Expr code vt nb hs) = E nb hs $ Nilary vt code

-- toUnary (E nb hs (Binary (Expr c1 rt1 nb1 hs1) e2 rtf)) =
-- 	Expr c2 (rtf (rt1,rt2)) (nb++nb1++nb2) (app1 hs (hs1,hs2))

codepage = "~0\"$@`;:+-*/%,^?p"
cpind c = case elemIndex c codepage of
	Just i -> i
	Nothing -> error $ "invalid symbol: " ++ [c]

i s = "("++show s++"::Integer)"

parseVInt' [] _ = error "unterminated number" -- todo auto range map
parseVInt'(f:s) a
	| f>=8 = Expr s vint (' ':show c) $ i c
	| otherwise = parseVInt' s c
	where c=a*8+f`mod`8
parseVInt :: Code -> Expr
parseVInt(0:s) = parseVInt' (15:s) (-1)
parseVInt(s) = parseVInt' s 0

toByte :: Code -> Char
toByte [a,b]=chr $ 16 * a + b

fromByte b=[ord b `div` 16, ord b `mod` 16]

parseVStr' [] = error "unterminated string" -- todo auto join
parseVStr'(a:s)
	| a8==0 = cont '\n' s
	| a8==1 = cont ' ' s
	| c==32 = ("", s1)
	| c==127 = cont (toByte s1) (drop 2 s1)
	| otherwise = cont (chr c) s1
	where
		a8=a`mod`8
		s1=tail s
		c=a8*16+head s
		cont ch s' = if a>=8
			then ([ch], s')
			else case parseVStr' s' of (rest, rs) -> (ch:rest, rs)

parseVStr :: Code -> Expr
parseVStr(s) = Expr s' vstr h (app1 "sToA" h)
	where (str, s') = parseVStr'(s)
	      h = show str

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

coercez (E nb hs (Binary (Expr c1 rt1 nb1 hs1) (Expr c2 rt2 nb2 hs2) rtf)) =
	E nb hs (Binary (Expr c1 crt nb1 (app1 chs1 hs1)) (Expr c2 crt nb2 (app1 chs2 hs2)) rtf)
		where (chs1, chs2, crt) = coerce(rt1, rt2)

m1 f (E nb hs (Binary (Expr c1 rt1 nb1 hs1) (Expr c2 rt2 nb2 hs2) rtf)) =
	E nb hs (Binary (Expr c1 mrt nb1 (app1 mhs hs1)) (Expr c2 rt2 nb2 hs2) rtf)
		where (mhs, mrt) = f rt1
m2 f (E nb hs (Binary (Expr c1 rt1 nb1 hs1) (Expr c2 rt2 nb2 hs2) rtf)) =
	E nb hs (Binary (Expr c1 rt1 nb1 hs1) (Expr c2 mrt nb2 (app1 mhs hs2)) rtf)
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
vectorize (E nb hs (Binary (Expr c1 rt1 nb1 hs1) e2 rtf)) =
	(E nb "map" (Binary (Expr c1 rt1 nb1 (app1 hs hs1)) e2 (\(a,b)->rtf(VList a, b))))

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

getValueF :: (Code, [VT], Int, VT, String) -> Expr
getValueF(code, vt, depth, argT, caller) = Expr s2f rt2f nb2f $ ("(\\"++argLhs depth argT++"->"++hs2f++")")
	where Expr s2f rt2f nb2f hs2f = getValue(code, argT:vt, depth+argsize argT, caller, [])

auto "+" _ =(vint, i 1)
auto "-" [] =(vint, i 0) --todo would like it to be 2 if slice step
auto "-" [_] =(vint, i 1)
auto "*" _ =(vint, i 2)
auto "/" _ =(vint, i 2) -- would like it to be head
auto "%" _ =(vint, i 2) -- would like it to be tail
auto "^" _ =(vint, i 2)
auto _ _ = (vauto, undefined) -- for using in a more custom way
--auto "?" _ =(vint, i 2) --would like it to be ||

getValue :: (Code, [VT], Int, String, [VT]) -> Expr
getValue(ch:code, vt, depth, caller, lhsTs) = toExpr $ case (cch, rt1, rt2) of
-- 	(c, VMaybe m, _) -> "\acase a of (Just b) -> op b rhs; Nothing -> rhs"
	('~', _, _) -> makeConst $ auto caller lhsTs
	('0', _, _) -> toNilary $ parseVInt code
	('"', _, _) -> toNilary $ parseVStr code
	('$', _, _) -> nilary 0 $ argn vt 0
	('@', _, _) -> nilary 0 $ argn vt 1
	('`', _, _) -> nilary 1 $ argn vt (head code +2)
	(';', t, _) -> unaryf t "flip id" id
	(':', _, _) -> mboth list1 $ coercez $ binary "(++)" fst
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
	('*', VInt _, VList (VInt True)) -> error "todo" -- not sure yet, str int/chr
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
	('?', VInt _, _) -> coercez $ E (cch:nbOf e1) ("iff "++hsOf e1) (Binary e2 e3 fst)
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
		cch = codepage !! ch
		e1 = getValue(code, vt, depth, [cch], [])
		e2 = getValue(nextCode e1, vt, depth, [cch], [rt1])
		e3 = getValue(nextCode e2, vt, depth, [cch], [rt1, rt2])
		rt1 = retT(e1)
		rt2 = retT(e2)
		binary op rt = E [cch] op (Binary e1 e2 rt)
		unary op rt = E [cch] op (Unary e1 rt)
		unaryf et op rt = E [cch] op (Binary e1 e2f (rt.snd))
			where e2f = getValueF(nextCode e1, vt, depth, et, [cch])
		nilary extra (rt, rhs) =  toNilary $ Expr (drop extra code) rt (cch:map (codepage!!) (take extra code)) rhs
		makeConst (t, val) = toNilary $ Expr code t [cch] $ val
