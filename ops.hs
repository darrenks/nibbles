{-# LANGUAGE FlexibleInstances #-} -- for String instances

module Ops (Operation(..), ops, impossibleAuto, autoTodo) where

import Types
import Polylib
import Expr
import Parse
import Args
import Parse

data Operation = Op [ArgSpec] ([VT]->(VT, String)) [Int] | Atom (Rep -> Thunk -> (Thunk, Expr)) deriving Show

op(lit, nib, t, impl, autos) = (lit, nib, Op t (toImpl impl) autos)
atom(lit, nib, impl) = (lit, nib, Atom impl)

autoTodo = -88
impossibleAuto = -77 -- suppress displaying in quickref

ops :: [(String, [Int], Operation)]
ops = [
	-- Desc: auto int
	-- Example (size 4): +4~ -> 5
	op("~", [0], [], (undefined::String)~>VAuto, []),
	-- Desc: integer
	-- Example (size 2): 3 -> 3
	-- Test (size 3): 8 -> 8
	-- Test (size 4): 100 -> 100
	-- Test negative one (special case) (size 2): -1 -> -1
	atom("0-9", [1], parseIntExpr), 
	-- Desc: string
	-- Example (size 6): "hi\n" -> "hi\n"
	-- Test space (size 2): " " -> " "
	-- Test empty (size 3): "" -> ""
	-- Test escapes: "\"" -> "\""
	-- Test binary (size 5): "\200" -> "\200"
	atom("\"", [2], parseStrExpr),
	-- Desc: char
	-- Example (size 4): 'b' -> 'b'
	-- Test (size 3): 'a' -> 'a'
	-- Test (size 3): ' ' -> ' '
	-- Test (size 5): '\200' -> '\200'
	atom("'", [13,2], parseChrExpr),
	-- Desc: 1st arg
	-- Example: ;1;2;3 $ -> 1,2,3,3
	atom("$", [3], getArg 0),
	-- Desc: 2nd arg
	-- Example: ;1;2;3 @ -> 1,2,3,2
	atom("@", [4], getArg 1),
	-- Desc: nth arg
	-- Example: ;1;2;3 `2 -> 1,2,3,1
	atom("`", [5], getArgN), -- todo make it 3 to f instead of 2 to f
	-- Desc: tbd (@ too)
	-- Example: 0 -> 0
	op(";$", [6,3], [anyT], "asdf" ~> VInt, []),
	-- Desc: tbd (fn, n ;'s)
	-- Example: 0 -> 0
	op(";;", [6,6], [anyT], "asdf" ~> VInt, []),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(";~", [6,0], [anyT], "asdf" ~> VInt, []),	
	-- Desc: let
	-- Example: + ;3 $ -> 6
	-- Test: ++; 3 ; 2 $ -> 7
	-- Test: ++; 3 ; 2 @ -> 8
	-- Test: ++; 5 /,1 $ $ -> 11
	-- Test: ++; 5 /,2 `2 $ -> 15
	-- Test: ++; 5 /,1 ;7 $ -> 13
	-- Test: ++; 5 /,1 ;+0$ $ -> 11
	-- Test: +;1 + ;2 @ -> 4
	-- Test: .,3 ;%$3 -> [1,2,0]
	op(";", [6], [anyT], "\\x->(x,x)" ~> dup.a1, [autoTodo]),
	-- Desc: singleton
	-- Example: :~3 -> [3]
	op(":~", [7,0], [anyT], "(:[])" ~> VList .a1, [autoTodo]),
	-- Desc: cons
	-- Example: :"a"~"b" -> ["a","b"]
	-- Test (coerce, but pointless todo): :,2~ "34" -> "1234"
	op(":", [7], [list, auto, anyT], cons, [impossibleAuto, impossibleAuto, autoTodo]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(":", [7], [num, auto, anyT], "todo" ~> VList .a1, [autoTodo]),
	-- Desc: append
	-- Example: :"abc""def" -> "abcdef"
	-- Test coerce: :"abc"1 -> "abc1"
	-- Test coerce: :1"abc" -> "1abc"
	-- Test promoting to list: :1 2 -> [1,2]
	op(":", [7], [anyT, anyT], composeOp promoteList (coerce "(++)" [0,1] id), []),
	-- Desc: add
	-- Example: +1 2 -> 3
	-- Test: +2 'a' -> 'c'
	-- Test: +'a' 2 -> 'c'
	-- Test: +' ' ' ' -> 64
	-- Test vectorized: +1,3 -> [2,3,4]
	-- Test 2d vectorized: +1 .,2 ,2 -> [[2,3],[2,3]]
	-- Test string vectorized: +1"abc" -> "bcd"
	-- Test char vectorized: +'a' :1 2 -> "bc"
	op("+", [8], [num, vec], vectorize "+" xorChr, [1,1]),
	-- Desc: split. Removing empties.
	-- Example: %"a b c"" " -> ["a","b","c"]
	-- Test empties: %" a  b "" " -> ["a","b"]
	op("%", [8], [str, str], "flip$(filter (/=[]).).splitOn" ~> VList .a1, []),
	-- Desc: join
	-- Example: *" ",3 -> "1 2 3"
	-- Test 2d: *" ".,2,3 -> ["1 2 3","1 2 3"]
	op("*", [8], [str, list], join.elemT.a2, []),
	-- Desc: product
	-- Example: pt,4 -> 24
	-- Test: pt,0 -> 1
	op("pt", [8,11], [listOf int], "product" ~> VInt, []),
	-- Desc: sum
	-- Example: +,3 -> 6
	-- Test empty: +,0 -> 0
	op("+", [8], [listOf int], "sum" ~> VInt, []),
	-- Desc: concat
	-- Example: +.,3,$ -> [1,1,2,1,2,3]
	op("+", [8], [listOf list], "concat" ~> elemT.a1, []),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("tbd", [8], [str, num], "asdf" ~> VInt, [autoTodo]),
	-- Desc: subtract
	-- Example: - 5 3 -> 2
	-- Test: -'b''a' -> 1
	-- Test: -'d'1 -> 'c'
	op("-", [9], [num, num], "-" ~> xorChr, [1, 1]),
	-- Desc: step
	-- todo test/make negative
	-- Example: %2,5 -> [1,3,5]
	op("%", [9], [num, list], "step" ~> a2, [2]),
	-- Desc: filter
	-- Example: &,5%$2 -> [1,3,5]
	op("&", [9], [list, fn (elemT.a1)], (\args -> "flip$filter.("++truthy (a2 args)++".)") ~> a1, []),
	-- Desc: multiply
	-- Example: *7 6 -> 42
	-- Test: *2 "dd" -> [200,200]
	op("*", [10], [num, vec], vectorize "*" (const VInt), [-1, 2]),
	-- Desc: foldr1
	-- Example: /,3+$@ -> 6
	-- todo make/test empty
	-- todo coerce accum type?
	op("/", [10], [list, fn (\[VList e]->VPair e e)], "flip$foldr1.curry" ~> a2, []),
	-- Desc: sort
	-- Example: st"asdf" -> "adfs"
	op("st", [11, 11], [list], "sort" ~> a1, []),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("tbd", [11,2], [anyT], "asdf" ~> VInt, []),
	-- Desc: reverse
	-- Example: \,3 -> [3,2,1]
	op("\\", [11], [list], "reverse" ~> a1, []),
	-- Desc: divmod
	-- Example: /~7 2 $ -> 3,1
	op("/~", [11,0], [num, num], "divMod" ~> VPair VInt VInt, [2]),
	-- Desc: divide
	-- Example: /7 2 -> 3
	op("/", [11], [num, num], "div" ~> VInt, [impossibleAuto, 2]),
	-- Desc: take
	-- Example: <3,5 -> [1,2,3]
	-- todo test/make negative
	op("<", [11], [num, list], "take.fromIntegral" ~> a2, [1]),
	-- Desc: map accum L
	-- Example: mac,3 0 +@$ $ $ -> [0,1,3],6
	op("mac", onlyLit, [list, anyT, Fn (\[VList e, x]->[x,e])], "\\l i f->swap $ mapAccumL (curry f) i l" ~> (\[_, x, ft] -> VPair (VList $ sndT ft) x), [autoTodo]),
	
	-- Desc: sort by
	-- Example: sb,4%$2 -> [2,4,1,3]
	op("sb", [13,12], [list, fn (elemT.a1)], "flip sortOn" ~> a1, []),	
	-- Desc: map
	-- Example: ."abc"+1$ -> "bcd"
	op(".", [12], [list, fn (elemT.a1)], "flip map" ~> VList .a2, []),
	-- Desc: drop
	-- Example: >3,5 -> [4,5]
	-- Test more than size: >5,3 -> []
	-- todo test/make negative
	op(">", [12], [num, list], "drop.fromIntegral" ~> a2, [1]),
	-- Desc: moddiv
	-- Example : %~7 2 $ -> 1,3
	op("%~", [12,0], [num, num], "(swap.).divMod" ~> VPair VInt VInt, [2]),
	-- Desc: modulus
	-- Example:  %7 2 -> 1
	-- todo test negatives
	op("%", [12], [num, num], "mod" ~> VInt, [impossibleAuto, 2]),
	-- Desc: chr/ord
	-- Example: ch 100 ch 'e' -> 'd',101
	op("ch", [13,13], [num], "id" ~> xorChr.(VChr:), [autoTodo]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(",\\", [13,11], [list], "asdf" ~> VInt, []),
	-- Desc: reshape
	-- Example: rs2,5 -> [[1,2],[3,4],[5]]
	op("rs", [13,9], [num, list], "reshape" ~> VList .a2, [2]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(",^", [13,14], [int, list], "asdf" ~> VInt, [autoTodo]),
	-- Desc: length
	-- Example: ,:3 4 -> 2
	op(",", [13], [list], "length" ~> VInt, []),
	-- Desc: range from 1 to
	-- todo test negative
	-- Example: ,3 -> [1,2,3]
	op(",", [13], [num], "\\x->[1..x]" ~> VList .a1, [autoTodo]),
	-- Desc: is alpha?
	-- Example: a'z' -> 1
	op("a", [14], [char], "bToI.isAlpha.safeChr" ~> VInt, []),
	-- Desc: exponentiation
	-- todo test/make negative
	-- Example: ^2 8 -> 256
	op("^", [14], [int, num], "^" ~> a1, [10,2]),
	-- Desc: replicate
	-- todo test/make negative
	-- Example: ^3 "ab" -> "ababab"
	op("^", [14], [int, list], "(concat.).(replicate.fromIntegral)" ~> a2, [2 {- todo maybe this should be inf -}]),
	-- Desc: subscript. Wrapped.
	-- Example: ="asdf" 2 -> 's'
	-- Test 0 (wrapped): ="asdf" 0 -> 'f'
	-- Test auto: ="asdf"~ -> 'a'
	-- todo make lazier
	op("=", [14], [list, num], "\\a i->a!!(fromIntegral (i-1)`mod`length a)" ~> elemT.a1, [impossibleAuto, 1]),
	-- Desc: zip
	-- Example: z,3"abc" -> [(1,'a'),(2,'b'),(3,'c')]
	op("z", [14], [list, list], "zip" ~>  VList .pairOf.(both elemT), []),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("tbd", [15,1], [anyT], "asdf" ~> VInt, [autoTodo]),
	-- Desc: if/else
	-- Example: ? +0 0 "T" "F" -> "F"
	-- Test coerce: ? +0 1 1 "F" -> "1"
	-- todo more lazy empty check than computing length
	-- todo args could be fn's with orig value (orig list if ?,)
	op("?", [15], [num, anyT, anyT], \ts -> coerce ("(iff."++truthy (a1 ts)++")") [1,2] id ts, [autoTodo, autoTodo, autoTodo]),
	-- Desc: index. Or 0 if not found.
	-- Example: ?  :3:4 5  4 -> 2
	-- Test not found: ? ,3 4 -> 0
	op("?", [15], [list, elemOfA1], "\\a e->1+(fromMaybe (-1) $ elemIndex e a)" ~> VInt, []),
	-- Desc: diff
	-- Example: -"abcd""bd" -> "ac"
	-- Test non existant elements: -"abc""de" -> "abc"
	-- Test doesn't drop all: -"aa""a" -> "a"
	op("-", [15], [list, sameAsA1], "\\\\" ~> a1, []),
	-- Desc: add w/ cast
	-- todo return Maybe
	-- Example: +"10" 2 -> 12
	op("+", [15], [str, int], "(+).read.aToS" ~> VInt, [impossibleAuto, 0]),
	
	-- todo there are some type combinations that are invalid for bin 15
	
	-- Desc: show
	-- Example: p"a" -> "\"a\""
	op("p", onlyLit, [anyT], inspect.a1 ~> vstr, []),
-- todo move test code out of this file, bloated
	-- Test: testCoerce2 1 1 -> "VInt"
	-- Test: testCoerce2 1 'a' -> "VInt"
	-- Test: testCoerce2 1 ,3 -> "VList VInt"
	-- Test: testCoerce2 1 "abc" -> "VList VChr"
	-- Test: testCoerce2 1 .,3,3 -> "VList (VList VInt)"
	-- Test: testCoerce2 1 .,3"abc" -> "VList (VList VChr)"

	-- Test: testCoerce2 'a' 'a' -> "VChr"
	-- Test: testCoerce2 'a' ,3 -> "VList VInt"
	-- Test: testCoerce2 'a' "abc" -> "VList VChr"
	-- Test: testCoerce2 'a' .,3,3 -> "VList (VList VInt)"
	-- Test: testCoerce2 'a' .,3"abc" -> "VList (VList VChr)"

	-- Test: testCoerce2 ,3 ,3 -> "VList VInt"
	-- Test: testCoerce2 ,3 "abc" -> "VList VChr"
	-- Test: testCoerce2 ,3 .,3,3 -> "VList (VList VInt)"
	-- Test: testCoerce2 ,3 .,3"abc" -> "VList (VList VChr)"

	-- Test: testCoerce2 "abc" "abc" -> "VList VChr"
	-- Test: testCoerce2 "abc" .,3,3 -> "VList VChr"
	-- Test: testCoerce2 "abc" .,3"abc" -> "VList (VList VChr)"

	-- Test: testCoerce2 .,3,3 .,3,3 -> "VList (VList VInt)"
	-- Test: testCoerce2 .,3,3 .,3"abc" -> "VList (VList VChr)"

	-- Test: testCoerce2 .,3"abc" .,3"abc" -> "VList (VList VChr)"
	op("testCoerce2", onlyLit, [anyT, anyT], testCoerce2 ~> vstr, []),
	
	-- Test: testCoerceToInt 1 -> 1
	-- Test: testCoerceToInt 'a' -> 97
	-- Test: testCoerceToInt ,3 -> 6
	-- Test: testCoerceToInt "abc" -> 0
	-- Test: testCoerceToInt "12" -> 12
	-- Test: testCoerceToInt .,3,3 -> 18
	-- Test: testCoerceToInt .,3"3" -> 9
	op("testCoerceToInt", onlyLit, [anyT], testCoerceTo VInt, []),
	
	-- Test: testCoerceToChr 100 -> 'd'
	-- Test: testCoerceToChr 'a' -> 'a'
	-- Test: testCoerceToChr :100 200 -> 'd'
	-- Test: testCoerceToChr "abc" -> 'a'
	-- Test: testCoerceToChr .,3:100 200 -> 'd'
	-- Test: testCoerceToChr .,3"abc" -> 'a'
	op("testCoerceToChr", onlyLit, [anyT], testCoerceTo VChr, []),
	
	-- Test: testCoerceToListInt 100 -> [100]
	-- Test: testCoerceToListInt 'a' -> [97]
	-- Test: testCoerceToListInt ,3 -> [1,2,3]
	-- Test: testCoerceToListInt "abc" -> [97,98,99]
	-- Test: testCoerceToListInt .,3,3 -> [1,2,3,1,2,3,1,2,3]
	-- Test: testCoerceToListInt .,3"abc" -> [0,0,0]
	-- Test: testCoerceToListInt .,3"12" -> [12,12,12]
	op("testCoerceToListInt", onlyLit, [anyT], testCoerceTo (VList VInt), []),

	-- Test: testCoerceToStr 100 -> "100"
	-- Test: testCoerceToStr 'a' -> "a"
	-- Test: testCoerceToStr ,3 -> "123"
	-- Test: testCoerceToStr "abc" -> "abc"
	-- Test: testCoerceToStr .,3,3 -> "123123123"
	-- Test: testCoerceToStr .,3"abc" -> "abcabcabc"
	op("testCoerceToStr", onlyLit, [anyT], testCoerceTo vstr, []),

	-- Test: testCoerceToListListInt 100 -> [[100]]
	-- Test: testCoerceToListListInt 'a' -> [[97]]
	-- Test: testCoerceToListListInt ,3 -> [[1,2,3]]
	-- Test: testCoerceToListListInt "abc" -> [[97,98,99]]
	-- Test: testCoerceToListListInt .,3,3 -> [[1,2,3],[1,2,3],[1,2,3]]
	-- Test: testCoerceToListListInt .,3"abc" -> [[97,98,99],[97,98,99],[97,98,99]]
	op("testCoerceToListListInt", onlyLit, [anyT], testCoerceTo (VList $ VList VInt), []),
	
	-- Test: testCoerceToListStr 100 -> ["100"]
	-- Test: testCoerceToListStr 'a' -> ["a"]
	-- Test: testCoerceToListStr ,3 -> ["1","2","3"]
	-- Test: testCoerceToListStr "abc" -> ["abc"]
	-- Test: testCoerceToListStr .,3,3 -> ["1","2","3","1","2","3","1","2","3"]
	-- Test: testCoerceToListStr .,3"abc" -> ["abc","abc","abc"]
	op("testCoerceToListStr", onlyLit, [anyT], testCoerceTo (VList vstr), []),
	
	-- Test: testFinish 123 -> "123"
	-- Test: testFinish 'c' -> "c"
	-- Test: testFinish "abc" -> "abc"
	-- Test: testFinish ,3 -> "1\n2\n3\n"
	-- Test: testFinish .,3,3 -> "1 2 3\n1 2 3\n1 2 3\n"
	-- Test: testFinish .,3"abc" -> "abc\nabc\nabc\n"
	-- Test: testFinish .,2.,2,2 -> "12 12\n12 12\n"
	-- Test: testFinish .,2.,2"ab" -> "ab ab\nab ab\n"
	-- Test: testFinish z ,3 "abc" -> "1\n2\n3\n"
	op("testFinish", onlyLit, [anyT], finish.a1 ~> vstr, [])]

infixr 8 ~>
a~>b = (b,a)

onlyLit = [16, undefined]
a1 = head :: [VT] -> VT
a2 = (!!1) :: [VT] -> VT
a3 = (!!2) :: [VT] -> VT

both f [a,b] = (f a, f b)
pairOf = uncurry VPair
sndOf (VPair a b) = b
dup a = VPair a a

xorChr [VInt, VChr] = VChr
xorChr [VChr, VInt] = VChr
xorChr _ = VInt

class OpImpl impl where
	toImpl :: impl -> [VT] -> (VT, String)
instance OpImpl ([VT] -> VT, [VT] -> String) where
	toImpl (f1,f2) context = (f1 context, f2 context)
instance OpImpl ([VT] -> VT, String) where
	toImpl (f1,s) context = (f1 context, s)
instance OpImpl (VT, [VT] -> String) where
	toImpl (t,f2) context = (t, f2 context)
instance OpImpl (VT, String) where
	toImpl (t,s) context = (t, s)
instance OpImpl ([VT] -> (VT, String)) where
	toImpl f context = f context

int = Exact VInt
char = Exact VChr
str =  Exact vstr
auto = Exact VAuto

fn fx = Fn $ \ts -> [fx ts] -- fn of 1 arg
num = Cond "num" $ isNum . last
vec = Cond "vec" $ isVec . last
list = Cond "list" $ isList . last
anyT = Cond "any" $ const True
listOf (Exact t) =  Exact $ VList t
listOf (Cond desc c) = Cond ("["++desc++"]") $ \vts -> c [elemT $ last vts]

elemT (VList e) = e
fstT (VPair a _) = a
sndT (VPair _ b) = b

elemOfA1 = Cond "a" (\[a1,a2]->VList a2==a1)
sameAsA1 = Cond "[a]" (\[a1,a2]->(a1==a2))

cons [a,b,c] = (t, "\\a b c->"++code++"[a] b c") where
	(t, code) = coerce "(\\a b c->a++c)" [0,2] id [VList a,b,c]

testCoerce2 :: [VT] -> String
testCoerce2 [a1,a2] = "const $ const $ sToA $ show $" ++ if ct1 == ct2
	then show ct1
	else "flipped mismatch: " ++ show ct1 ++ "," ++ show ct2
	where
		ct1 = coerce2(a1,a2)
		ct2 = coerce2(a2,a1)

testCoerceTo :: VT -> [VT] -> (VT, String)
testCoerceTo to [a1] =  (to, coerceTo(to, a1))