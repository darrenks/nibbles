{-# LANGUAGE FlexibleInstances #-} -- for String instances

module Ops (Operation(..), ops, impossibleAuto, autoTodo) where

import Types
import Polylib
import Expr
import Parse
import Args

import State

data Operation = Op [ArgSpec] ([VT]->([VT], String)) [Int] | Atom (ParseState Impl)

op(lit, nib, t, impl, autos) = (lit, nib, Op t (toImpl impl) autos)
atom(lit, nib, impl) = (lit, nib, Atom impl)

autoTodo = -88
impossibleAuto = -77 -- suppress displaying in quickref

ops :: [(String, [Int], Operation)]
ops = map convertNullNib [
	-- Desc: auto int
	-- Example (size 4): +4~ -> 5
	op("~", [0], [], (undefined::String)~>VAuto, []),
	-- Desc: integer
	-- Example (size 2): 3 -> 3
	-- Test (size 3): 8 -> 8
	-- Test (size 4): 100 -> 100
	-- Test negative one (special case) (size 2): -1 -> -1
	atom(" ", [1], parseIntExpr), 
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
	atom("$", [3], argn 1),
	-- Desc: 2nd arg
	-- Example: ;1;2;3 @ -> 1,2,3,2
	atom("@", [4], argn 2),
	-- Desc: nth arg
	-- Example: ;1;2;3 `3 -> 1,2,3,1
	atom("`", [5], nextHex >>= argn), -- todo make it 3 to f instead of 2 to f
	-- Desc: tbd (@ too)
	-- Example: 0 -> 0
	op(";$", [6,3], [anyT], "asdf" ~> VInt, []),
	-- Desc: let fn
	-- Example: ;;2+$1 $4 -> 3,5
	-- Test (multiple args): ;;~1 2 +$@ $4 5 -> 3,9
	-- Test (multiple returns): ;;1 ~$3 $ @4 $ -> 1,3,4,3
	-- Test (mult args and rets): ;;~1 2 ~+~$+~@ $ @3 4 $ -> 2,3,4,5
	-- Test (coerce arg): ;;2+$1 $"4" -> 3,5
	-- Test (coerce pair): ;;~1 2 +$@  $"5"2 -> 3,7
	op(";;", [6,6], [fn noArgs, fn $ ret.a1], 
		(\[a1,a2]->
			let a1L = length $ ret a1
			    a2L = length $ ret a2 in
			"\\x f->"++flattenTuples a2L 1 ++ "(" ++ uncurryN a1L ++ " f $ x(),f)" ~>
			ret a2 ++ [VFn (ret a1) (ret a2)]
			), []),
	-- Desc: let rec
	-- Example (fact): ;~ 5 $ 1 *$@-$~ $3 -> 120,6
	-- Test (multiple args): ;~ ~3 4 $ 0 +@`3 -$1 @   $ 5 6 -> 12,30
	-- Test (multiple rets): ;~ 1 $ ~3 7 +$@0$ $  @2$ -> 4,7,5,7
	-- Test (quicksort): ;~"hello world!"$$:@&$-/@$$:&$-~^-/@$$~@&$-$/@$ -> " !dehllloorw"
	-- Test (coerce rec ret): ;~ 5 1 1 "2" -> 2
	op(";~", [6,0], [fn noArgs, Fn 0 (\[a1]->ret a1++[undefined])],
	(\[a1,a2]->
		let a1L = length $ ret a1
		    rt = ret $ head $ tail $ ret a2 in
		"\\x f -> let ff=fix (\\rec x->let (a,b,c)="++ uncurryN a1L ++"f x ("++ curryN a1L ++"rec) in if "
		++truthy (head $ ret a2)++" a then c else b) in "++flattenTuples (length rt) 1 ++ "(ff $ x(), "++ curryN a1L ++"ff)" ~>
		rt ++ [VFn (ret a1) rt]), []),
	-- Desc: let
	-- Example: + ;3 $ -> 6
	-- Test: ++; 3 ; 2 $ -> 7
	-- Test: ++; 3 ; 2 @ -> 8
	-- Test: ++; 5 /,1 $ $ -> 11
	-- Test: ++; 5 /,2 `3 $ -> 15
	-- Test: ++; 5 /,1 ;7 $ -> 13
	-- Test: ++; 5 /,1 ;+0$ $ -> 11
	-- Test: +;1 + ;2 @ -> 4
	-- Test: .,3 ;%$3 -> [1,2,0]
	-- Test: +;1 ;+2$ -> 4
	op(";", [6], [anyT], "\\x->(x,x)" ~> dup.a1, [autoTodo]),
	-- Desc: singleton
	-- Example: :~3 -> [3]
	op(":~", [7,0], [anyT], "(:[])" ~> vList1 .a1, [autoTodo]),
	-- Desc: cons
	-- Example: :"a"~"b" -> ["a","b"]
	-- Test (coerce, but pointless todo): :,2~ "34" -> "1234"
	op(":", [7], [list, auto, anyT], cons, [impossibleAuto, impossibleAuto, autoTodo]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(":", [7], [num, auto, anyT], "todo" ~> vList1 .a1, [autoTodo]),
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
	-- Test empty: %"" "a" -> []
	-- Test empty div: %"abc" "" -> ["a","b","c"]
	op("%", [8], [str, str], "flip$(filter (/=[]).).splitOn" ~> vList1 .a1, []),
	-- Desc: join
	-- Example: *" ",3 -> "1 2 3"
	-- Test 2d: *" ".,2,3 -> ["1 2 3","1 2 3"]
	op("*", [8], [str, list], join.elemT.a2, []),
	-- Desc: product
	-- Example: pd,4 -> 24
	-- Test: pd,0 -> 1
	op("pd", [8,11], [listOf int], "product" ~> VInt, []),
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
	-- Test chr truthy: &"a b\nc"$ -> "abc"
	-- Test list truthy: &:""~"b"$ -> ["b"]
	op("&", [9], [list, fn ((:[]).elemT.a1)], (\args -> "flip$filter.("++truthy (ret1 $ a2 args)++".)") ~> a1, [impossibleAuto, autoTodo {-reject?-}]),
	-- Desc: multiply
	-- Example: *7 6 -> 42
	-- Test: *2 "dd" -> [200,200]
	op("*", [10], [num, vec], vectorize "*" (const VInt), [-1, 2]),
	-- Desc: foldr1
	-- Example: /,3+$@ -> 6
	-- todo make/test empty
	-- todo coerce accum type?
	op("/", [10], [list, fn $ dup.elemT.a1], "flip$foldr1" ~> ret.a2, []),
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
	op("/~", [11,0], [num, num], "divMod" ~> [VInt, VInt], [2]),
	-- Desc: divide
	-- Example: /7 2 -> 3
	op("/", [11], [num, num], "div" ~> VInt, [impossibleAuto, 2]),
	-- Desc: take
	-- Example: <3,5 -> [1,2,3]
	-- todo test/make negative
	op("<", [11], [num, list], "take.fromIntegral" ~> a2, [1]),
	-- Desc: map accum L
	-- Example: mac,3 0 +@$ $ $ -> [0,1,3],6
	-- Test: mac,3 :~0 :$@ $ $ -> [[0],[0,1],[0,1,2]],[0,1,2,3]
	op("mac", [], [list, anyT, fn2 (\[l, x]->[x,elemT l])], "\\l i f->swap $ mapAccumL f i l" ~> (\[_,x,ft2] -> [vList1$last$ret ft2,x]), [autoTodo]),
	
	-- Desc: sort by
	-- Example: sb,4%$2 -> [2,4,1,3]
	op("sb", [13,12], [list, fn ((:[]).elemT.a1)], "flip sortOn" ~> a1, []),
	-- Desc: map
	-- Example: ."abc"+1$ -> "bcd"
	op(".", [12], [list, fn ((:[]).elemT.a1)], "flip map" ~> VList .ret.a2, []),
	-- Desc: drop
	-- Example: >3,5 -> [4,5]
	-- Test more than size: >5,3 -> []
	-- todo test/make negative
	op(">", [12], [num, list], "drop.fromIntegral" ~> a2, [1]),
	-- Desc: moddiv
	-- Example : %~7 2 $ -> 1,3
	op("%~", [12,0], [num, num], "(swap.).divMod" ~> [VInt,VInt], [2]),
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
	op("rs", [13,9], [num, list], "reshape" ~> vList1 .a2, [2]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(",^", [13,14], [int, list], "asdf" ~> VInt, [autoTodo]),
	-- Desc: length
	-- Example: ,:3 4 -> 2
	op(",", [13], [list], "length" ~> VInt, []),
	-- Desc: range from 1 to
	-- todo test negative
	-- Example: ,3 -> [1,2,3]
	op(",", [13], [num], "\\x->[1..x]" ~> vList1 .a1, [autoTodo]),
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
	op("=", [14], [list, num], "\\a i->lazyAtMod a (fromIntegral (i-1))" ~> elemT.a1, [impossibleAuto, 1]),
	-- Desc: zip
	-- Example: z,3"abc" -> [(1,'a'),(2,'b'),(3,'c')]
	op("z", [14], [list, list], "zip" ~>  VList .(map elemT), []),
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
	op("p", [], [anyT], inspect.a1 ~> vstr, []),
	-- Desc: debug context types
	-- Example: ;5 ct -> error "$ LetArg VInt ..."
	atom("ct", [], gets pdContext >>= parseError . debugContext),
	-- Desc: error
	-- Example: error "asdf" -> error "asdf"
	op("error", [], [str], "error.aToS" ~> vstr, []),
	-- Desc: undefined
	-- Example: un -> error "undefined"
	op("un", [], [], "error \"undefined (todo put location in msg)\"" ~> vstr, []),

	op("testCoerce2", [], [anyT, anyT], testCoerce2 ~> vstr, []),
	op("testCoerceToInt", [], [anyT], testCoerceTo VInt, []),
	op("testCoerceToChr", [], [anyT], testCoerceTo VChr, []),
	op("testCoerceToListInt", [], [anyT], testCoerceTo (VList [VInt]), []),
	op("testCoerceToStr", [], [anyT], testCoerceTo vstr, []),
	op("testCoerceToListListInt", [], [anyT], testCoerceTo (VList [VList [VInt]]), []),
	op("testCoerceToListStr", [], [anyT], testCoerceTo (VList [vstr]), []),
	op("testFinish", [], [anyT], finish.a1 ~> vstr, [])]

infixr 1 ~>
a~>b = (b,a)

-- 16 makes it so that parsing bin will never try it
convertNullNib (lit, nib, op) = (lit, if null nib
		then [16, error $ "attempt to convert "++lit++" to bin (it is only for literate mode)"]
		else nib
	, op)
	
a1 = head :: [VT] -> VT
a2 = (!!1) :: [VT] -> VT

vList1 x = VList [x]

dup a = [a,a]

xorChr [VInt, VChr] = VChr
xorChr [VChr, VInt] = VChr
xorChr _ = VInt

class OpImpl impl where
	toImpl :: impl -> [VT] -> ([VT], String)
instance OpImpl ([VT] -> VT, [VT] -> String) where
	toImpl (f1,f2) context = ([f1 context], f2 context)
instance OpImpl ([VT] -> VT, String) where
	toImpl (f1,s) context = ([f1 context], s)
instance OpImpl (VT, [VT] -> String) where
	toImpl (t,f2) context = ([t], f2 context)
instance OpImpl (VT, String) where
	toImpl (t,s) context = ([t], s)
instance OpImpl ([VT] -> (VT, String)) where
	toImpl f context = ([t],s) where (t, s) = f context

instance OpImpl ([VT] -> [VT], [VT] -> String) where
	toImpl (f1,f2) context = (f1 context, f2 context)
instance OpImpl ([VT] -> [VT], String) where
	toImpl (f1,s) context = (f1 context, s)
instance OpImpl ([VT], [VT] -> String) where
	toImpl (t,f2) context = (t, f2 context)
instance OpImpl ([VT], String) where
	toImpl (t,s) context = (t, s)
instance OpImpl ([VT] -> ([VT], String)) where
	toImpl f context = f context


int = Exact VInt
char = Exact VChr
str =  Exact vstr
auto = Exact VAuto

noArgs = const $ []

fn = Fn 1
fn2 = Fn 2
num = Cond "num" $ isNum . last
vec = Cond "vec" $ const True
list = Cond "list" $ isList . last
anyT = Cond "any" $ const True
listOf (Exact t) =  Exact $ VList [t]
listOf (Cond desc c) = Cond ("["++desc++"]") $ \vts -> case last vts of
	t@(VList _) -> c [elemT $ t]
	otherwise -> False

ret1 (VFn from [to]) = to
elemT (VList e) = todoAssumeFst e
elemT s = error $ show s

-- todo consider arg matching in opcode 15
elemOfA1 = Cond "a" (\[a1,a2]->VList [a2]==a1)
sameAsA1 = Cond "[a]" (\[a1,a2]->(a1==a2))

cons [a,b,c] = (t, "\\a b c->"++code++"[a] b c") where
	(t, code) = coerce "(\\a b c->a++c)" [0,2] id [VList [a],b,c]

testCoerce2 :: [VT] -> String
testCoerce2 [a1,a2] = "const $ const $ sToA $ " ++ show (if ct1 == ct2
	then show ct1
	else "flipped mismatch: " ++ show ct1 ++ "," ++ show ct2)
	where
		ct1 = coerce2(a1,a2)
		ct2 = coerce2(a2,a1)

testCoerceTo :: VT -> [VT] -> (VT, String)
testCoerceTo to [a1] =  (to, coerceTo(to, a1))
