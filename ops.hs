{-# LANGUAGE FlexibleInstances #-} -- for String instances

module Ops (Operation(..), ops, allOps, impossibleAuto, autoTodo) where

import Types
import Polylib
import Expr
import Parse
import Args

import State
import Data.List(concat)

data Operation = Op [ArgSpec] ([VT]->([VT], String)) [Integer] | Atom (ParseState Impl)

op(lit, nib, t, impl, autos) = [(lit, nib, Op t (toImpl impl) autos)]
atom(lit, nib, impl) = [(lit, nib, Atom impl)]

genericReason = "This usually means there is an alternative (likely shorter) way to do what you are trying to."
associativeReason = "Use the other operation order for this associative op to accomplish this. E.g. a+(b+c) instead of (a+b)+c."

litExtError invalidLit lit reason = error $ "You used an op combo that has been remapped to an extension in the binary form.\nYou wrote:\n" ++ invalidLit ++ "\nBut this actually will mean:\n" ++ lit ++ "\n" ++ reason ++ " For more infromation see https://nibbles.golf/tutorial_ancillary.html#extensions"

extendAtom invalidLit reason (lit, nib, impl) =
	atom (invalidLit, [], litExtError invalidLit lit reason) ++ atom (lit, nib, impl)

extendOp invalidLit reason (lit, nib, t, impl, autos) =
	op (invalidLit, [], t, litExtError invalidLit lit reason :: (VT,String), []) ++ op (lit, nib, t, impl, autos)

autoTodo = -88
impossibleAuto = -77 -- suppress displaying in quickref

rawOps :: [[(String, [Int], Operation)]]
rawOps = [
	-- Desc: auto int
	-- Example (size 4): +~4 -> 5
	op("~", [0], [], (error"undefined auto"::String)~>VAuto, []),
	-- Desc: tbd
	-- Example: 0 -> 0
	atom("tbd", [1,0], undefined),
	-- Desc: integer
	-- Example (size 2): 3 -> 3
	-- Test (size 2): 0 -> 0
	-- Test (size 2): 1 -> 1
	-- Test (size 2): 7 -> 7
	-- Test (size 3): 8 -> 8
	-- Test (size 3): 20 -> 20
	-- Test leading zero is separate: :05 -> [0,5]
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
	extendAtom ",\"" genericReason ("'", [13,2], parseChrExpr),
	-- Desc: 1st arg
	-- Example: ;1;2;3 $ -> 1,2,3,3
	-- Test: ;1;2;3;4 ;$ -> 1,2,3,4,1
	-- Test: ;1;2;3;4;5;6;7 ;;$ -> 1,2,3,4,5,6,7,1
	atom("$", [3], argn 1),
	-- Desc: 2nd arg
	-- Example: ;1;2;3 @ -> 1,2,3,2
	atom("@", [4], argn 2),
	-- Desc: 3rd arg
	-- Example: ;1;2;3 _ -> 1,2,3,1
	atom("_", [5], argn 3),
	-- Desc: let fn
	-- todo this ambiguous ;;; (that's okish for now since there are workarounds)
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
	-- Test (multiple args): ;~ ~3 4 $ 0 +@_ -$1 @   $ 5 6 -> 12,30
	-- Test (multiple rets): ;~ 1 $ ~3 7 +$@0$ $  @2$ -> 4,7,5,7
	-- Test (quicksort): ;~"hello world!"$$:@&$-/@$$:&$-~^-/@$$~@&$-$/@$ -> " !dehllloorw"
	-- Test (coerce rec ret): ;~ 5 1 1 "2" -> 2
	op(";~", [6,0], [fn noArgs, Fn (\[a1]->(0, ret a1++[undefined]))],
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
	-- Test: ++; 5 /,2 _ $ -> 15
	-- Test: ++; 5 /,1 ;7 $ -> 13
	-- Test: ++; 5 /,1 ;+0$ $ -> 11
	-- Test: +;1 + ;2 @ -> 4
	-- Test: .,3 ;%$3 -> [1,2,0]
	-- Test: +;1 ;+2$ -> 4
	op(";", [6], [anyT], "\\x->(x,x)" ~> dup.a1, [impossibleAuto]),
	-- Desc: iterate
	-- Example: <3 it 3 +1$ -> [3,4,5]
	-- Test coerce: <3 it 3 :""+1$ -> [3,4,5]
	-- Test tuple: <2 it ~1'a' +$1 +@1 -> [(1,'a'),(2,'b')]
	extendOp "::" associativeReason ("it", [7,7], [fn noArgs, Fn (\[a1]->(length $ ret a1, ret a1))],
		\[a1,a2]->"\\i f->iterate ("++coerceTo (ret a1) (ret a2)++"."++uncurryN (length$ret a1)++"f) (i())" ~> VList (ret a1), []),
	-- Desc: singleton
	-- Example: :~3 -> [3]
	-- Test tuple: :~~1 2 -> [(1,2)]
	op(":~", [7,0], [fn noArgs], "\\v->v():[]" ~> VList .ret.a1, [autoTodo]),
	-- Desc: abs
	-- Example: ab *~5 -> 5
	op("ab", [7], [num, BinAuto], "abs" ~> a1, [autoTodo, impossibleAuto]),
	-- Desc: cons
	-- Example: :"ab"~"cd" -> ["ab","cd"]
	-- Test coerce: :,2~ "34" -> ["12","34"]
	op(":", [7], [list, auto, anyT], \[a,_,c]->
			let (coercedType, coerceFn) = coerceEither [a] [c] in
				"\\a _ c->map"++coerceFn++"$[Left a,Right c]"~>VList coercedType
		, [impossibleAuto, impossibleAuto, autoTodo]),
	-- Desc: append
	-- Example: :"abc""def" -> "abcdef"
	-- Test coerce: :"abc"1 -> "abc1"
	-- Test coerce: :1"abc" -> "1abc"
	--- Test tuple: : z,1"a" z,1"d" -> [(1,'a'),(1,'d')]
	-- Test promoting to list: :1 2 -> [1,2]
	op(":", [7], [anyT, anyT], \[a,b]->
		let
			(ap,apFn) = promoteList a
			(coercedType, coerceFnA, coerceFnB) = coerce [ap] [b]
		in
			"\\a b->("++coerceFnA++"$"++apFn++"a)++"++coerceFnB++"b"~>coercedType
		, []),
	-- Desc: add
	-- Example: +1 2 -> 3
	-- Test: +2 'a' -> 'c'
	-- Test: +'a' 2 -> 'c'
	-- Test: +' ' ' ' -> 64
	-- Test vectorized: +1,3 -> [2,3,4]
	-- Test 2d vectorized: +1 .,2 ,2 -> [[2,3],[2,3]]
	-- Test string vectorized: +1"abc" -> "bcd"
	-- Test char vectorized: +'a' :1 2 -> "bc"
	op("+", [8], [num, vec], vectorize "+" xorChr, [1,2 {- this one will go away to make way for max 0 when we have that extension-}]),
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
	extendOp "+\\" genericReason ("pd", [8,11], [listOf int], "product" ~> VInt, []),
	-- Desc: sum
	-- Example: +,3 -> 6
	-- Test empty: +,0 -> 0
	--- \a -> (sum (map fst a), map snd a)
	op("+", [8], [listOf int], "sum" ~> VInt, []),
	-- Desc: concat
	-- Example: +.,3,$ -> [1,1,2,1,2,3]
	-- Test tuple: +.,2 z ,2 "ab" -> [(1,'a'),(2,'b'),(1,'a'),(2,'b')]
	--- Test tuple2: +z .,2,2 "ab" -> 
	-- \a -> (concat (map fst a), map snd a)
	op("+", [8], [listOf list], "concat" ~> elemT.a1, []),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("tbd", [8], [str, num], "asdf" ~> VInt, [autoTodo]),
	-- Desc: subtract
	-- Example: -5 3 -> 2
	-- Test: -'b''a' -> 1
	-- Test: -'d'1 -> 'c'
	op("-", [9], [num, num], "-" ~> xorChr, [1, 1]),
	-- Desc: step
	-- Example: %2,5 -> [1,3,5]
	-- Test: % *~2,5 -> [5,3,1]
	-- Test todo?: % 0 ,5 -> error
	-- Test: % 1 ,0 -> []
	-- Test lazy: <5 %2,^10 100 -> [1,3,5,7,9]
	op("%", [9], [num, list], "step" ~> a2, [2]),
	-- Desc: reject
	-- Example: &,5~%$2 -> [2,4]
	op("&", [9], [list, auto, fn (elemT.a1)], (\[a1,_,a2] -> "\\l _ f->filter (not."++truthy (ret1 $ a2)++".("++uncurryN (length (elemT a1))++"f)) l") ~> a1, [impossibleAuto, autoTodo]),
	-- Desc: filter
	-- Example: &,5%$2 -> [1,3,5]
	-- Test chr truthy: &"a b\nc"$ -> "abc"
	-- Test list truthy: &:""~"b"$ -> ["b"]
	-- Test tuple: & z,3 "abc" /$2 -> [(2,'b'),(3,'c')]
	op("&", [9], [list, fn (elemT.a1)], (\[a1,a2] -> "\\a f->filter ("++truthy (ret1 a2)++".("++uncurryN (length (elemT a1))++"f)) a") ~> a1, [impossibleAuto, impossibleAuto]),
	-- Desc: multiply
	-- Example: *7 6 -> 42
	-- Test: *2 "dd" -> [200,200]
	op("*", [10], [num, vec], vectorize "*" (const VInt), [-1, 2]),
	-- Desc: scanl
	-- Example: sc,3 ~ 0 +$@ -> [0,1,3,6]
	extendOp ",\\" genericReason ("sc", [13,11], [list, auto, fn noArgs, Fn (\[a1,_,a2]->(length $ ret a2, elemT a1 ++ ret a2))], (\[a1,_,a2,a3]->"\\a _ i f->scanl (\\x y->"++coerceTo (ret a2) (ret a3)++"$"++uncurryN (length (ret a2))++"(("++uncurryN (length (elemT a1))++"f) y) x) (i()) a"  ~> VList (ret a2)), [impossibleAuto, impossibleAuto]),
	-- Desc: scanl1
	-- Example: sc,3+*2$@ -> [1,5,11]
	-- todo make/test empty
	-- Test tuple: sc z ,3 "a.c" +_$ +a@;$ -> [(1,'a'),(3,'a'),(6,'b')]
	extendOp ",\\" genericReason ("sc", [13,11], [list, Fn $ \[a1]->(length $ elemT a1, concat $ replicate 2 $ elemT a1)], (\[a1,a2]->"\\a f->scanl1 (\\x y->"++coerceTo (elemT a1) (ret a2)++"$"++uncurryN (length (elemT a1))++"("++uncurryN (length (elemT a1))++" f y) x) a") ~> VList .elemT.a1, []),
	-- Desc: foldr
	-- Example: /,3 ~ 1 +$@ -> 7
	-- Test(list has tuple): / z ,3 ,3 ~ 1 ++$@_ -> 13
	-- Test(accum has tuple): / ,3 ~ ~0 "" +$@ :$_ $ -> 6,"123"
	-- Test coerce: / ,3 ~ 0 "5" -> 5
	op("/", [10], [list, auto, fn noArgs, Fn (\[a1,_,a2]->(length $ ret a2, elemT a1 ++ ret a2))], (\[a1,_,a2,a3]->"\\a _ i f->foldr (\\x y->"++coerceTo (ret a2) (ret a3)++"$"++uncurryN (length (ret a2))++"(("++uncurryN (length (elemT a1))++"f) x) y) (i()) a" ~> ret a2), [impossibleAuto, impossibleAuto]),
	-- Desc: foldr1
	-- Example: /,3+$@ -> 6
	-- Test coerce: /,3"5" -> 5
	-- todo make/test empty
	op("/", [10], [list, Fn $ \[a1]->(length $ elemT a1, concat $ replicate 2 $ elemT a1)], (\[a1,a2]->"\\a f->foldr1 (\\x y->"++coerceTo (elemT a1) (ret a2)++"$"++uncurryN (length (elemT a1))++"("++uncurryN (length (elemT a1))++" f x) y) a") ~> elemT.a1, []),
	-- Desc: sort
	-- Example: st"asdf" -> "adfs"
	extendOp "\\\\" genericReason ("st", [11, 11], [list], "sort" ~> a1, []),
	-- Desc: tbd
	-- Example: 0 -> 0
	extendOp "\\\"" genericReason ("tbd", [11,2], [], "asdf" ~> VInt, []),
	-- Desc: transpose
	-- Example: tr :"hi"~"yo" -> ["hy","io"]
	-- Test mismatch dims: tr :"hi"~"y" -> ["hy","i"]
	-- Test mismatch dims: tr :"h"~"yo" -> ["hy","o"]
	-- Test 1 dim: tr "abc" -> ["a","b","c"]
	extendOp "\\." genericReason ("tr", [11,12], [list], \[a1] ->
		case a1 of
			VList [VList _] -> "transpose" ~> a1 -- todoAssumeFst
			otherwise -> "transpose.(:[])" ~> VList [a1]
		, []),
	-- Desc: chunk
	-- Example: ck "abbc" ~ -> ["a","bb","c"]
	extendOp "\\&" genericReason ("ck", [11,9], [list, auto], "\\a _->chunkSameAdjacents a" ~> vList1.a1, [impossibleAuto, impossibleAuto]),
	-- Desc: chunkWhile
	-- todo could have also made this chunk while values same, or other behaviors
	-- Example: ck "hey there world!" a$ -> ["hey","there","world"]
	extendOp "\\&" genericReason ("ck", [11,9], [list, fn (elemT.a1)],
		\[a1,a2]->"\\a f->filter (/=[]) $ splitWhen (not."++truthy (safeOnlyElem $ ret a2) ++".("++uncurryN (length (elemT a1))++"f)) a" ~> vList1 a1, []),
	-- Desc: reverse
	-- Example: \,3 -> [3,2,1]
	op("\\", [11], [list], "reverse" ~> a1, []),
	-- Desc: divmod
	-- Example: /~7 2 $ -> 3,1
	op("/~", [11,0], [num, num], "divMod" ~> [VInt, VInt], [2]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("/~", [11,0], [num, list], "asdf" ~> VInt, []),
	-- Desc: divide
	-- todo protect div 0?
	-- Example: /7 2 -> 3
	-- Test: / *~2 7 -> -1
	-- Test: / *~2 *~7 -> 0
	-- Test: / 2 *~7 -> -1
	op("/", [11], [num, num], "div" ~> VInt, [impossibleAuto, 2]),
	-- Desc: take
	-- Example: <3,5 -> [1,2,3]
	-- todo test/make negative
	op("<", [11], [num, list], "take.fromIntegral" ~> a2, [1]),
	-- Desc: sort by
	-- Example: sb,4%$2 -> [2,4,1,3]
	-- Test tuple: sb z ,3 "bca" @ -> [(3,'a'),(1,'b'),(2,'c')]
	extendOp ",." genericReason ("sb", [13,12], [list, fn (elemT.a1)], \[a1,_]->"\\a f->sortOn ("++uncurryN (length (elemT a1))++"f) a" ~> a1, []),
	-- Desc: map
	-- Example: ."abc"+1$ -> "bcd"
	op(".", [12], [list, fn (elemT.a1)], (\[a1,a2]->"(\\a f->map ("++uncurryN (length (elemT a1))++"f) a)") ~> VList .ret.a2, []),
	-- Desc: drop
	-- Example: >3,5 -> [4,5]
	-- Test more than size: >5,3 -> []
	-- todo test/make negative
	op(">", [12], [num, list], "drop.fromIntegral" ~> a2, [1]),
	-- Desc: moddiv
	-- Example : %~7 2 $ -> 1,3
	op("%~", [12,0], [num, num], "(swap.).divMod" ~> [VInt,VInt], [2]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("%~", [12,0], [num, list], "asdf" ~> VInt, []),
	-- Desc: modulus
	-- Example:  %7 2 -> 1
	-- Test: % *~2 7 -> 5
	-- Test: % *~2 *~7 -> -2
	-- Test: % 2 *~7 -> -5
	op("%", [12], [num, num], "mod" ~> VInt, [impossibleAuto, 2]),
	-- Desc: chr/ord
	-- Example: ch 100 ch 'e' -> 'd',101
	extendOp ",," genericReason ("ch", [13,13], [num], "id" ~> xorChr.(VChr:), [autoTodo]),
	-- Desc: reshape
	-- Example: rs2,5 -> [[1,2],[3,4],[5]]
	-- Test lazy: <3 rs2,^10 100 -> [[1,2],[3,4],[5,6]]
	extendOp ",%" genericReason ("rs", [13,9], [num, list], "reshape" ~> vList1 .a2, [2]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(",^", [13,14], [int, list], "asdf" ~> VInt, [autoTodo]),
	-- Desc: length
	-- Example: ,:3 4 -> 2
	op(",", [13], [list], "length" ~> VInt, []),
	-- Desc: range from 0 ...
	-- Example: ,~3 -> [0,1,2]
	op(",~", [13, 0], [num], "\\x->[0..x-1]" ~> vList1 . a1, [autoTodo]),
	-- Desc: range from 1 to
	-- Example: ,3 -> [1,2,3]
	-- Test: ,*~3 -> []
	op(",", [13], [num], "\\x->[1..x]" ~> vList1 .a1, [impossibleAuto]),
	-- Desc: is alpha?
	-- Example: a'z' -> 1
	-- Test: a' ' -> 0
	op("a", [14], [char], "bToI.isAlpha.safeChr" ~> VInt, []),
	-- Desc: exponentiation
	-- todo test/make negative
	-- Example: ^2 8 -> 256
	-- Test: ^2 *~3 -> 0
	-- Test: ^0 0 -> 1
	-- todo handle 0**-3 (maybe should be infinity?)
	op("^", [14], [int, num], "\\a b->if b<0 then 0 else a^b" ~> a1, [10,2]),
	-- Desc: replicate
	-- Example: ^3 "ab" -> "ababab"
	-- Test: ^ *~3 "ab" -> ""
	op("^", [14], [int, list], "(concat.).(replicate.fromIntegral)" ~> a2, [2^128]),
	-- Desc: subscript. Wrapped.
	-- Example: ="asdf" 2 -> 's'
	-- Test 0 (wrapped): ="asdf" 0 -> 'f'
	-- todo empty list will error, maybe it should use maybe or default??
	op("=", [14], [list, num], "\\a i->lazyAtMod a (fromIntegral i - 1)" ~> elemT.a1, [impossibleAuto, autoTodo]),
	-- Desc: zip
	-- Example: z,3"abc" -> [(1,'a'),(2,'b'),(3,'c')]
	-- Test: .z,3,3+$@ -> [2,4,6]
	-- Test 3 tuple: .z z,3,3,3++$@_ -> [3,6,9]
	op("z", [14], [list, list], "zip" ~>  (VList .(concatMap elemT) :: [VT] -> VT), []),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("?~", [15,0], [], "asdf" ~> VInt, []),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("tbd", [15,1], [], "asdf" ~> VInt, [autoTodo]),
	-- Desc: if nonnull (lazy)
	-- Example: ?,"hi" 1 0 -> 1
	-- Test: ?,"" 1 0 -> 0
	-- Test: ?,"hi" $ 0 -> "hi"
	-- todo, the arg passed in should be marked optional used
	op("?,", [15,13], [list, fn ((:[]).a1), Fn (\[a1,a2]->(length$ret a2,[]))], \ts -> let (coercedType, coerceFn) = coerceEither (ret$ts!!1) (ret$ts!!2) in
		"\\c a b->"++ coerceFn ++ "$ iff (not (null c)) (a c) (b())" ~> coercedType
		, [impossibleAuto, autoTodo, autoTodo]),
	-- Desc: if/else
	-- Example: ? +0 0 "T" "F" -> "F"
	-- Test coerce: ? +0 1 1 "F" -> "1"
	-- Test mult rets: ? +0 0 ~1 2 3 4 $ -> 3,4
	-- todo add ability to see c with $, but should it be for true value or both?
	op("?", [15], [num, fn noArgs, Fn (\[a1,a2]->(length$ret a2,[]))], \ts -> let (coercedType, coerceFn) = coerceEither (ret$ts!!1) (ret$ts!!2) in
		"\\c a b->"++coerceFn ++ "$ (iff."++truthy (a1 ts)++") c (a()) (b())" ~> coercedType
		, [autoTodo, autoTodo, autoTodo]),
	-- Desc: index by
	-- Example: ?"...a.."~ a$ -> 4
	op("?", [15], [list, auto, fn (elemT.a1)], (\[a1,_,a2]->"\\l _ f->1+(fromMaybe (-1) $ findIndex ("++truthy (todoAssumeFst $ ret a2)++".("++uncurryN (length (elemT a1))++"f)) l)") ~> VInt, [impossibleAuto, impossibleAuto]),
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
	
	-- Desc: hash (md5) mod
	-- todo auto parse int (would save 1 nibble per use) (or consider using an int str op combo
	-- todo provide an option to easily add salt
	-- Example: hm "asdf" 256 -> 112
	-- Test: hm 5 10 -> 9
	-- Test: hm :1 2 ~ -> 16914085776040879869467699104040987770
	op("hm", [], [anyT, int], (\[a1,a2]->"mod.hlist."++flatten a1) ~> VInt, [autoTodo,2^128]),
	
	-- Desc: debug arg type
	-- Example: pt 5 -> error "VInt"
	op("pt", [], [anyT], "" ~> errorWithoutStackTrace.show :: ([VT]->[VT],String), []),
	-- Desc: show
	-- Example: p"a" -> "\"a\""
	op("p", [], [anyT], inspect.a1 ~> vstr, []),
	-- Desc: debug context types
	-- Example: ;5 ct -> error "$ LetArg VInt ..."
	atom("ct", [], gets pdContext >>= parseError . debugContext), -- todo this puts error in wrong code spot
	-- Desc: error
	-- Example: error "asdf" -> error "asdf"
	op("error", [], [str], "errorWithoutStackTrace.aToS" ~> vstr, []),
	-- Desc: undefined
	-- Example: un -> error "undefined"
	op("un", [], [], "errorWithoutStackTrace \"undefined (todo put location in msg)\"" ~> vstr, []),

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

noArgs = const []

fn e = (Fn $ \prev -> (1, e prev))
fn2 e = (Fn $ \prev -> (2, e prev))


num = Cond "num" $ isNum . last
vec = Cond "vec" $ const True
list = Cond "list" $ isList . last
anyT = Cond "any" $ const True
listOf (Exact t) =  Exact $ VList [t]
listOf (Cond desc c) = Cond ("["++desc++"]") $ \vts -> case last vts of
	t@(VList _) -> c [safeElemT $ t] -- match only first tuple type (todoAssumeFst - check that done where used)
	_ -> False

ret1 (VFn from [to]) = to

safeElemT (VList [e]) = e
safeElemT t = error $ "safeElemT: " ++ show t

safeOnlyElem [e] = e
safeOnlyElem t = error $ "safeOnlyElem: " ++ show t

elemT :: VT -> [VT]
elemT (VList e) = e
elemT s = error $ show s


-- todo consider arg matching in opcode 15
elemOfA1 = Cond "a" (\[a1,a2]->VList [a2]==a1) -- todoAssumeFst ?
sameAsA1 = Cond "[a]" (\[a1,a2]->(a1==a2))

testCoerce2 :: [VT] -> String
testCoerce2 [a1,a2] = "const $ const $ sToA $ " ++ show (if ct1 == ct2
	then show ct1
	else "flipped mismatch: " ++ show ct1 ++ "," ++ show ct2)
	where
		ct1 = coerce2(a1,a2)
		ct2 = coerce2(a2,a1)

testCoerceTo :: VT -> [VT] -> (VT, String)
testCoerceTo to a1 =  (to, coerceTo [to] a1)

ops = map (convertNullNib.last) rawOps -- the others are for invalid literate warning
allOps = concat [atom(
		replicate unary ';' ++ snd symb,
		replicate unary 6 ++ [2+fst symb],
		argn (unary*3+fst symb))
	| unary <- [1..10]
	, symb <- [(1,"$"),(2,"@"),(3,"_")]
	] ++ map convertNullNib (concat rawOps)
