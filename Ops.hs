module Ops where

import Types
import Polylib
import Expr
import Parse
import Args
import Hs
import OpsHelper

import State
import Data.List(concat)
import Data.Maybe

autoTodoValue = -88
autoTodo t = AutoDefault t autoTodoValue

rawOps :: [[(Bool, [String], [Int], Operation)]]
rawOps = [
	-- Desc: auto value
	-- Example (size 4): +4~ -> 5
	-- This is only here so that auto handling can use memoized args after.
	-- and so that if ops check type it won't match
	op("~", [0], [], (error"undefined auto impl"::String)~>InvalidType), 
	-- Desc: integer
	-- Example (size 2): 3 -> 3
	-- Test (size 2): 0 -> 0
	-- Test (size 2): 1 -> 1
	-- Test (size 2): 7 -> 7
	-- Test (size 3): 8 -> 8
	-- Test (size 2): 10 -> 10
	-- Test (size 3): 20 -> 20
	-- Test leading zero is separate: :05 -> [0,5]
	op(litDigit, [1], [ParseArg "int" intParser], ()), 
	-- Desc: string
	-- Example (size 6): "hi\n" -> "hi\n"
	-- Test space (size 2): " " -> " "
	-- Test empty (size 3): "" -> ""
	-- Test escapes: "\"" -> "\""
	-- Test binary (size 5): "\200" -> "\200"
	-- Test list of strs (size 5) : """a" -> ["","a"]
	-- Test list of strs (size 7) : "a""" -> ["a",""]
	-- Test list of strs (size 7) : "a""b" -> ["a","b"]
	op("\"", [2], [ParseArg "str" strParser], ()),
	-- Desc: char
	-- Example (size 4): 'b' -> 'b'
	-- Test (size 3): 'a' -> 'a'
	-- Test (size 3): '\n' -> '\n'
	-- Test (size 3): ' ' -> ' '
	-- Test (size 3): '/' -> '/'
	-- Test (size 3): '0' -> '0'
	-- Test (size 4): '!' -> '!'
	-- Test chr 127 (size 5): '\DEL' -> '\DEL'
	-- Test chr 0 (size 5): '\NUL' -> '\NUL'
	-- Test chr 15 (size 5): '\SI' -> '\SI'
	-- Test chr 16 (size 5): '\DLE' -> '\DLE'
	-- Test chr 31 (size 5): '\US' -> '\US'
	-- Test chr (size 5): '\128' -> '\128'
	-- Test chr (size 5): '\255' -> '\255'
	extendOp [",","\""] genericReason ("'", [13,2], [ParseArg "chr" chrParser], ()),
	-- Desc: 1st arg
	-- Example: ;1;2;3 $ -> 1,2,3,3
	-- Test: ;1;2;3;4 ;$ -> 1,2,3,4,1
	-- Test: ;1;2;3;4;5;6;7 ;;$ -> 1,2,3,4,5,6,7,1
	op("$", [3], [], argn 1),
	-- Desc: 2nd arg
	-- Example: ;1;2;3 @ -> 1,2,3,2
	op("@", [4], [], argn 2),
	-- Desc: 3rd arg
	-- Example: ;1;2;3 _ -> 1,2,3,1
	op("_", [5], [], argn 3),
	-- Desc: let fn (todo this needs a new home for its rep)
	-- Example: ;;2+1$ $4 -> 3,5
	-- Test (multiple args): ;;~1 2 +@$ $4 5 -> 3,9
	-- Test (multiple returns): ;;1 ~$3 $ @4 $ -> 1,3,4,3
	-- Test (mult args and rets): ;;~1 2 ~+$~+@~ $ @3 4 $ -> 2,3,4,5
	-- Test (coerce arg): ;;2+1$ $"4" -> 3,5
	-- Test (coerce pair): ;;~1 2 +@$  $"5"2 -> 3,7
	-- Test deps: .,2;;2+@$ -> [3,4]
	op([";",";"], [6,6], [fn noArgs, fn $ ret.a1],
		(\[a1,a2]->
			let a1L = length $ ret a1
			    a2L = length $ ret a2 in
			"\\x f->"++flattenTuples a2L 1 ++ "(f $ x(),f)" ~>
			ret a2 ++ [VFn (ret a1) (ret a2)]
			)),
	-- Desc: let rec
	-- Example (fact): ;~ 5 $ 1 *@-$~$ $3 -> 120,6
	-- Test (multiple args): ;~ ~3 4 $ 0 +_@ -$1 @   $ 5 6 -> 12,30
	-- Test (multiple rets): ;~ 1 $ ~3 7 +@0 @ $ $  @2$ -> 4,7,5,7
	-- Test (quicksort): ;~"hello world!"$$:@&$-/@$$:&$-~^-/@$$~@&$-$/@$ -> " !dehllloorw"
	-- Test (coerce rec ret): ;~ 5 1 1 "2" -> 2
	op([";","~"], [6,0], [fn noArgs, fnx (\[a1]->(0, ret a1++[InvalidType]))],
	(\[a1,a2]->
		let a1L = length $ ret a1
		    rt = ret $ head $ tail $ ret a2 in
		"\\x f -> let ff=fix (\\rec x->let (a,b,c)=f ("++flattenTuples a1L 1++"(x,rec)) in if "
		++truthy [head $ ret a2]++" a then c else b) in "++flattenTuples (length rt) 1 ++ "(ff $ x(), ff)" ~>
		OptionalLets (rt ++ [VFn (ret a1) rt]))),
	-- Desc: let
	-- Example: + ;3 $ -> 6
	-- Test: ++; 3 ; 2 $ -> 7
	-- Test: ++; 3 ; 2 @ -> 8
	-- Test: ++; +5 0 /,1 $ $ -> 11
	-- Test: ++; +5 0 /,2 _ $ -> 15
	-- Test: ++; + +5 0 0 /,1 ;7 $ -> 13
	-- Test: ++; +++5 0 0 0 /,1 ;+0$ $ -> 11
	-- Test: ++;2 ;1 @ -> 5
	-- Test: .,3 ;%$3 -> [1,2,0]
	-- Test: +++0 0;1 ;+2$ -> 4
	op(";", [6], [anyT], "\\x->(x,x)" ~> dup.a1),
	-- Desc: iterate
	-- Example: <3 it 3 +1$ -> [3,4,5]
	-- Test coerce: <3 it 3 :""+1$ -> [3,4,5]
	-- Test tuple: <2 it ~1'a' +1$ +1@ -> [(1,'a'),(2,'b')]
	extendOp [":",":"] associativeReason ("it", [7,7], [fn noArgs, fnx (\[a1]->(length $ ret a1, ret a1))],
		\[a1,a2]->"\\i f->iterate ("++coerceTo (ret a1) (ret a2)++".f) (i())" ~> VList (ret a1)),
	-- Desc: singleton
	-- Example: :3~ -> [3]
	-- Test tuple: :~1 2~ -> [(1,2)]
	op([":"], [7], [fn noArgs, auto], "\\v->v():[]" ~> VList .ret.a1),
	--- Desc: abs
	--- Example: ab *~5 -> 5
	-- op("ab", [7], [autoTodo num, binOnlyAuto], "abs" ~> a1),
	-- Desc: append
	-- Example: :"abc" "def" -> "abcdef"
	-- Test coerce: :"abc"1 -> "abc1"
	-- Test coerce: :1"abc" -> "1abc"
	-- Test tuple: : z,1"a" z,1"d" -> [(1,'a'),(1,'d')]
	-- Test tuple ~ happy path: :~1'a' z,3"abc" -> [(1,'a'),(1,'a'),(2,'b'),(3,'c')]
	--- todo make work Test: ~1'a' 2'b' -> [(1,'a'),(2,'b')]
	-- Test promoting to list: :1 2 -> [1,2]
	op(":", [7], [fn noArgs, fn noArgs], \[a,b]->
		let
			(ap,apFn) = promoteList (ret a)
			(coercedType, coerceFnA, coerceFnB) = coerce [ap] (ret b)
		in
			"\\a b->("++coerceFnA++"$"++apFn++"(a()))++"++coerceFnB++"(b())"~>coercedType
		),
	-- Desc: max
	-- Example: ]4 5 -> 5
	-- Test: ]~ *~4 -> 0
	extendOp ["+"] commutativeReason ("]", [8], [AutoDefault num 0, andC num sndArgLarger], "max"~>orChr),
	-- Desc: add
	-- Example: +2 1 -> 3
	-- Test: +'a' 2 -> 'c'
	-- Test: +' ' ' ' -> 64
	-- Test vectorized: +1,3 -> [2,3,4]
	-- Test 2d vectorized: +1 ^:,2~ 2 -> [[2,3],[2,3]]
	-- Test string vectorized: +1"abc" -> "bcd"
	-- Test char vectorized: +'a' :1 2 -> "bc"
	-- Test vectorized tuple: +1 z,3"abc" -> [(2,'b'),(3,'c'),(4,'d')]
	-- Test: +3 ~ -> 4
	-- Test: +3 3 -> 6
	extendOp ["]"] commutativeReason ("+", [8], [num, AutoDefault vec 1], vectorize "+" xorChr),
	-- Desc: split. Removing empties.
	-- Example: %"a b c" " " -> ["a","b","c"]
	-- Test empties: %" a  b " " " -> ["a","b"]
	-- Test empty: %"" "a" -> []
	-- Test empty div: %"abc" "" -> ["a","b","c"]
	-- Test chr split: %"a b" ' ' -> ["a","b"]
	op("%", [8], [str, orC str char], (\[a1,a2]->
		let (ap2, apf) = promoteList [a2]
		in "(\\a b->filter (/=[])$splitOn ("++apf++"b) a)") ~> vList1 .a1),
	-- Desc: words
	-- Example: %"a\nb c.d"~ -> ["a","b","c.d"]
	op("%", [8], [str, auto], "\\a->map sToA $ words (aToS a)" ~> vList1 .a1),
	-- Desc: tbd
	-- Example: 0 -> 0
	op("tbd", [8], [str, int], undefinedImpl),
	-- Desc: join
	-- Example: *" ",3 -> "1 2 3"
	-- Test 2d: *" "^:,3~2 -> ["1 2 3","1 2 3"]
	-- Test tuple: *" "z,3"abc" -> ["1 a","2 b","3 c"]
	-- Test lopsided tuple: *" "z^:,2~2"ab" -> [("1 2",'a'),("1 2",'b')]
	-- Test: *" "z^:,2~ 2^:,2~2 -> [("1 2","1 2"),("1 2","1 2")]
	op("*", [8], [str, list], join.a2),
	-- Desc: product
	-- Example: pd,4 -> 24
	-- Test: pd,0 -> 1
	-- Test tuple: pd z,4 "abcd" $ -> 24,"abcd"
	extendOp ["+","\\"] genericReason ("pd", [8,11], [listOf int], \[a1]->let (uzT,uzF)=unzipTuple a1 in
			appFst uzT "product" ++ "." ++ uzF ~> VInt : tail uzT
		),
	-- Desc: sum
	-- Example: +,3 -> 6
	-- Test empty: +,0 -> 0
	-- Test tuple: +z ,3 "abc" $ -> 6,"abc"
	op("+", [8], [listOf int], \[a1]->
		let (uzT,uzF)=unzipTuple a1 in
			appFst uzT "sum" ++ "." ++ uzF ~> VInt : tail uzT
		),
	-- Desc: concat
	-- Example: +.,3,$ -> [1,1,2,1,2,3]
	-- Test tuple: +^:z ,2 "ab"~2 -> [(1,'a'),(2,'b'),(1,'a'),(2,'b')]
	-- Test tuple2: +z ^:,2~2 "ab" $ -> [1,2,1,2],"ab"
	-- \a -> (concat (map fst a), map snd a)
	op("+", [8], [listOf list], \[a1]->let (uzT,uzF)=unzipTuple a1 in
			appFst uzT "concat" ++ "." ++ uzF ~> head (elemT (head uzT)) : tail uzT
		),
	-- Desc: subtract
	-- Example: -5 3 -> 2
	-- Test: -'b''a' -> 1
	-- Test: -'d'1 -> 'c'
	-- Test: -~2 -> -1
	-- Test: -2~ -> 1
	op("-", [9], [AutoDefault num 1, AutoDefault num 1], "-" ~> xorChr),
	-- Desc: square
	-- Example: sqr ,9 -> [[1,2,3],[4,5,6],[7,8,9]]
	-- Test: sqr ,11 -> [[1,2,3,4],[5,6,7,8],[9,10,11]]
	extendOp ["%","0"] genericReason ("sqr", [9,1,8], [list], "\\a->chunksOf (ceiling $ sqrt $ fromIntegral $ length a) a" ~> vList1 .a1),
	-- Desc: step
	-- Example: %2,5 -> [1,3,5]
	-- Test: % *~2,5 -> [5,3,1]
	-- Test: %~,5 -> [1,3,5]
	-- Test: % 1 ,0 -> []
	-- Test lazy: <5 %2,^10 100 -> [1,3,5,7,9]
	op("%", [9], [AutoDefault num 2, list], "step" ~> a2),
	-- Desc: partition
	-- Example: &,5~~%$2 $ -> [1,3,5],[2,4]
	-- Example notted: &,5~~~%$2 $ -> [2,4],[1,3,5]
	op("&", [9], [list, Auto False, Auto False, AutoNot $ fn (elemT.a1)], \[a1,a2]->"\\a f->partition f a" ~> [a1::VT,a1]),
	-- Desc: filter
	-- Example: &,5%$2 -> [1,3,5]
	-- Test chr truthy: &"a b\nc"$ -> "abc"
	-- Test list truthy: &"""b"$ -> ["b"]
	-- Test tuple: & z,3 "abc" /$2 -> [(2,'b'),(3,'c')]
	-- Test auto not: &,5~%$2 -> [2,4]
	op("&", [9], [list, AutoNot $ fn (elemT.a1)], "\\a f->filter f a" ~> a1),
	-- Desc: is alpha?
	-- Example: a'z' -> 1
	-- Test: a' ' -> 0
	op("a", [10], [char], "bToI.isAlpha.chr.fromIntegral" ~> VInt),
	-- Desc: min
	-- Example: [4 5 -> 4
	extendOp ["*"] commutativeReason ("[", [10], [int, andC num sndArgLarger], "min"~>orChr),
	-- Desc: multiply
	-- Example: *7 6 -> 42
	-- Test: *2 "dd" -> [200,200]
	-- Test: *~ 5 -> -5
	-- Test: *5 ~ -> 10
	extendOp ["["] commutativeReason ("*", [10], [AutoDefault int (-1), AutoDefault vec 2], vectorize "*" (const VInt)),
	-- Desc: scanl
	-- Example: sc,3 ~ 0 +@$ -> [0,1,3,6]
	-- Test tuple: sc,3 ~ ~1 2 +*2$_ 3 -> [(1,2),(4,3),(7,3),(9,3)]
	extendOp [",","\\"] genericReason ("sc", [13,11], [list, auto, fn noArgs, fnx (\[a1,a2]->(length $ ret a2, elemT a1 ++ ret a2))], (\[a1,a2,a3]->"\\a i f->scanl (\\x y->"++coerceTo (ret a2) (ret a3)++"$f$"++flattenTuples(length $ elemT a1)(length $ ret a2)++"(y,x)) (i()) a"  ~> VList (ret a2))),
	-- Desc: scanl1
	-- Example: sc,3+*2$@ -> [1,5,11]
	-- Test empty: sc,0+@$ -> []
	-- Test tuple: sc z ,3 "a.c" +_$ +a@;$ -> [(1,'a'),(3,'a'),(6,'b')]
	extendOp [",","\\"] genericReason ("sc", [13,11], [list, fnx $ \[a1]->(length $ elemT a1, concat $ replicate 2 $ elemT a1)], (\[a1,a2]->"\\a f->scanl1 (\\x y->"++coerceTo (elemT a1) (ret a2)++"$f$"++flattenTuples(length $ elemT a1)(length $ elemT a1)++"(y,x)) a") ~> VList .elemT.a1),
	-- Desc: foldr
	-- Example: /,3 ~ 1 +@$ -> 7
	-- Test(list has tuple): / z ,3 ,3 ~ 1 ++_@$ -> 13
	-- Test(accum has tuple): / ,3 ~ ~0 "" +@$ :$_ $ -> 6,"123"
	-- Test coerce: / ,3 ~ 0 "5" -> 5
	op("/", [10], [list, auto, fn noArgs, fnx (\[a1,a2]->(length $ ret a2, elemT a1 ++ ret a2))], (\[a1,a2,a3]->"\\a i f->foldr (\\x y->"++coerceTo (ret a2) (ret a3)++"$f$"++flattenTuples(length $ elemT a1)(length $ ret a2)++"(x,y)) (i()) a" ~> ret a2)),
	-- Desc: foldr1
	-- Example: /,3+@$ -> 6
	-- Test coerce: /,3"5" -> 5
	-- Test empty: /,0+@$ -> 0
	op("/", [10], [list, fnx $ \[a1]->(length $ elemT a1, concat $ replicate 2 $ elemT a1)], foldr1Fn ~> elemT.a1),
	-- Desc: sort
	-- Example: st"asdf" -> "adfs"
	lowPriorityExtendOp ["\\","\\"] genericReason ("st", [11, 11], [list], "sort" ~> a1),
	-- Desc: tbd
	-- Example: 0 -> 0
	extendOp ["\\","\""] genericReason ("tbd", [11,2], [], undefinedImpl),
	-- Desc: transpose
	-- Example: tr "hi""yo" -> ["hy","io"]
	-- Test mismatch dims: tr "hi""y" -> ["hy","i"]
	-- Test mismatch dims: tr "h""yo" -> ["hy","o"]
	-- Test 1 dim: tr "abc" -> ["a","b","c"]
	-- Test tuple, unzip it: tr z,3"abc" $ -> [1,2,3],"abc"
	extendOp ["\\","."] genericReason ("tr", [11,12], [list], \[a1] ->
		case a1 of
			VList [VList _] -> "transpose" ~> [a1]
			VList (_:_:_) -> unzipTuple a1
			otherwise -> "transpose.(:[])" ~> [VList [a1]]
		),
	-- Desc: splitWhen
	-- Example: sw "abc\nde  f " -~a$ $ -> [("","abc"),("\n","de"),("  ","f")]," "
	-- Test leading split: sw " a" -~a$ $ -> [(" ","a")],""
	-- Test snd ret isnt used implicitly: : sw ;"a$" -~a$ -> "a$"
	-- Test swapped: sw "abc\nde  f " ~-~a$ $ -> " ",[("","abc"),("\n","de"),("  ","f")]
	-- Test swapped leading split: sw " a" ~-~a$ $ -> "",[(" ","a")]
	op("sw", [], [list, AutoSwap, fn (elemT.a1)], \[a1,a2]->"\\a f->mySplitWhen ("++truthy (ret a2)++".f) a" ~> OptionalLets [VList [a1,a1], a1]),
	-- Desc: groupOn
	-- Example: gp ,5 /$2 -> [[1],[2,3],[4,5]]
	-- Test tuple: gp .,5~$/$2 @ -> [[(1,0)],[(2,1),(3,1)],[(4,2),(5,2)]]
	-- Test tuple ret: gp ,6 ~/$3 /$4 -> [[1,2],[3],[4,5],[6]]
	extendOp ["\\","&"] genericReason ("gp", [11,9], [list, fn (elemT.a1)],
		\[a1,a2]->"\\a f->groupBy (\\a b->f a==f b) a" ~> vList1 a1),
	-- Desc: reverse
	-- Example: \,3 -> [3,2,1]
	op("\\", [11], [list], "reverse" ~> a1),
	-- Desc: divmod
	-- Example: /~7 2 $ -> 3,1
	-- Test: /7 ~ -> 3
	op(["/","~"], [11,0], [AutoData num, AutoDefault num 2], "divMod" ~> [VInt, VInt]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(["/","~"], [11,0], [autoTodo num, list], undefinedImpl),
	-- Desc: divide
	-- Example: /7 2 -> 3
	-- Test: / *~2 7 -> -1
	-- Test: / *~2 *~7 -> 0
	-- Test: / 2 *~7 -> -1
	-- Test: / 7 ~ -> 3
	op("/", [11], [num, AutoDefault num 2], "div" ~> VInt),
	-- Desc: init
	-- Example: <~,5 -> [1,2,3,4]
	op(["<","~"], [11,0], [list], "init" ~> a1),
	-- Desc: take
	-- Example: <3,5 -> [1,2,3]
	-- todo test/make negative
	op("<", [11], [num, list], "take.fromIntegral" ~> a2),
	-- Desc: sort by
	-- Example: sb,4%$2 -> [2,4,1,3]
	-- Test tuple: sb z ,3 "bca" @ -> [(3,'a'),(1,'b'),(2,'c')]
	extendOp [",","."] genericReason ("sb", [13,12], [list, fn (elemT.a1)], "\\a f->sortOn f a" ~> a1),
	-- Desc: map
	-- Example: ."abc"+1$ -> "bcd"
	-- Test tuple: .,3~$*$$ -> [(1,1),(2,4),(3,9)]
	-- Test doesnt zip: ."ab".,2 :$ %@+$~ -> [[[1,1],[2,1]],[[1,0],[2,2]]]
	op(".", [12], [list, Fn True UnusedArg $ \[a1]->(1,elemT a1)], mapFn ~> VList .ret.a2),
	-- Desc: zip2 (incomplete)
	-- Example: ."abc",3 -> [('a',1),('b',2),('c',3)]
	--- todo, coerce dim length can do the vectorizing for us??
	op(".", [12], [list, Fn False UnusedArg $ \[a1]->(1,elemT a1)], (\[a1,a2]->"\\aa bb->zipWith (\\a b->"++flattenTuples (length$elemT a1) (length$elemT$head$ret a2) ++ "(a,b)) aa (bb())" ~> (VList $ elemT a1++(elemT$head$ret$a2)))),
	-- Desc: drop
	-- Example: >3,5 -> [4,5]
	-- Test more than size: >5,3 -> []
	-- Test: >~,3 -> [2,3]
	-- todo test/make negative
	op(">", [12], [AutoDefault num 1, list], "drop.fromIntegral" ~> a2),
	-- Desc: moddiv
	-- Example : %~7 2 $ -> 1,3
	-- Test: %~7 ~ -> 1
	op(["%","~"], [12,0], [AutoData num, AutoDefault num 2], "(swap.).divMod" ~> [VInt,VInt]),
	-- Desc: tbd
	-- Example: 0 -> 0
	op(["%","~"], [12,0], [num, list], undefinedImpl),
	-- Desc: modulus
	-- Example:  %7 2 -> 1
	-- Test: % *~2 7 -> 5
	-- Test: % *~2 *~7 -> -2
	-- Test: % 2 *~7 -> -5
	-- Test: % 7 ~ -> 1
	op("%", [12], [num, AutoDefault num 2], "mod" ~> VInt),
	-- Desc: chr/ord
	-- Example: ch 100 ch 'e' -> 'd',101
	-- Test: ch ~ -> '\NUL'
	extendOp [",",","] genericReason ("ch", [13,13], [AutoDefault num 0], "id" ~> xorChr.(VChr:)),
	-- Desc: chunksOf
	-- Example: rs2,5 -> [[1,2],[3,4],[5]]
	-- Test doesnt get swallowed by ?, : ?rs 1"...a.."~ a/$$ -> 4
	-- Test lazy: <3 rs2,^10 100 -> [[1,2],[3,4],[5,6]]
	-- Test: rs~,5 -> [[1,2],[3,4],[5]]
	extendOp [",","%"] genericReason ("rs", [13,9], [AutoDefault num 2, list], "chunksOf.fromIntegral" ~> vList1 .a2),
	-- Desc: nChunks
	-- Example: nc 2 ,6 -> [[1,2,3],[4,5,6]]
	-- Test: nc 2 ,5 -> [[1,2,3],[4,5]]
	-- Test: nc ~ ,5 -> [[1,2,3],[4,5]]
	extendOp [",","^"] genericReason ("nc", [13,14], [AutoDefault int 2, list], "\\a b->chunksOf (ceiling $ fromIntegral (length b) / fromIntegral a) b" ~> vList1 . a2),
	-- Desc: length
	-- Example: ,:3 4 -> 2
	op(",", [13], [list], "length" ~> VInt),
	-- Desc: range from 0 ...
	-- Example: ,~3 -> [0,1,2]
	-- Test: <3,~~ -> [0,1,2]
	op([",","~"], [13, 0], [AutoDefault num (2^128)], "\\x->[0..x-1]" ~> vList1 . a1),
	-- Desc: range from 1 to
	-- Example: ,3 -> [1,2,3]
	-- Test: ,*~3 -> []
	op(",", [13], [num], "\\x->[1..x]" ~> vList1 .a1),
	-- Desc: exponentiation
	-- todo test/make negative
	-- Example: ^2 8 -> 256
	-- Test: ^~ 1 -> 10
	-- Test: ^8 ~ -> 64
	-- Test: ^2 *~3 -> 0
	-- Test: ^0 0 -> 1
	-- todo handle 0**-3 (maybe should be infinity?)
	op("^", [14], [AutoDefault int 10, AutoDefault num 2], "\\a b->if b<0 then 0 else a^b" ~> a1),
	-- Desc: replicate
	-- Example: ^"ab"3 -> "ababab"
	-- Test: ^"ab" *~3 -> ""
	-- Test: ^'a' 3 -> "aaa"
	-- Test: <3^"ab" ~ -> "aba"
	op("^", [14], [orC list char, AutoDefault int (2^128)], \[a1,_] ->
		let (ap1, apf) = promoteList [a1] in
		"(flip$(concat.).genericReplicate)."++apf ~> ap1),
	-- Desc: inits
	-- Example: is,3 -> [[],[1],[1,2],[1,2,3]]
	extendOp ["=","~"] genericReason ("is", [14,0], [list], "inits"~>VList),
	-- Desc: subscript. Wrapped.
	-- Example: =2 "asdf" -> 's'
	-- Test 0 (wrapped): =0 "asdf" -> 'f'
	-- Test empty: =0"" -> ' '
	op("=", [14], [int, list], \[a1,a2]->"\\i a->if null a then "++defaultValue (elemT a2)++"else lazyAtMod a (fromIntegral i - 1)" ~> elemT a2),
	-- Desc: zip
	-- Example: z,3"abc" -> [(1,'a'),(2,'b'),(3,'c')]
	-- Test: .z,3,3+@$ -> [2,4,6]
	-- Test 3 tuple: .z z,3,3,3++_@$ -> [3,6,9]
	-- Test 3 tuple: z,3 z,3"abc" -> [(1,1,'a'),(2,2,'b'),(3,3,'c')]
	op("z", [14], [list, list], (\[a1,a2]->"zipWith (\\a b->"++flattenTuples (length$elemT a1) (length$elemT a2) ++ "(a,b))") ~> (VList .(concatMap elemT) :: [VT] -> VT)),
	-- Desc: hash (md5) mod
	-- Example: hm~ "5a" 100 -> 62
	-- Test: . tb 16 hm "asdf" 0 =$"123456789abcdef0" -> "912ec803b2ce49e4a541068d495ab570"
	-- Test: hm "d" 256 -> 173
	-- Test: hm :100~ 256 -> 173
	extendOp ["?","~"] genericReason ("hm", [15,0], [auto, anyT, int {-ParseArg "int" intParser-}], (\[a1,a2]->"\\a b->(fromIntegral$hlist$"++flatten a1++"$a)`mod`(if b==0 then 2^128 else b)") ~> a2),
	-- Desc: data salted hashmod
	-- untested example: hm "5" 100 97 -> 62
	extendOp ["?","~"] genericReason ("hm", [15,0], [anyT, ParseArg "int" intParser], (\[a1,a2]->do
		modify $ \s -> s { pdDataUsed = True }
		return $ "\\a b->(fromIntegral$hlist$("++flatten a1++")a++toBase 256 dat)`mod`(if b==0 then 2^128 else b)" ~> a2)::[VT]->ParseState (VT,String)),
 	-- Desc: to base
 	-- Example: tb 2 10 -> [1,0,1,0]
 	-- Test: tb 2 0 -> []
 	-- Test: tb ~ 10 -> [1,0,1,0]
 	extendOp ["?",litDigit] genericReason ("tb", [15,1], [AutoDefault num 2, AutoData num], "toBase"~>vList1 .a1),
 	-- Desc: from base
 	-- Example: fb 2 :1 :0 :1 0 -> 10
 	-- Test: fb 2 <0,3 -> 0
 	-- Test: fb ~ :1 :0 :1 0 -> 10
 	extendOp ["?",litDigit] genericReason ("fb", [15,1], [AutoDefault num 2, list {-todo 1d-}], "fromBase"~>a1),
 	-- Desc: tbd
 	-- Example: 0 -> 0
 	extendOp ["?",litDigit] genericReason ("tbd", [15,1], [list], undefinedImpl),
	-- Desc: if nonnull (lazy)
	-- Example: ?,:5 5 1 0 -> 1
	-- Test: ?,:"" "" 1 0 -> 0
	-- Test: ?,:5 5 $ 0 -> [5,5]
	-- todo, the arg passed in should be marked optional used
	lowPriorityOp(["?",","], [15,13], [list, Fn False OptionalArg $ \a1 -> (1,a1), fnx (\[a1,a2]->(length$ret a2,[]))], \ts -> let (coercedType, coerceFn) = coerceEither (ret$ts!!1) (ret$ts!!2) in
		"\\c a b->"++ coerceFn ++ "$ iff (not (null c)) (a c) (b())" ~> coercedType
		),
	-- Desc: if/else
	-- Example: ? +0 0 "T" "F" -> "F"
	-- Test coerce: ? +1 0 1 "F" -> "1"
	-- Test mult rets: ? +0 0 ~1 2 3 4 $ -> 3,4
	-- Test using cond value: ? +1 0 $ 5 -> 1
	-- Test cond value doesn't clobber implicit: ;5 ? +1 0 -> 5,5
	-- todo auto value "nothing" which coerces usefully
	-- todo match number of rets in false clause (similarly to what should be done for :)
	-- todo make clauses 1 fn, that way they could share let statements
	op("?", [15], [num, Fn False OptionalArg $ \a1 -> (1,a1), Fn False OptionalArg (\[a1,a2]->(length$ret a2,[a1]))], \ts -> let (coercedType, coerceFn) = coerceEither (ret$ts!!1) (ret$ts!!2) in
		"\\c a b->"++coerceFn ++ "$ (iff."++truthy [a1 ts]++") c (a c) (b c)" ~> coercedType
		),
	-- Desc: add w/ cast
	-- todo return Maybe
	-- todo this blocks index by on string which is probably ok, but not ideal
	-- Example: +"10" 2 -> 12
	-- Test: +"10"~ -> 10
	op("+", [15], [str, AutoDefault int 0], "(+).read.aToS" ~> VInt),
	-- Desc: index by
	-- Example: ?,100~ -*$$80 -> 9
	op("?", [15], [list, auto, fn (elemT.a1)], (\[a1,a2]->"\\l f->1+(fromMaybe (-1) $ findIndex ("++truthy (ret a2)++".f) l)") ~> VInt),
	-- Desc: index. Or 0 if not found.
	-- Example: ?  :3:4 5  4 -> 2
	-- Test not found: ? ,3 4 -> 0
	-- Test tuple: ? z ,3 "abc" 2 -> 2
	op("?", [15], [listToBeReferenced, elemOfA1], \[a1,a2]->"\\a e->1+(fromMaybe (-1) $ elemIndex e (map "++firstOf (elemT a1)++" a))" ~> VInt),
	-- Desc: diff
	-- Example: -"abcd" "bd" -> "ac"
	-- Test non existant elements: -"abc" "de" -> "abc"
	-- Test doesn't drop all: -"aa" "a" -> "a"
	op("-", [15], [listToBeReferenced, sameAsA1], "\\\\" ~> a1),
	
	-- todo there are some type combinations that are invalid for bin 15
	-- diff could work with non matching tuples too, aka diff by?
	
	-- Desc: debug arg type
	-- Example: pt 5 -> error "VInt"
	op("pt", [], [anyT], "" ~> errorWithoutStackTrace.show.a1 :: ([VT]->[VT],String)),
	-- Desc: show
	-- Example: p"a" -> "\"a\""
	op("p", [], [anyT], inspect.a1 ~> vstr),
	-- Desc: debug context types
	-- Example: ;5 ct -> error "$ LetArg VInt ..."
	op("ct", [], [], gets pdContext >>= parseError . debugContext :: ParseState Impl),
	-- Desc: error
	-- Example: error p +2 1 -> error "3"
	op("error", [], [str], "errorWithoutStackTrace.aToS" ~> vstr),

	op("testCoerce2", [], [anyT, anyT], testCoerce2 ~> vstr),
	op("testCoerceToInt", [], [anyT], testCoerceTo [VInt]),
	op("testCoerceToChr", [], [anyT], testCoerceTo [VChr]),
	op("testCoerceToListInt", [], [anyT], testCoerceTo [VList [VInt]]),
	op("testCoerceToStr", [], [anyT], testCoerceTo [vstr]),
	op("testCoerceToListListInt", [], [anyT], testCoerceTo [VList [VList [VInt]]]),
	op("testCoerceToListStr", [], [anyT], testCoerceTo [VList [vstr]]),
	op("testCoerceToX", [], [fn noArgs, fn noArgs], \ts -> "\\a b->" ++ snd (testCoerceTo (ret $ a1 ts) (ret $ a2 ts)) ++ "(b())" ~> (ret $ a1 ts)),
	op("testFinish", [], [anyT], finish.a1 ~> vstr)]

foldr1Fn :: [VT] -> String
foldr1Fn = (\[a1,a2]->"\\a f->if null a then "++defaultValue (elemT a1)++" else foldr1 (\\x y->"++coerceTo (elemT a1) (ret a2)++"$f$"++flattenTuples(length $ elemT a1)(length $ elemT a1)++"(x,y)) a")
mapFn :: [VT] -> String
mapFn = (\[a1,a2]->"(\\a f->map f a)")

addHigherValueDeBruijnOps ops = concat [op(
		replicate unary ';' ++ snd symb,
		replicate unary 6 ++ [2+fst symb],
		[],
		argn (unary*3+fst symb))
	| unary <- [1..10]
	, symb <- [(1,"$"),(2,"@"),(3,"_")]
	] ++ map convertNullNib ops

allOps = addHigherValueDeBruijnOps $ concat rawOps
simpleOps = addHigherValueDeBruijnOps $ filter isOpSimple $ map last rawOps