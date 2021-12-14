{-# LANGUAGE FlexibleContexts #-} -- for binarySetOp type
module Ops where

import Types
import Polylib
import Expr
import Parse
import Args
import Hs
import OpsHelper

import State
import Data.List(concat,sortOn)
import Data.Maybe

tildaRep = (head $ head $ snd tildaOp, head $ fst tildaOp)
intRep = ('\0', 1)
strRep = ('"',2)
setRep = (';',6)
consRep = (':',7)
sumRep = ('+',8)
mapRep = ('.',12)
filterRep = ('|',9)
subscriptRep = ('=',9)
reverseRep = ('\\',11)
lengthRep = (',',13)
rangeRep = lengthRep
repRep = ('^',14)
addRep = sumRep
multRep = ('*',10)
takeRep = ('<',11)
dropRep = ('>',12)
ifRep = ('?',15)
diffRep = ('-',15)

rawOps :: [[(Bool, [String], [Int], Operation)]]
rawOps = [
	-- Desc: auto value
	-- Example (size 4): +4~ -> 5
	-- This is only here so that auto handling can use memoized args after.
	-- and so that if ops check type it won't match
	op(tildaRep, [], (error"undefined auto impl"::String)~>InvalidType), 
	-- Desc: integer
	-- Example (size 2): 3 -> 3
	-- Test (size 2): 0 -> 0
	-- Test (size 2): 1 -> 1
	-- Test (size 3): 7 -> 7
	-- Test (size 3): 8 -> 8
	-- Test (size 2): 10 -> 10
	-- Test (size 3): 20 -> 20
	-- Test leading zero is separate: :05 -> [0,5]
	-- Test negative (size 3): -1 -> -1
	-- Test negative (size 3): -8 -> -8
	-- Test negative (size 4): -9 -> -9
	-- Test negative (size 4): -7 -> -7
	-- Test negative (size 3): -10 -> -10
	op(intRep, [ParseArg "int" intParser], ()),
	-- Desc: string
	-- Example (size 6): "hi\n" -> "hi\n"
	-- Test space (size 2): " " -> " "
	-- Test empty (size 3): "" -> ""
	-- Test escapes: "\"" -> "\""
	-- Test binary (size 5): "\200" -> "\200"
	-- Test list of strs (size 5) : """a" -> ["","a"]
	-- Test list of strs (size 7) : "a""" -> ["a",""]
	-- Test list of strs (size 7) : "a""b" -> ["a","b"]
	op(strRep, [ParseArg "str" strParser], ()),
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
	extendOp "'" [lengthRep, strRep] (shorterReason"just hardcode the length of the string") ([ParseArg "chr" chrParser], ()),
	-- Desc: 1st arg
	-- Example: ;1;2;3 $ -> 1,2,3,3
	-- Test: ;1;2;3;4 ;$ -> 1,2,3,4,1
	-- Test: ;1;2;3;4;5;6;7 ;;$ -> 1,2,3,4,5,6,7,1
	op(('$', 3), [], argn 1),
	-- Desc: 2nd arg
	-- Example: ;1;2;3 @ -> 1,2,3,2
	op(('@', 4), [], argn 2),
	-- Desc: 3rd arg
	-- Example: ;1;2;3 _ -> 1,2,3,1
	op(('_', 5), [], argn 3),
	-- Desc: let fn (todo this needs a new home for its rep)
	-- Example: ;~2+1$ $4 -> 3,5
	-- Test (multiple args): ;~~1 2 +@$ $4 5 -> 3,9
	-- Test (multiple returns): ;~1 ~$3 $ @4 $ -> 1,3,4,3
	-- Test (mult args and rets): ;~~1 2 ~+$~+@~ $ @3 4 $ -> 2,3,4,5
	-- Test (coerce arg): ;~2+1$ $"4" -> 3,5
	-- Test (coerce pair): ;~~1 2 +@$  $"5"2 -> 3,7
	-- Test deps: .,2;~2+@$ -> [3,4]
	opM(";~",[6,0], [AnyS, Fn ReqArg UnusedArg $ \[a1]->(1,ret a1)],
		(\[a1,a2]->
			let a1L = length $ ret a1
			    a2L = length $ ret a2 in
			"\\x f->"++flattenTuples a2L 1 ++ "(f $ x(),f)" ~>
			ret a2 ++ [VFn (ret a1) (ret a2)]
			)),
	-- Desc: equal?
	-- todo consider returning list of the value or empty rather than 1 0
	-- Example: == 1 2 == 1 1 -> 0,1
	-- Test coerce: == 1 "1" -> 1
	opM("==",[6,0],[AnyS, Fn ReqConst UnusedArg $ \[a1]->(1,ret a1)],\[a1,a2]->
		let (coercedType, coerceFnA, coerceFnB) = coerce (ret a1) (ret a2) in
			"\\a b->bToI$"++coerceFnA++"(a())=="++coerceFnB++"b" ~> VInt),

	-- Desc: recursion
	-- Example (fact): `; 5 $ 1 *@-$~$ -> 120
	-- Test (multiple args): `; ~3 4 $ 0 +_@ -$1 @ -> 12
	-- Test (multiple rets): `; 1 $ ~3 7 +@0 @ $ $ -> 4,7
	-- Test (quicksort): `;"hello world!"$$:@|$-/@$$:|$-~^-/@$$~@|$-$/@$ -> " !dehllloorw"
	-- Test (coerce rec ret): `; 5 1 1 "2" -> 2
	-- Test memoize: `; 100 $ 1 +@-$2 @-$1 -> 927372692193078999176
	-- Test memoize tuple: `; ~1 2 0 @ 5 -> 2
	-- Test not cond: `; 5 ~$ 1 0 -> 1
	extendOp "`;" [setRep,setRep] (shorterReason"multiple assignments on the same thing is useless, just use 1") ([AnyS, fnx (\[a1]->(0, ret a1++[InvalidType]))],
	\[a1,a2]->
		"\\x f -> memoFix (\\rec x->let (a,b,c)=f ("++flattenTuples (length $ ret a1) 1++"(x,rec)) in if a then c else b) (x())" ~> ret (head $ tail $ ret a2)),
	-- Desc: let
	-- Example: + ;3 $ -> 6
	-- Test: ++; 3 ; 2 $ -> 7
	-- Test: ++; 3 ; 2 @ -> 8
	-- Test: ++; +5 0 /,1 $ $ -> 11
	--- Test: ++; +5 0 /,2 _ $ -> 15
	--- Test: ++; + +5 0 0 /,1 ;7 $ -> 13
	-- Test: ++; +++5 0 0 0 /,1 ;+0$ $ -> 11
	-- Test: ++;2 ;1 @ -> 5
	-- Test: .,3 ;%$3 -> [1,2,0]
	-- Test: +++0 0;1 ;+2$ -> 4
	op(setRep, [any1], "\\x->(x,x)" ~> dup.a1),
	-- Desc: singleton
	-- Example: :3~ -> [3]
	-- Test tuple: :~1 2~ -> [(1,2)]
	op(consRep, [AnyS, auto], "\\v->v():[]" ~> VList .ret.a1),
	-- Desc: append (todo consider ~ prepend default)
	-- Example: :"abc" "def" -> "abcdef"
	-- Test coerce: :"abc"1 -> "abc1"
	-- Test coerce: :1"abc" -> "1abc"
	-- Test tuple: : z,1"a" z,1"d" -> [(1,'a'),(1,'d')]
	-- Test tuple ~ happy path: :~1'a' z,3"abc" -> [(1,'a'),(1,'a'),(2,'b'),(3,'c')]
	--- todo make work Test: ~1'a' 2'b' -> [(1,'a'),(2,'b')]
	-- Test promoting to list: :1 2 -> [1,2]
	op(consRep, [AnyS, AnyS], \[a,b]->
		let
			(ap,apFn) = promoteList (ret a) -- FYI : int chr extension relies on this coerce behavior
			(coercedType, coerceFnA, coerceFnB) = coerce [ap] (ret b)
		in
			"\\a b->("++coerceFnA++"$"++apFn++"(a()))++"++coerceFnB++"(b())"~>coercedType
		),
	-- Desc: signum
	-- Example: `$ *~1 `$ 0 `$ 2 -> -1,0,1
	extendOp "`$" [lengthRep, consRep] (shorterReason"just add 1 to the length") ([autoTodo {- -1? -} num], "signum" ~> VInt),
		
	commutativeExtension [8]
		-- Desc: max
		-- Example: ]4 5 -> 5
		-- Test: ]~ *~4 -> 0
		-- todo if change, update it's lit warning catchall at end
		("]", [AutoDefault num 0, num], "max"~>orChr)
		-- Desc: add
		-- Example: +2 1 -> 3
		-- Test: +'a' 2 -> 'c'
		-- Test: +'\n' '\n' -> 20
		-- Test vectorized: +1,3 -> [2,3,4]
		-- Test 2d vectorized: +1 ^2 :,2~ -> [[2,3],[2,3]]
		-- Test string vectorized: +1"abc" -> "bcd"
		-- Test char vectorized: +'a' :1 2 -> "bc"
		-- Test vectorized tuple: +1 z,3"abc" -> [(2,'b'),(3,'c'),(4,'d')]
		-- Test: +3 ~ -> 4
		-- Test: +3 3 -> 6
		("+", [AutoDefault num 1, AutoDefault vec 1], vectorize "+" xorChr),

	-- Desc: split. Removing empties.
	-- Example: %"a b c" " " -> ["a","b","c"]
	-- Test empties: %" a  b " " " -> ["a","b"]
	-- Test empty: %"" "a" -> []
	-- Test empty div: %"abc" "" -> ["a","b","c"]
	-- Test chr split: %"a b" ' ' -> ["a","b"]
	-- Test auto (words): %"a\nb c.d"~ -> ["a","b","c.d"]
	op(('%',8), [str, OrAuto "words" $ orC str char], \[a1,a2]->
		(if a2==OptionYes then "\\a->map sToA $ words (aToS a)" else
		let (ap2, apf) = promoteList [a2]
		in "(\\a b->filter (/=[])$splitOn ("++apf++"b) a)") ~> vList1 a1),
	
	-- words steals the auto value from int, but justify doesn't need it
	
	-- Desc: justify
	-- negative number of ljust instead of right
	-- ~ before the obj to be justified to center (l/r determines rounding)
	-- Lists vectorize (and also maxes their length with n)
	
	-- Example: & " " 4 "hi" -> "  hi"
	-- Test ljust: & " " *~4 "hi" -> "hi  "
	-- Test center: & " " 5 ~"hi" -> " hi  "
	-- Test center left: & " " *~5 ~"hi" -> "  hi "
	-- Test coerce: & " " 4 5 -> "   5"
	-- Test list: & " " 0 >7,10 -> [" 8"," 9","10"]
	-- Test ljust list: & " " *~1 >7,10 -> ["8 ","9 ","10"]
	op(('&',8), [str, int, AutoOption "center", vec], \[a1,a2,o1,a3] ->
		let justify t = 
			"\\s n o->let oc = "++coerceTo [vstr] [t]++"o ;\
						    \pad = concat (genericReplicate (abs n-genericLength oc) s) in " ++
			(if o1 == OptionYes then
				"let (smallPad,bigPad)=splitAt (length pad `div` 2) pad in \
				\if n >= 0 then smallPad ++ oc ++ bigPad else bigPad ++ oc ++ smallPad"
			else
				"if n >= 0 then pad ++ oc else oc ++ pad") in
		if isList a3 && a3 /= vstr then
			"\\s n o->let ocs = map "++coerceTo [vstr] (elemT a3)++" o;\
			            \ nmax = (if n<0 then -1 else 1) * (maximum $ abs n : map genericLength ocs) in \
			            \ map ((" ++ justify vstr ++ ")s nmax) ocs"  ~> vList1 vstr
		else justify a3 ~> vstr),
	-- Desc: join
	-- Example: *" ",3 -> "1 2 3"
	-- Test 2d: *" "^2:,3~ -> ["1 2 3","1 2 3"]
	-- Test tuple: *" "z,3"abc" -> ["1 a","2 b","3 c"]
	-- Test lopsided tuple: *" "z^2:,2~"ab" -> [("1 2",'a'),("1 2",'b')]
	-- Test: *" "z^2:,2~ ^2:,2~ -> [("1 2","1 2"),("1 2","1 2")]
	op(('*',8), [str, list], Polylib.join.a2),
	-- Desc: product
	-- Example: `*,4 -> 24
	-- Test: `*,0 -> 1
	-- Test tuple: `* z,4 "abcd" $ -> 24,"abcd"
	opM("`*", [], [listOf int], \[a1]->let (uzT,uzF)=unzipTuple a1 in
			appFst uzT "product" ++ "." ++ uzF ~> VInt : tail uzT
		),
	-- Desc: sum
	-- Example: +,3 -> 6
	-- Test empty: +,0 -> 0
	-- Test tuple: +z ,3 "abc" $ -> 6,"abc"
	op(sumRep, [listOf int], \[a1]->
		let (uzT,uzF)=unzipTuple a1 in
			appFst uzT "sum" ++ "." ++ uzF ~> VInt : tail uzT
		),
	-- Desc: concat
	-- Example: +.,3,$ -> [1,1,2,1,2,3]
	-- Test tuple: +^2:z ,2 "ab"~ -> [(1,'a'),(2,'b'),(1,'a'),(2,'b')]
	-- Test tuple2: +z ^2:,2~ "ab" $ -> [1,2,1,2],"ab"
	-- \a -> (concat (map fst a), map snd a)
	op(sumRep, [listOf list], \[a1]->let (uzT,uzF)=unzipTuple a1 in
			appFst uzT "concat" ++ "." ++ uzF ~> head (elemT (head uzT)) : tail uzT
		),

	commutativeExtension [12,0]
		-- Desc: abs diff
		-- Example: != 3 5 -> 2
		-- Test auto: != ~ -3 -> 3
		("!=", [AutoDefault num 0,num], "(abs.).(-)" ~> VInt)
		-- Desc: bit intersection
		-- Example: `& 6 3 -> 2
		-- Test chr: `& 'q' 'e' -> 'a'
		-- Test auto: `& 3 ~ -> 1
		("`&", [num,AutoDefault num 1],".&."~>orChr),

	-- Desc: groupBy
	-- Example: `= ,5 /$2 -> [[1],[2,3],[4,5]]
	-- Test tuple: `= .,5~$/$2 @ -> [[(1,0)],[(2,1),(3,1)],[(4,2),(5,2)]]
	-- Test tuple ret: `= ,6 ~/$3 /$4 -> [[1,2],[3],[4,5],[6]]
	extendOp "`=" [reverseRep, filterRep] equivalentOrderReason ([list, Fn ReqArg UnusedArg $ \[a1]->(1,elemT a1)], \[a1,a2]->"\\a f->groupBy (onToBy f) a" ~> vList1 a1),

	-- Desc: to uppercase
	-- Example: `> 'a' -> 'A'
	-- Test: ."Hi there!"`>$ -> "HI THERE!"
	opM(["`",">"], [12], [char, BinCode 2], "myOrd.toUpper.myChr"~>a1),
	-- Desc: to lowercase
	-- Example: `< 'A' -> 'a'
	-- Test: ."Hi there!"`<$ -> "hi there!"
	opM(["`","<"], [12], [char, BinCode 7], "myOrd.toLower.myChr"~>a1),
	-- Desc: divmod
	-- Example: `/7 2 $ -> 3,1
	-- Test: `/7 ~ $ -> 3,1
	extendOp "`/" [subscriptRep] (shorterReason"this could be accomplished with mod after length") ([num, BinCodeRep rangeRep,AutoDefault num 2], "safeDivMod" ~> [VInt, VInt]),
	-- Desc: subtract
	-- Example: - 5 3 -> 2
	-- Test: -'b''a' -> 1
	-- Test: -'d'1 -> 'c'
	-- Test: -~2 -> -1
	-- Test: - 2~ -> 1
	op(('-',9), [AutoDefault num 1, AutoDefault num 1], "-" ~> xorChr),
	-- Desc: inits
	-- Example: `>,3 -> [[],[1],[1,2],[1,2,3]]
	opM("`>", [9,0], [list], "inits"~>VList),

--		-- Test nowrap: =~5"asdf" -> ' '
-- 	op("=", [], [AutoOption "nowrap", int, list], \[o1,a1,a2]->(if o1==OptionYes
-- 		then "\\i a->fromMaybe "++defaultValue (elemT a2)++" $ at a (fromIntegral i)"
-- 		else "\\i a->if null a then "++defaultValue (elemT a2)++"else lazyAtMod a (fromIntegral i - 1)") ~> elemT a2),

	
	-- Desc: subscript. Wrapped. todo maybe should use default or provide another option?
	-- Example: =2 "asdf" -> 's'
	-- Test 0 (wrapped): =0 "asdf" -> 'f'
	-- Test empty: =0"" -> ' '
	op(subscriptRep, [num, list], \[a1,a2]->"\\i a->if null a then "++defaultValue (elemT a2)++"else lazyAtMod a (fromIntegral i - 1)" ~> elemT a2),
	-- Desc: partition
	-- Example: |,5~~%$2 $ -> [1,3,5],[2,4]
	-- Test notted: |,5~~~%$2 $ -> [2,4],[1,3,5]
	op(filterRep, [list, auto, auto, AutoNot $ fn (elemT.a1)], \[a1,a2]->"\\a f->partition f a" ~> [a1::VT,a1]),
	-- Desc: filter
	-- Example: |,5%$2 -> [1,3,5]
	-- Test chr truthy: |"a b\nc"$ -> "abc"
	-- Test list truthy: |"""b"$ -> ["b"]
	-- Test tuple: | z,3 "abc" /$2 -> [(2,'b'),(3,'c')]
	-- Test auto not: |,5~%$2 -> [2,4]
	op(filterRep, [list, AutoNot $ Fn ReqArg UnusedArg $ \[a1]->(1,elemT a1)], "\\a f->filter f a" ~> a1),
	-- Desc: zipWith (todo ops don't handle tuples well) todo test vectorize on ~ with scalar, todo also, could use auto value for another special thing)
	-- Example: ! ,3"abc" + -> "bdf"
	-- Test: ! ,3"abc" , -> [(1,'a'),(2,'b'),(3,'c')]
	-- Test 3tuple: ! z,3"abc" ,3 , -> [(1,'a',1),(2,'b',2),(3,'c',3)]
	-- Test tuple const: ! "abc" 1 , -> [('a',1),('b',1),('c',1)]
	-- Test arbitrary fn: ! ,3 "abc" ~ ++1@$ -> "ceg"
	-- Test arbitrary fn tuple: ! ,3 z,3"abc" ~ ++_@$ -> "cfi"
	-- Test append coerce: ! ,3 "abc" : -> [[1,97],[2,98],[3,99]]
	-- Test append cons: ! ,3 4 : -> [[1,4],[2,4],[3,4]]
	-- Test append cons coerce: ! "abc" 3 : -> ["a3","b3","c3"]
	-- Test vec: ! "abc" .,3,3 + -> ["bdf","bdf","bdf"]
	-- Test double vec: ! "ab" .,2.,2,2 + -> [["bd","bd"],["bd","bd"]]
	-- Test anti vec: ! "abc""def" ,3 + -> ["bdf","egi"]
	-- Test subscript: ! "abc"\,3 = -> "cba"
	-- Test vec subscript: ! "abc""abc""def",3 = -> "abf"
	-- Test antivec subscript: ! "abc".,3,3 = -> ["abc","abc","abc"]
	-- Test: !,2 2+ !,2 2* !,2 2- !,2 2/ !,2 2% !,2 2^ !,2 2] !,2 2[ !,2 2! -> [3,4],[2,4],[-1,0],[0,1],[1,0],[1,4],[2,2],[1,2],[1,0]
	-- Test: !"abc" "ccb"? -> [3,3,2]
	op(('!',snd filterRep), [list, Fn ReqConst UnusedArg $ \[a1]->(1,elemT a1), ZipMode], \[a1,a2,rt]->"\\a b f->f a b" ~> ret rt),
	-- Desc: char class?
	-- Example: \'z'a -> "z"
	-- Test: \' 'a -> ""
	-- Test: \'a'$ \'_'$ -> "","_"
	-- Test: \'a'! \'_'! -> "a",""
	-- Test: \'a'A \'a'n \'a'N \'a's \'a'S \'a'l \'a'L \'a'u \'a'U \'a'p \'a'P \'a'd \'a'D \'a'$ \'a'! -> "","a","","","a","a","","","a","a","","","a","","a"
	op(('\\',10), [char, CharClassMode], "\\a f->if f $ myChr a then [a] else []" ~> vstr),

	commutativeExtension [10]
		-- Desc: min
		-- Example: [4 5 -> 4
		("[", [int, num], "min"~>orChr)
		-- Desc: multiply
		-- Example: *7 6 -> 42
		-- Test: *2 "dd" -> [200,200]
		-- Test: *~ 5 -> -5
		-- Test: *5 ~ -> 10
		("*", [AutoDefault int (-1), AutoDefault vec 2], vectorize "*" (const VInt)),
		
	-- Desc: scanl1 tuple
	-- Example: =\,3 ~1 2 +*2$_ 3 -> [(1,2),(4,3),(7,3),(9,3)]
	extendOp "=\\" [lengthRep, mapRep] (shorterReason"just take length without map") ([nonTupleList, auto, fn2 (const []), fnx (\[a1,a2]->(length $ ret a2, elemT a1 ++ ret a2))], foldrFn "(scanl . flip)" ~> VList .ret.a2),
	
	-- Desc: scanl
	-- Example: =\,3 0 +@$ -> [0,1,3,6]
	extendOp "=\\" [lengthRep, mapRep] (shorterReason"just take length without map") ([nonTupleList, Fn ReqArg UnusedArg $ \[a1]->(1,concat $ replicate 2 $ elemT a1)], (\[a1,a2]->"\\a f->scanl1 (\\x y->"++coerceTo (elemT a1) (ret a2)++"$f$"++flattenTuples(length $ elemT a1)(length $ elemT a1)++"(y,x)) a") ~> VList .elemT.a1),

	-- Desc: scanl1
	-- Example: =\,3+*2$@ -> [1,5,11]
	-- Test empty: =\,0+@$ -> []
	extendOp "=\\" [lengthRep, mapRep] (shorterReason"just take length without map") ([nonTupleList, Fn ReqConst UnusedArg $ \[a1]->(1,concat $ replicate 2 $ elemT a1), fn (\[a1,a2]->elemT a1 ++ ret a2)], (\[a1,a2,a3]->
		"\\a i f->scanl (\\x y->"++coerceTo (ret a2) (ret a3)++"$f(y,x)) i a" ~> VList (ret a2))),
	
	-- Desc: scanl list tuple
	-- Example: =\ z ,3 "abc" ~0 ++_@$ -> [0,98,198,300]
	extendOp "=\\" [lengthRep, mapRep] (shorterReason"just take length without map") ([list, auto, AnyS, fnx (\[a1,a2]->(length $ ret a2, elemT a1 ++ ret a2))], foldrFn "(scanl . flip)" ~> VList .ret.a2),
	-- Desc: scanl1 list tuple
	-- Example: =\ z ,3 "a.c" +_$ +,\@a;$ -> [(1,'a'),(3,'a'),(6,'b')]
	extendOp "=\\" [lengthRep, mapRep] (shorterReason"just take length without map") ([list, fnx $ \[a1]->(length $ elemT a1, concat $ replicate 2 $ elemT a1)], (\[a1,a2]->"\\a f->scanl1 (\\x y->"++coerceTo (elemT a1) (ret a2)++"$f$"++flattenTuples(length $ elemT a1)(length $ elemT a1)++"(y,x)) a") ~> VList .elemT.a1),

	-- Desc: foldr1tuple
	-- Example: /,3 ~1 2 _ @  $ -> 2,1
	-- Test triple: /,3 ~~1 2 3 ;$ _ @  $ @ -> 3,2,1
	-- Test: / ,3 ~0 "" +@$ :$_ $ -> 6,"123"
	op(('/',10), [nonTupleList, auto, fn2 (const []), fnx (\[a1,a2]->(length $ ret a2, elemT a1 ++ ret a2))], foldrFn "foldr" ~> ret.a2),
	
	-- Desc: foldr
	-- Example: /,3 1 +@$ -> 7
	op(('/',10), [nonTupleList, Fn ReqArg UnusedArg $ \[a1]->(1,concat $ replicate 2 $ elemT a1)], foldr1Fn ~> elemT.a1),

	-- Desc: foldr1
	-- Example: /,3+@$ -> 6
	-- Test empty: /,0+@$ -> 0
	-- Test coerce: / ,3 0 "5" -> 5
	op(('/',10), [nonTupleList, Fn ReqConst UnusedArg $ \[a1]->(1,concat $ replicate 2 $ elemT a1), fn (\[a1,a2]->elemT a1 ++ ret a2)], (\[a1,a2,a3]->
		"\\a i f->foldr (\\x y->"++coerceTo (ret a2) (ret a3)++"$f(x,y)) i a" ~> ret a2)),

	-- Desc: foldr list tuple
	-- Example: / z ,3 ,3 ~ 1 ++_@$ -> 13
	op(('/',10), [list, auto, AnyS, fnx (\[a1,a2]->(length $ ret a2, elemT a1 ++ ret a2))], foldrFn "foldr" ~> ret.a2),
	-- Desc: foldr1 list tuple
	-- Example: /z"acb" ,3 ]$_ +;$@  $ -> 'c',6
	-- Test coerce: / z "acb" ,3 100 99 $ -> 'd',99
	-- Test empty: / z "" ,3 ]$_ +;$@  $ -> ' ',0
	op(('/',10), [list, fnx $ \[a1]->(length $ elemT a1, concat $ replicate 2 $ elemT a1)], foldr1Fn ~> elemT.a1),
	-- Desc: sort
	-- Example: `<"asdf" -> "adfs"
	-- FYI rep is used for hex extension too!
	opM("`<", [14], [list, BinCode 13], "sort" ~> a1),
	-- Desc: iterate while uniq
	-- Example: `. 10 %+1$3 -> [10,2,0,1]
	-- Test never stop: <5 `. 10 ~1 -> [10,1,1,1,1]
	-- Test tuple: `. ~1 2 @$ -> [(1,2),(2,1)]
	-- Test lazy: <1 `. ~4 5 ? $ 0 error "not lazy" -> [(4,5)]
	extendOp "`." [reverseRep, strRep] (shorterReason"just write the string in reverse") ([AnyS, AutoOption "inf", fnx (\[a1,o1]->(length $ ret a1, ret a1))],
		\[a1,o1,a2]->"\\i f->"++(if o1==OptionYes then "iterate" else "iterateWhileUniq") ++"("++coerceTo (ret a1) (ret a2)++".f) (i())" ~> VList (ret a1)),
	-- Desc: transpose (maybe this should even be 1 nib... very common for 2d lists and list of tuples).
	-- Example: `' "hi""yo" -> ["hy","io"]
	-- Test mismatch dims: `' "hi""y" -> ["hy","i"]
	-- Test mismatch dims: `' "h""yo" -> ["hy","o"]
	-- Test 1 dim: `' "abc" -> ["a","b","c"]
	-- Test tuple, unzip it: `' z,3"abc" $ -> [1,2,3],"abc"
	opM("`'",[14],[list, BinCode 3], \[a1] ->
		case a1 of
			VList [VList _] -> "transpose" ~> [a1]
			VList (_:_:_) -> unzipTuple a1
			otherwise -> "transpose.(:[])" ~> [VList [a1]]
		),
	-- Desc: moddiv
	-- Example : %~7 2 $ -> 1,3
	-- Test: %~7 ~ $ -> 1,3
	extendOp "%~" [takeRep] (shorterReason"this could be done by doing a range on the min of the two num values") ([num,BinCodeRep rangeRep,AutoDefault num 2], "(swap.).safeDivMod" ~> [VInt,VInt]),
	-- Desc: reverse
	-- Example: \,3 -> [3,2,1]
	op(reverseRep, [list], "reverse" ~> a1),
	-- Desc: divide
	-- Example: /7 2 -> 3
	-- Test: / *~2 7 -> -1
	-- Test: / *~2 *~7 -> 0
	-- Test: / 2 *~7 -> -1
	-- Test: / 7 ~ -> 3
	-- Test div 0: /7 0 -> 340282366920938463463374607431768211456
	op(('/',11), [num, AutoDefault num 2], "safeDiv" ~> VInt),	

	commutativeExtension [11,0]
		-- Desc: bit union
		-- Example: `| 3 6 -> 7
		-- Test chr: `| 1 'b' -> 'c'
		-- Test auto: `| ~ 2 -> 3
		("`|",[AutoDefault num 1,num],".|."~>orChr)
		-- Desc: bit xor
		-- Example: `^ 6 3 -> 5
		-- Test auto: `^ 6 ~ -> 7
		("`^",[num,AutoDefault num 1],"xor"~>xorChr),

	-- Desc: init
	-- Example: <<"asdf" -> "asd"
	opM(["<","<"], [11,0], [list], "init" ~> a1),
	-- Desc: step
	-- Example: `%2,5 -> [1,3,5]
	-- Test: `% *~2,5 -> [5,3,1]
	-- Test: `%~,5 -> [1,3,5]
	-- Test: `% 1 ,0 -> []
	-- Test lazy: <5 `%2,^10 100 -> [1,3,5,7,9]
	extendOp "`%" [takeRep, mapRep] equivalentOrderReason ([AutoDefault num 2, list], "step" ~> a2),
	-- Desc: subsequences
	-- 0 means all
	-- - allow repeat
	-- auto means 2
	-- Example: `_ 2 "abc" -> ["ab","ac","bc"]
	-- Test: `_ ~ "abc" -> ["ab","ac","bc"]
	-- Test: `_ 0 "abc" -> ["","a","b","ab","c","ac","bc","abc"]
	-- Test: `_ -2 "abc" -> ["aa","ab","ac","bb","bc","cc"]
	extendOp "`_" [dropRep, mapRep] equivalentOrderReason ([AutoDefault int 2, list],"\\n a->\
		\if n>0 then subsequencesN n a \
		\else if n==0 then subsequences a \
		\else repeatedSubsequencesN (-n) a"~>vList1.a2),
	-- Desc: take
	-- Example: <3"asdfg" -> "asd"
	-- Test negative: <-2 "asdfg" -> "asd"
	op(takeRep, [num, list], "\\n a->genericTake (if n<0 then genericLength a+n else n) a" ~> a2),
	-- Desc: append until null (todo consider prepend or something since this will be inefficient)
	-- Example: .~ ,4 >1^- 5,$$ -> [1,2,3,4,3,2,1]
	-- Test coerce: <7 .~ ,4 1 -> [1,2,3,4,1,1,1]
	-- Test coerce first: <4 .~ :2~ 1 -> [2,1,1,1]
	-- Test fibonnaci: <5 .~ :1~ +<2 $ -> [1,1,2,3,5]
	opM(".~", [], [list, autoTodo $ fn $ \[a1]->[fst $ promoteList [a1]]], \[a1,a2]-> "\\a f->appendUntilNull a ("++coerceTo [a1] (ret a2)++".f)" ~> a1),
	-- Desc: map
	-- Example: ."abc"+1$ -> "bcd"
	-- Test tuple: .,3~$*$$ -> [(1,1),(2,4),(3,9)]
	-- Test doesnt zip: ."ab".,2 :$ %@+$~ -> [[[1,1],[2,1]],[[1,0],[2,2]]]
	op(mapRep, [list, fn (elemT.a1)], mapFn ~> VList .ret.a2),
	--- Desc: zip2 (incomplete)
	--- Example: ."abc",3 -> [('a',1),('b',2),('c',3)]
	--- todo, coerce dim length can do the vectorizing for us??
-- 	op(".", [12], [list, Fn False UnusedArg $ \[a1]->(1,elemT a1)], (\[a1,a2]->"\\aa bb->zipWith (\\a b->"++flattenTuples (length$elemT a1) (length$elemT$head$ret a2) ++ "(a,b)) aa (bb())" ~> (VList $ elemT a1++(elemT$head$ret$a2)))),

	-- Desc: take drop while
	-- Example: `<~ ,5 - 3$ $ -> [1,2],[3,4,5]
	-- Test not: `<~ ,5 ~-$3 $ -> [1,2,3],[4,5]
	opM(["`<","~"], [], [list, AutoNot $ fn (elemT.a1)], "flip span" ~> \[a1,_]->[a1::VT,a1]),
	-- Desc: take while
	-- Example: <~ ,5 - 3$ -> [1,2]
	-- Test not: <~ ,5 ~-$3 -> [1,2,3]
	opM(["<","~"], [14], [list, BinCode 15, AutoNot $ fn (elemT.a1)], "flip takeWhile" ~> \[a1,_]->a1::VT),
	-- Desc: drop take while
	-- Example: `>~ ,5 - 3$ $ -> [3,4,5],[1,2]
	-- Test not: `>~ ,5 ~-$3 $ -> [4,5],[1,2,3]
	opM(["`>","~"], [12,0,0], [list, AutoNot $ fn (elemT.a1)], "(swap.).flip span" ~> \[a1,_]->[a1::VT,a1]),
	-- Desc: drop while
	-- Example: >~ ,5 - 3$ -> [3,4,5]
	-- Test not: >~ ,5 ~-$3 -> [4,5]
	opM([">","~"], [14], [list, BinCode 14, AutoNot $ fn (elemT.a1)], "flip dropWhile" ~> \[a1,_]->a1::VT),
	-- Desc: take drop
	-- Example: `< 2 ,5 $ -> [1,2],[3,4,5]
	-- Test negative: `< *~2 ,3 $ -> [1],[2,3]
	opM("`<", [14], [num, BinCode 11, list], "\\n a->genericSplitAt (if n<0 then genericLength a+n else n) a" ~> \[_,a2]->[a2::VT,a2]),
	-- Desc: drop take
	-- Example: `> 2 ,5 $ -> [3,4,5],[1,2]
	-- Test negative: `> *~2 ,3 $ -> [2,3],[1]
	opM("`>", [12,0], [num, list], "\\n a->swap$genericSplitAt (if n<0 then genericLength a+n else n) a" ~> \[_,a2]->[a2::VT,a2]),
	-- Desc: uncons
	-- Example: `( ,3 $ -> 1,[2,3]
	-- Test empty: `( ,0 $ -> 0,[]
	-- Test tuple: `( z ,3 "abc" $ @ -> 1,'a',[(2,'b'),(3,'c')]
	-- Test tuple empty: `( z ,0"a" $ @ -> 0,' ',[]
	opM(["`","("], [14], [list, BinCode 1], \[a1]->flattenTuples (length$elemT a1) 1++".fromMaybe("++defaultValue(elemT a1)++",[]).uncons" ~> elemT a1++[a1]),
	-- Desc: swapped uncons
	-- Example: `) ,3 $ -> [2,3],1
	-- Test empty: `) ,0 $ -> [],0
	-- Test tuple: `) z ,3 "abc" $ @ -> [(2,'b'),(3,'c')],1,'a'
	-- Test tuple empty: `) z ,0"a" $ @ -> [],0,' '
	opM(["`",")"], [14], [list, BinCode 2], \[a1]->flattenTuples 1 (length$elemT a1)++".swap.fromMaybe("++defaultValue(elemT a1)++",[]).uncons" ~> a1:elemT a1),
	-- Desc: tail
	-- Example: >>,5 -> [2,3,4,5]
	opM([">",">"], [12,0], [list], "tail" ~> a1),
	-- Desc: drop
	-- Example: >3,5 -> [4,5]
	-- Test more than size: >5,3 -> []
	-- Test negative: >-2 ,5 -> [4,5]
	op(dropRep, [int, list], "\\n a->genericDrop (if n<0 then genericLength a+n else n) a" ~> a2),
	-- Desc: modulus
	-- Example: %7 2 -> 1
	-- Test: % *~2 7 -> 5
	-- Test: % *~2 *~7 -> -2
	-- Test: % 2 *~7 -> -5
	-- Test: % 7 ~ -> 1
	-- Test mod 0: %7 0 -> 0
	op(('%',12), [num, AutoDefault num 2], "safeMod" ~> VInt),
	-- Desc: chr
	-- Example: ch 100 -> 'd'
	extendOp "ch" [lengthRep, rangeRep] (shorterReason"use max ~ (0) if you wanted that") ([AutoDefault int 126], "id" ~> xorChr.(VChr:)),
	-- Desc: tbd
	-- Example: 0 -> 0
	extendOp "tbd" [lengthRep, rangeRep] (shorterReason"use max ~ (0) if you wanted that") ([autoTodo char], undefinedImpl),

	-- Desc: chunksOf
	-- Example: `/2,5 -> [[1,2],[3,4],[5]]
	-- Test doesnt get swallowed by ?, : ?`/ 1"...a.."~ \/$$a -> 4
	-- Test lazy: <3 `/2,^10 100 -> [[1,2],[3,4],[5,6]]
	-- Test: `/~,5 -> [[1,2],[3,4],[5]]
	-- Test negative: `/ -2 ,5 -> [[1],[2,3],[4,5]]
	-- todo warn extend with bincode
	extendOp "`/" [reverseRep,repRep] equivalentOrderReason ([AutoDefault num 2, list], "\\n a->if n<0 \
	\then reverse $ map reverse $ chunksOf (fromIntegral (-n)) (reverse a)\
	\else chunksOf (fromIntegral n) a" ~> vList1 .a2),
	-- Desc: nChunks
	-- Example: `\ 2 ,6 -> [[1,2,3],[4,5,6]]
	-- Test: `\ 2 ,5 -> [[1,2,3],[4,5]]
	-- Test: `\ ~ ,5 -> [[1,2,3],[4,5]]
	-- Test: `\ 4 ,10 -> [[1,2,3],[4,5],[6,7,8],[9,10]]
	-- Test: `\ -4 ,10 -> [[1,2],[3,4,5],[6,7],[8,9,10]]
	-- Test empty: `\ -4 "" -> []
	extendOp "`\\" [lengthRep, repRep] "Multiply length by fst arg instead of replicating list." ([AutoDefault int 2, list], "\\a b->map (map fst) $ groupBy (onToBy $ \\e->a*(snd e + if a<0 then 1 else 0)`div`genericLength b) $ zip b [0..]" ~> vList1 . a2),
	-- Desc: length
	-- Example: ,>1"asdf" -> 3
	op(lengthRep, [list], "genericLength" ~> VInt),
	-- Desc: range from 1 to (todo what negatives do?)
	-- Example: ,3 -> [1,2,3]
	-- Test: ,*~3 -> []
	-- Test auto: <3 :0,~ -> [0,1,2]
	op(rangeRep, [AutoDefault num (2^128)], "\\x->[1..x]" ~> vList1 .a1),
	-- Desc: exponentiation
	-- Example: ^2 8 -> 256
	-- Test: ^~ 1 -> 10
	-- Test: ^8 ~ -> 64
	-- Test: ^0 0 -> 1
	-- Test negative is nth root: ^200 -2 -> 14
	-- Test: ^81 -4  ^80 -4 -> 3,2
	op(('^',14), [AutoDefault int 10, AutoDefault int 2], "\\a b->if b<0 then nthRoot (-b) a else a^b" ~> a1),
	-- Desc: replicate
	-- Example: ^3"ab" -> "ababab"
	-- Test: ^-3"ab" -> ""
	-- Test: ^3'a' -> "aaa"
	-- Test: <3^~"ab" -> "aba"
	op(repRep, [AutoDefault int (2^128), orC list char], \[_,a2] ->
		let (ap2, apf) = promoteList [a2] in
		"\\n a->concat$genericReplicate n$"++apf++" a" ~> ap2),
	-- Desc: ord
	-- Example: o'd' -> 100
	op(('o',14), [char], "id"~>VInt),
	-- Desc: splitBy
	-- returns a list of pair of (adjacent matching sequence, non matching sequence before it)
	-- assumes that the input ends with a match, if it does not, then it appends an empty match
	-- so that you may have access to the final non matching sequence.
	-- Example: %~ "abc\nde  f" \$a -> [("abc",""),("de","\n"),("f","  ")]
	-- Test leading non match: %~ " a" \$a -> [("a"," ")]
	-- Test trailing non match: %~ "a " \$a -> [("a",""),(""," ")]
	-- Test outer non match: %~ " a " \$a -> [("a"," "),(""," ")]
	-- Test not: %~ " a" ~\$a -> [(" ",""),("","a")]
	opM("%~", [14], [list, BinCode 0, AutoNot $ fn (elemT.a1)], \[a1,_]->
		"\\a f->let r = chunksOf 2 $ (split.dropFinalBlank.condense.whenElt) f a \
		\in map (\\c->let (a,b)=splitAt 1 c in (concat b,concat a)) r" ~> VList [a1,a1]),
	-- Desc: find salt by
	-- Example: findSaltBy "Fizz""Buzz" 100 ~ -+$195 -> 1258
	opM ("findSaltBy", [], [list, int, AutoDefault int (2^128), AutoNot $ fn (\_->[VList [VInt]])], \[a1,_,_,_]->"\\inputs modn stopat goalChecker->\
		\fromMaybe (-1) $ find (\\salt->goalChecker (map (\\input->(fromIntegral$hlist$("++flatten(head $ elemT a1)++")input++toBase 256 salt) `mod` modn) inputs)) [0..stopat] "~>VInt),
	-- Desc: find salt
	-- there is an option to provide lists of acceptables for each input
	-- Example: findSalt "Fizz""Buzz" 6 ~ :3 5 -> 68
	-- Test: findSalt "Fizz""Buzz" 60 ~ :5 :,3~ -> 51
	opM ("findSalt", [], [list, int, AutoDefault int (2^128), list], \[a1,_,_,a4]->"\\inputs modn stopat goal->\
		\fromMaybe (-1) $ find (\\salt->and $ zipWith ("++(if cidim [a4]>1 then "elem" else "(==)")++") (map (\\input->(fromIntegral$hlist$("++flatten(head $ elemT a1)++")input++toBase 256 salt) `mod` modn) inputs) goal) [0..stopat] "~>VInt),
	
	-- Desc: hashmod (md5) (todo use a 1 char thing since bin int doesn't need a nibble)
	-- untested example: ."Fizz""Buzz" `# $ 6   68 -> [3,5]
	-- RawTest: p."Fizz""Buzz" `# $ 6   68 -> "[3,5]\n"
	-- RawTest: p:`# ~"5a" 100 `# "5" 100 97 -> "[62,62]\n"
	-- Test: hex `# "asdf" 0 -> "912ec803b2ce49e4a541068d495ab570"
	opM("`#", [15,0], [AutoOption "nosalt", any1, ParseArg "int" intParser], (\[o,a1,a2]->do
		let salt = o/=OptionYes
		modify $ \s -> s { pdDataUsed = pdDataUsed s || salt }
		return $ "\\a b->(fromIntegral$hlist$("++flatten a1++")a++toBase 256 "++(if salt then "dat" else "0") ++")`mod`(if b==0 then 2^128 else b)" ~> a2)::[VT]->ParseState (VT,String)),
	-- Desc: to base from data
	-- untested example: `D 2   10 -> [1,0,1,0]
	-- RawTest: p `D 2 10 -> "[1,0,1,0]\n"
	-- RawTest: `D -5 1934 -> "c bad\n"
	-- RawTest: `D -150 19178 -> "\DEL\128\n"
	extendOp "`D" [addRep,tildaRep,tildaRep] (shorterReason"+~~ = 3") ([ParseArg "int" staticIntParser], ((\[StaticInt v1]->do
		modify $ \s -> s { pdDataUsed = True }
		return $ (if v1<0 then "map ((\\c->if c<128 then (printables++unprintables)!!fromIntegral c else c).fromIntegral)." else "") ++ "(\\base->toBase (abs base) dat)"~>vList1 (if v1<0 then VChr else VInt))::[VT]->ParseState (VT, String))),
	-- Desc: to base
	-- Example: `@ 2 10 -> [1,0,1,0]
	-- Test: `@ 2 0 -> []
	-- Test: `@ ~ 89 -> [8,9]
	extendOp "`@" [sumRep, consRep] ("just use a+b") ([AutoDefault int 10, autoTodo num], "toBase"~>vList1 .a1),
	-- Desc: from base
	-- Example: `@ 2 :1 :0 :1 0 -> 10
	-- Test: `@ 2 ,0 -> 0
	-- Test: `@ ~ :8 9 -> 89
	-- Test: `@ -27 "fizzbuzz" -> 66635848070
	opM("`@",[11,0], [AutoDefault num 10,list {-todo 1d-}], "\\base a->fromBase (abs base) (if base < 0 then map (\\c->fromIntegral $ fromMaybe (fromIntegral c) $ elemIndex c printables) a else a)"~>a1),
	-- Desc: int to str
	-- Example: `p 5 -> "5"
	extendOp "`p" [ifRep, intRep] (shorterReason"don't use an if, just provide either the true or false clause depending on the constant)") ([num], inspect.a1 ~> vstr),
	-- Desc: split list
	-- Example: /~ ,5 :3~ -> [[1,2],[4,5]]
	-- Test end splits: /~ "abca" "a" -> ["","bc",""]
	-- Test promote: /~ ,5 3 -> [[1,2],[4,5]]
	-- Test coerce: /~ "abc" 'b' -> ["a","c"]
	extendOp "/~" [ifRep, intRep] (shorterReason"don't use an if, just provide either the true or false clause depending on the constant)") ([list,any1], \[a1,a2]->
		"\\a needle->splitOn ("++coerceTo [a1] [a2]++" needle) a" ~> vList1 a1),
	-- Desc: if nonnull (lazy) todo alias to regular if/else
	-- Example: ?,>0:5 5 1 0 -> 1
	-- Test: ?,>0:"" "" 1 0 -> 0
	-- Test: ?,>0:5 5 $ 0 -> [5,5]
	lowPriorityOp([fst ifRep,fst lengthRep], [snd ifRep,snd lengthRep], list:ifBodyArgs, ifImpl),
	-- Desc: if/else todo delet fn arg
	-- Example: ? +0 0 "T" "F" -> "F"
	-- Test coerce: ? +1 0 1 "F" -> "1"
	-- Test mult rets: ? +0 0 ~1 2 3 4 $ -> 3,4
	-- Test using cond value: ? +1 0 $ 5 -> 1
	-- Test cond value doesn't clobber implicit: ;5 ? +1 0 -> 5,5
	-- Test false clause default: ? +0 0 "hi" ~ -> ""
	-- todo auto value "nothing" which coerces usefully
	-- todo match number of rets in false clause (similarly to what should be done for :)
	-- todo make clauses 1 fn, that way they could share let statements
	op(ifRep, num:ifBodyArgs, ifImpl),
	-- Desc: read str at base (todo need ~ = 10, index uses it)
	-- todo also consider returning the stuff after the parsed number...
	-- note negative base does nothing useful
	-- Example: r"1f" 16 -> 31
	-- Test negative: r"-1f" 16 -> -31
	-- test uppercase: r"1F" 16 -> 31
	-- Test empty: r"" 16 -> 0
	-- Test invalids: r"z1fz2" 16 -> 31
	op(('r',15), [str, int], "parseNum" ~> VInt),
	-- Desc: index by
	-- Example: ?,100~ -*$$80 -> 9
	-- Test negation: ?,5~ ~0 -> 1
	op(('?',15), [list, auto, AutoNot $ fn (elemT.a1)], "\\l f->fromIntegral$1+(fromMaybe (-1) $ findIndex f l)" ~> VInt),

	-- Desc: index. Or 0 if not found.
	-- Example: ?  :3:4 5  4 -> 2
	-- Test not found: ? ,3 4 -> 0
	-- Test tuple: ? z ,3 "abc" 2 -> 2
	op(('?',15), [listToBeReferenced, elemOfA1], \[a1,a2]->"\\a e->fromIntegral$1+(fromMaybe (-1) $ elemIndex e (map "++firstOf (elemT a1)++" a))" ~> VInt),
	-- Desc: diff
	-- Example: -"abcd" "bd" -> "ac"
	-- Test non existant elements: -"abc" "de" -> "abc"
	-- Test doesn't drop all: -"aa" "a" -> "a"
	op(diffRep, [listToBeReferenced, sameAsA1], "\\\\" ~> a1),
	
	-- todo there are some type combinations that are unused for bin 15
	-- diff could work with non matching tuples too, aka diff by?
	
	-- Desc: groupAllBy
	-- Example: =~ "cabcb" $ -> ["a","bb","cc"]
	-- Test: =~ "cabcb" ~$ -> ["cc","a","bb"]
	extendOp "=~" [reverseRep, mapRep] equivalentOrderReason ([list, AutoOption "nosort", Fn ReqArg UnusedArg $ \[a1,_]->(1,elemT a1)], \[a1,o,_]->"\\a f->\
		\map (map (\\(e,_,_)->e)) $"++
		(if o==OptionYes then "sortOn (\\((_,_,ind):_)->ind) $" else "")++
		"groupBy (onToBy (\\(_,fa,_)->fa)) $\
		\sortOn (\\(_,fa,_)->fa) $\
		\zip3 a (map f a) [0..]" ~> vList1 a1),
	-- Desc: or
	-- Example: or"" "b" or "a" "c" -> "b","a"
	-- Test coerce: or "" 5 -> "5"
	extendOp "or" [reverseRep, mapRep] equivalentOrderReason ([list, AutoOption "todo", Fn ReqConst UnusedArg $ \[a1,_]->(1,elemT a1)], \[a1,o,a2]->"\\a b->if "++truthy [a1]++" a then a else "++coerceTo [a1] (ret a2)++"b" ~> a1),
	
	-- Desc: to hex
	-- Example: hex 31 -> "1f"
	-- Test negative: hex *~31 -> "-1f"
	-- [14,13], [int, BinCode 4]
	extendOpHelper ["`<",","]  (shorterReason"range is already sorted") ("hex", [14,snd rangeRep],  [num,BinCode 13], "\\i -> sToA $ (if i < 0 then \"-\" else []) ++ showHex (abs i) []" ~> vstr),
	-- Desc: uniq
	-- Example: `$ "bcba" -> "bca"
	opM("`$",[14],[list, BinCode 4],"nub"~>a1),
	-- Desc: list intersection
	-- Example: `& "abaccd" "aabce" -> "abac"
	-- Test ~ (true set): `& "aa" ~ "aa" -> "a"
	-- Test coerce: `& ,3 1 -> [1]
	--- Test tuple todo
	-- Test by: `& ,5 %$2 :1 1 -> [3,5]
	-- Test true set by: `& ,5 ~ %$2 :1 1 -> [1]
	binarySetOp "`&" 5 "a - (a - b)",
	-- Desc: list union
	-- Example: `| "abccd" "aabce" -> "abccdae"
	binarySetOp "`|" 6 "a ++ (b - a)",
	-- Desc: list xor
	-- Example: `^ "aabce" "abbde" -> "acbd"
	binarySetOp "`^" 7 "(a-b) ++ (b-a)",
	
	-- Desc: list of 2 lists
	-- Example: `: ,2 ,1 -> [[1,2],[1]]
	extendOpHelper ["`-"] (shorterReason"just use -") ("`:", [14], [listToBeReferenced, BinCode 8, sameAsA1], "\\a b->[a,b]" ~> vList1.a1),
	-- Desc: list difference
	-- Example: `- "aabd" 'a' -> "abd"
	binarySetOp "`-" 8 "a-b",
		
	-- Desc: strip
	-- Example: -~ " bcd\n\n" -> "bcd"
	opM("-~",[],[str {-could be more general, but probably not useful -}],\[a1]->let cond="(not."++truthy (elemT a1)++")" in
		"dropWhileEnd "++cond++" . dropWhile "++cond~>a1),
		
	-- Desc: nary cartesian product
	-- Example: `* `/ 2 ,4 -> [[1,3],[1,4],[2,3],[2,4]]
	opM("`*",[],[listOf list],"sequence"~>a1),
	-- Desc: permutations todo rename to `\ or `/ depending what scan uses
	-- Example: `P "abc" -> ["abc","bac","cba","bca","cab","acb"]
	opM("`P",[14],[list, BinCode 9],"permutations"~>vList1.a1),
	
	-- Desc: range from 0 ... (maybe don't need this?
	-- Example: `, 3 -> [0,1,2]
	-- Test: <3 `, ~ -> [0,1,2]
	extendOp "`," [reverseRep, consRep] equivalentOrderReason ([AutoDefault num (2^128)], "\\x->[0..x-1]" ~> vList1 . a1),
	
	-- Desc: find indices by
	-- Example: `? "a b" \$a -> [1,3]
	opM("`?", [14], [list, BinCode 10, AutoNot $ Fn ReqArg UnusedArg $ \[a1]->(1,elemT a1)], "\\l f->map ((+1).fromIntegral) $ findIndices f l" ~> vList1 VInt),
	
	-- Desc: find indices
	-- Example: `? "a b" ' ' -> [2]
	-- Test coerce: `? "a b" " " -> [2]
	-- Test not: `? "a b" ~' ' -> [1,3]
	opM("`?", [14], [list, BinCode 10, AutoOption "not", Fn ReqConst UnusedArg $ \[a1,o]->(1,elemT a1)], \[a1,o,a2]->("\\a needleFn->let needle = ("++coerceTo (elemT a1) (ret a2)++" needleFn) in \
			\map (\\i->fromIntegral i+1) $ " ++
				if o==OptionNo then "elemIndices needle a"
				else "findIndices (\\e->e /= needle) a"
		) ~> vList1 VInt),
	
	-- Desc: special folds (todo vec) (op shouldn't be last if possible, blocks implicit args) (todo use a ;; opcode)
	-- Example: `/ "asdf" ] -> 's'
	-- Test: `/ "" * -> 1
	-- Test: `/,3] `/,3[ `/,3+ `/,3* `/,3- `/,3% `/,3^ -> 3,1,6,6,2,1,1
	-- Test commutative order: `/ :9 :6 2 / -> 3
	-- Test by: `/ "asdf" >$ -> 's'
	-- Test by empty: `/ ,0 <$ -> 340282366920938463463374607431768211456
	opM("`/", [14], [list, BinCode 11, FoldMode], \[a1,rt]->"\\a f->f (foldr1,id) a" ~> ret rt),
	
	-- Desc: special scans
	-- Example: `\ ,4 + -> [1,3,6,10]
	-- Test: `\ ,0 + -> []
	-- Test commutative order: `\ :2 :6 9 / -> [2,3,3]
	opM("`\\", [14], [list, BinCode 12, FoldMode], \[a1,rt]->"\\a f->f (scanl1.flip,const[]) a" ~> VList (ret rt)),
	
	-- Desc: debug arg type
	-- Example: pt 5 -> error "VInt"
	opM("pt", [], [any1], "" ~> errorWithoutStackTrace.show.a1 :: ([VT]->[VT],String)),
	-- Desc: show
	-- Example: p"a" -> "\"a\""
	opM("p", [], [any1], inspect.a1 ~> vstr),
	-- Desc: debug context types
	-- Example: ;5 ct -> error "$ LetArg VInt ..."
	opM("ct", [], [], gets pdContext >>= parseError . debugContext :: ParseState Impl),
	-- Desc: error
	-- Example: error p +2 1 -> error "3"
	opM("error", [], [str], "errorWithoutStackTrace.aToS" ~> vstr),


	opM("testCoerce2", [], [any1, any1], testCoerce2 ~> vstr),
	opM("testCoerceToInt", [], [any1], testCoerceTo [VInt]),
	opM("testCoerceToChr", [], [any1], testCoerceTo [VChr]),
	opM("testCoerceToListInt", [], [any1], testCoerceTo [VList [VInt]]),
	opM("testCoerceToStr", [], [any1], testCoerceTo [vstr]),
	opM("testCoerceToListListInt", [], [any1], testCoerceTo [VList [VList [VInt]]]),
	opM("testCoerceToListStr", [], [any1], testCoerceTo [VList [vstr]]),
	opM("testCoerceToX", [], [AnyS, AnyS], \ts -> "\\a b->" ++ snd (testCoerceTo (ret $ a1 ts) (ret $ a2 ts)) ++ "(b())" ~> (ret $ a1 ts)),
	opM("testFinish", [], [any1], flip finish False . a1 ~> vstr),
	
	--- Desc: zip
	-- Example: z,3"abc" -> [(1,'a'),(2,'b'),(3,'c')]
	-- Test: .z,3,3+@$ -> [2,4,6]
	-- Test 3 tuple: .z z,3,3,3++_@$ -> [3,6,9]
	-- Test 3 tuple: z,3 z,3"abc" -> [(1,1,'a'),(2,2,'b'),(3,3,'c')]
	opM("z", [], [list, list], (\[a1,a2]->"zipWith (\\a b->"++flattenTuples (length$elemT a1) (length$elemT a2) ++ "(a,b))") ~> (VList .(concatMap elemT) :: [VT] -> VT))]

foldr1Fn :: [VT] -> String
foldr1Fn = (\[a1,a2]->"\\a f->if null a then "++defaultValue (elemT a1)++" else foldr1 (\\x y->"++coerceTo (elemT a1) (ret a2)++"$f$"++flattenTuples(length $ elemT a1)(length $ elemT a1)++"(x,y)) a")
foldrFn :: String -> [VT] -> String
foldrFn foldOp = (\[a1,a2,a3]->"\\a i f->"++foldOp++" (\\x y->"++coerceTo (ret a2) (ret a3)++"$f$"++flattenTuples(length $ elemT a1)(length $ ret a2)++"(x,y)) (i()) a")

mapFn :: [VT] -> String
mapFn = (\[a1,a2]->"(\\a f->map f a)")

ifBodyArgs = 
	[ Fn ReqDontCare OptionalArg $ \a1 -> (1,a1)
	, OrAuto "default" $ fnx (\[a1,a2]->(length$ret a2,[]))]
ifImpl [a1,a2,a3] = 
	if a3==OptionYes -- default
	then "\\c a->if "++truthy [a1]++" c then a c else "++defaultValue (ret a2) ~> ret a2
	else let (coercedType, coerceFn) = coerceEither (ret a2) (ret a3)
		in "\\c a b->"++coerceFn ++ "$ (iff."++truthy [a1]++") c (a c) (b())" ~> coercedType

zipImpl :: [VT] -> (VT, String)
zipImpl [a1,a2] = "zipWith (\\a b->"++flattenTuples (length$elemT a1) (length$elemT a2) ++ "(a,b))" ~> VList (elemT a1++elemT a2)

-- todo handle mismatch tuple better (by)
-- allows ~ to make it a true set up (duplicates removed)
-- allows option 2nd arg fn that makes it a "on" op
-- coerces/etc.
-- code must be defined using -
binarySetOp name binrep code =
	opM(name, [14], [list, BinCode binrep, AutoOption "uniq",OptionalFn (\[a1,_]->(1,elemT a1)),any1], \[a1,o1,optionalFn,a2]-> let
		(coercedType, coerceFnA, coerceFnB) = coerce [a1] bt
		replaceMinus = concatMap (\c->if c=='-' then "`minus`" else [c])
		maybeNub = if o1==OptionYes then "(nubBy (onToBy snd))" else "id"
		(byFn,bcode,bt) = case optionalFn of
			ItWasAConstant -> ("id","(bb())",ret a2) -- todo handle tuple elem, multi ()
			otherwise -> ("ff","bb",[a2])
		zipBy = "(\\x -> zip x $ map " ++ byFn ++ " x)"
	 in
		"\\aa ff bb->let minus=deleteFirstsBy (onToBy snd);\
		\a="++maybeNub++"$"++zipBy++"$"++coerceFnA++"aa;\
		\b="++maybeNub++"$"++zipBy++"$"++coerceFnB++bcode++" in \
		\map fst $ "++maybeNub++"$"++replaceMinus code~>coercedType)

addHigherValueDeBruijnOps ops = concat [opM(
		replicate unary (fst setRep) ++ snd symb,
		replicate unary (snd setRep) ++ [2+fst symb],
		[],
		argn (unary*3+fst symb))
	| unary <- [1..10]
	, symb <- [(1,"$"),(2,"@"),(3,"_")]
	] ++ map convertNullNib ops

-- todo replace code in ops table
binOp :: Char -> [VT] -> ([VT], String)
-- binOp ':' [a1,a2] = let (ct,f1,f2)=coerce [a1] [a2] in
-- 	(head ct, "\\a b->["++f1++" a,"++f2++" b]", (0,0))
binOp '+' a = ([xorChr a], "+")
binOp '*' a = ([VInt], "*")
binOp '-' a = ([xorChr a], "-")
binOp '/' a = ([VInt], "safeDiv")
binOp '%' a = ([VInt], "safeMod")
binOp '^' a = ([xorChr a], "^")
binOp ']' a = ([orChr a], "max")
binOp '[' a = ([orChr a], "min")
binOp '!' a = ([VInt], "(abs.).(-)")

binOp ':' [a,b] = let
		(ap,apFn) = promoteList (ret a)
		(coercedType, coerceFnA, coerceFnB) = coerce [ap] (ret b)
	in
		"\\a b->("++coerceFnA++"$"++apFn++"(a()))++"++coerceFnB++"(b())"~>coercedType

-- don't need these since they will auto vec already
-- binOp '|' a = (orChr a, ".|.", (0,0))
-- binOp '&' a = (xorChr a, ".&.", (0,0))
-- binOp 'x' a = (xorChr a, "xor", (0,0))

-- todo handle tuples...
binOp '=' [a1,a2] = (elemT a1, "\\a i->if null a then "++defaultValue (elemT a1)++"else lazyAtMod a (fromIntegral i - 1)")
binOp '?' [a1,a2] = ([VInt], "\\a e->fromIntegral$1+(fromMaybe (-1) $ elemIndex e a)")

allOps = sortOn opSpecificity $ addHigherValueDeBruijnOps $ concat rawOps
simpleOps = sortOn opSpecificity $ addHigherValueDeBruijnOps $ filter isOpSimple $ map (\l ->
	-- Nasty hack, select the 4th op from a commutative op (the normal case from 2 ops and 4 warnings)
	if length l == 6 then l!!3 else last l)
		rawOps

typeToStr (Cond desc _) = Just desc
typeToStr Auto = Just "~"
typeToStr (BinCode b) = Nothing
typeToStr (AutoOption _) = Nothing
typeToStr (AutoNot s) = typeToStr s
typeToStr (OrAuto _ a) = typeToStr a
typeToStr (Fn ReqDontCare _ _) = Just "fn"
typeToStr (Fn ReqArg _ _) = Just "reqfn"
typeToStr (Fn ReqConst _ _) = Just "const"
typeToStr (AutoDefault t _) = typeToStr t
typeToStr (AutoData t) = typeToStr t
typeToStr (ParseArg desc _) = Just $ "{"++desc++"}"
typeToStr (OptionalFn _) = Just $ "fn?"
typeToStr (ZipMode) = Just "zipop"
typeToStr (FoldMode) = Just "foldop"
typeToStr (CharClassMode) = Just "chClass"
typeToStr AnyS = Just "any*"
typeToStr _ = Just $ "unknown"
