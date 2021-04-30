{-# LANGUAGE FlexibleInstances #-} -- for String instances

module Ops (Operation(..), getOps) where

import Types
import Polylib
import Expr
import Parse
import InputCode
import Args

data Operation ic = Op [VT] ([VT]->(VT, String)) [Int] | Atom ([VT] -> Expr -> ic -> (ic, Expr)) deriving Show

op(lit, nib, t, impl, autos) = (lit, nib, Op t (toImpl impl) autos)
atom(lit, nib, impl) = (lit, nib, Atom impl)

todo = error "todo"
autoTodo = 0
impossibleAuto = 0

getOps :: InputCode ic => Thunk ic -> [(String, [Int], Operation ic)]
getOps _ = [
	-- Desc: auto int
	-- Example: +4~ -> 5
	op("~", [0], [], ("undefined"::String)~>VAuto, []),
	-- Desc: integer
	-- Example: 3 -> 3
	atom("0", [1], const $ const parseIntExpr), 
	-- Desc: string
	-- Example: "hi\n" -> "hi\n"
	-- (size 6)
	atom("\"", [2], const $ const parseStrExpr),
	-- Desc: 1st arg
	-- Example: ;1;2;3 $ -> 3
	atom("$", [3], getArg 0),
	-- Desc: 2nd arg
	-- Example: ;1;2;3 @ -> 2
	atom("@", [4], getArg 1),
	-- Desc: nth arg
	-- Example: ;1;2;3 \2 -> 1
	atom("\\", [5], getArgN),
	-- Desc: let
	-- Example: ;3 +$$ -> 6
	op(";", [6], [anyT, Fn a1], "flip id" ~> a2, []),
	-- Desc: append
	-- Example: :"abc""def" -> "abcdef"
	-- Test: :"abc"1 -> "abc1"
	-- Test: :1"abc" -> "1abc"
	-- Test: :1 2 -> [1,2]
	op(":", [7], [PromoteList (Coerce anyT), PromoteList (Coerce anyT)], "++" ~> a1, []),
	-- Desc: add
	-- Example: +1 2 -> 3
	-- Test: +2 ^"a"1 -> 'c'
	-- Test: +^"a"1 2 -> 'c'
	-- Test: +1,3 -> [2,3,4]
	-- Test: +1 %,2 ,2 -> [[2,3],[2,3]]
	-- Test: +1"abc" -> "bcd"
	-- Test: +^"a"1 :1 2 -> "bc"
	op("+", [8], [Coerce num, Vec $ Coerce num], "+" ~> a2, [1,1]),
-- 		--todo delete one this v ^ but need to coerce in vector
-- 		op("+", [8], [N, V N], (a2, "+"), [todo]),
	-- Desc: split
	-- Example: +"a b c"" " -> ["a","b","c"]
	-- Test: +" a  b "" " -> ["a","b"]
	op("+", [8], [str, str], "flip$(filter (/=[]).).splitOn" ~> listOf.a1, []),
	-- Desc: join
	-- Example: +" ",3 -> "1 2 3"
	-- Test: +" "%,2,3 -> ["1 2 3","1 2 3"]
	op("+", [8], [str, list], join.elemT.a2, []),
	-- Desc: sum
	-- Example: +,3 -> 6
	-- Test: +,0 -> 0
	op("+", [8], [listOf int], "sum" ~> int, []),
	-- Desc: concat
	-- Example: +%,3,$ -> [1,1,2,1,2,3]
	op("+", [8], [listOf list], "concat" ~> elemT.a1, []),
	-- Desc: subtract
	-- Example: - 5 3 -> 2
	op("-", [9], [Coerce num, Coerce num], "-" ~> a1, [1, 1]),
	-- Desc: step
	-- todo test/make negative
	-- Example: -2,5 -> [1,3,5]
	op("-", [9], [int, list], "step" ~> a2, [2]),
-- 		op("-", [9], [Chr, l], (todo::VT, todo::String), []), -- not sure yet, chr list
	-- Desc: filter
	-- Example: -,5%$2 -> [1,3,5]
	op("-", [9], [list, Fn (elemT.a1)], (\args -> "flip$filter.("++truthy (a2 args)++".)") ~> a1, []),
	-- Desc: multiply
	-- Example: *7 6 -> 42
	op("*", [10], [Coerce num, Vec $ Coerce num], "*" ~> a1, [-1, 2]),
-- 			-- 	-- todo * for either one being a char...
-- 		op("*", [10], [N, Str], (a1, todo::String), []), -- not sure yet, str int/chr (atoi+, ~ = 0, so it does atoi
-- 		op("*", [10], [N, V l], (a2, "*"), [-1, 2]),
-- 			-- 	-- todo ^ * chr list
	-- Desc: foldr1
	-- Example: *,3+$@ -> 6
	-- todo make/test empty
	op("*", [10], [list, Fn (\[VList e]->VPair e e)], "flip$foldr1.curry" ~> a2, []),
	-- Desc: reverse
	-- Example: /,3 -> [3,2,1]
	op("/", [11], [list], "reverse" ~> a1, []),
	-- Desc: divide
	-- Example: /7 2 -> 3
	op("/", [11], [Coerce num, Coerce num], "div" ~> a1, [autoTodo, 2]),
-- 				-- 	-- todo ^ chr div?
	-- Desc: take
	-- Example: /3,5 -> [1,2,3]
	-- todo test/make negative
	op("/", [11], [num, list], "take.fromIntegral" ~> a2, [1]),
-- 			--  	-- todo ^ chr take
	-- Desc: map
	-- Example: %"abc"+1$ -> "bcd"
	op("%", [12], [list, Fn (elemT.a1)], "flip map" ~> listOf.a2, []),
	-- Desc: drop
	-- Example: %3,5 -> [4,5]
	-- Test: %5,3 -> []
	-- todo test/make negative
	op("%", [12], [num, list], "drop.fromIntegral" ~> a2, [1]),
-- 			--  	-- todo ^ chr drop
	-- Desc: modulus
	-- Example:  %7 2 -> 1
	-- todo test negatives
	op("%", [12], [Coerce num, Coerce num], "mod" ~> a1, [autoTodo, 2]),
-- 			-- 	-- todo ^ chr mod - actually that makes sense
	-- Desc: length
	-- Example: ,"asdf" -> 4
	op(",", [13], [list], "length" ~> int, []),
	-- Desc: range from 1 to
	-- todo test negative
	-- Example: ,3 -> [1,2,3]
	op(",", [13], [num], "\\x->[1..x]" ~> listOf.a1, []),
	-- Desc: exponentiation
	-- todo test/make negative
	-- Example: ^2 8 -> 256
	op("^", [14], [num, num], "^" ~> a1, [10,2]),
-- 			-- 	-- todo ^ chr pow...
	-- Desc: replicate
	-- todo test/make negative
	-- Example: ^3 "ab" -> "ababab"
	op("^", [14], [num, list], "(concat.).(replicate.fromIntegral)" ~> a2, [1]),
	-- Desc: value at index
	-- Example: ^"asdf" 2 -> 's'
	-- Test: ^"asdf" 0 -> 'f'
	op("^", [14], [list, num], "\\a i->a!!(fromIntegral (i-1)`mod`length a)" ~> elemT.a1, [impossibleAuto, 1]),
-- 		-- todo ^ chr !!

	-- Desc: zip
	-- Example: %^,3"abc"+$@ -> "bdf"
	op("^", [14], [list, list], "zip" ~>  listOf.pairOf.(both elemT), []),
	-- Desc: if/else
	-- Example: ? 0 "T" "F" -> "F"
	-- Test: ? 1 1 "F" -> "1"
	op("?", [15], [num, Coerce anyT, Coerce anyT], ("iff."++).(truthy.a1) ~> a2, []),
	-- Desc: index
	-- Example: ?  :3:4 5  4 -> 2
	-- Test: ? ,3 4 -> 0
	op("?", [15], [list, elemOfA1], (int, "\\a e->1+(fromMaybe (-1) $ elemIndex e a)"), []), 
-- 			--todo if not eq then subarray index?
	-- Desc: diff
	-- Example: ?"abcd""bd" -> "ac"
	-- Test: ?"abc""de" -> "abc"
	-- Test: ?"aa""a" -> "a"
	op("?", [15], [list, sameAsA1], (a1, "\\\\"), []), -- todo e==e2
	-- Desc: show
	-- untested example: p"a" -> "\"a\""
	op("p", onlyLit, [anyT], inspect.a1 ~> str, [])]

infixr 8 ~>
a~>b = (b,a)

onlyLit = [16, undefined]
a1 = head :: [VT] -> VT
a2 = (!!1) :: [VT] -> VT
a3 = (!!2) :: [VT] -> VT

both f [a,b] = (f a, f b)
pairOf = uncurry VPair

elemOfA1 = Cond "a" (\[a1] a2->VList a2==a1)
sameAsA1 = Cond "[a]" (\[a1]->(a1==))

class Impl impl where
	toImpl :: impl -> [VT] -> (VT, String)
instance Impl ([VT] -> VT, [VT] -> String) where
	toImpl (f1,f2) context = (f1 context, f2 context)
instance Impl ([VT] -> VT, String) where
	toImpl (f1,s) context = (f1 context, s)
instance Impl (VT, [VT] -> String) where
	toImpl (t,f2) context = (t, f2 context)
instance Impl (VT, String) where
	toImpl (t,s) context = (t, s)
instance Impl ([VT] -> (VT, String)) where
	toImpl f context = f context
