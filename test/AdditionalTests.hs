-- Some tests for handling of saves within functions (these exposed bugs in the past)
-- Test: ``; 5 ;4 2 3 -> 3
-- Test: ;~ 5 -;4 @ -> -1
-- That last ;$ is "ints"
-- Test: ;~ 5 = $ ; :0 ;$ -> 0

-- tuple
-- Test: .,3 ~$1 -> [(1,1),(2,1),(3,1)]
-- Test: old_zip_ .,1 ~~$ 2 3 :4~ -> [(1,2,3,4)]

-- int versus integer
-- Test: %$=1"cde" -> 1

-- Test: testCoerce2 1 1 -> "[VInt]"
-- Test: testCoerce2 1 'a' -> "[VList [VChr]]"
-- Test: testCoerce2 1 ,3 -> "[VList [VInt]]"
-- Test: testCoerce2 1 "abc" -> "[VList [VChr]]"
-- Test: testCoerce2 1 :,3~ -> "[VList [VList [VInt]]]"
-- Test: testCoerce2 1 :"abc"~ -> "[VList [VList [VChr]]]"

-- Test: testCoerce2 'a' 'a' -> "[VChr]"
-- Test: testCoerce2 'a' ,3 -> "[VList [VList [VChr]]]"
-- Test: testCoerce2 'a' "abc" -> "[VList [VChr]]"
-- Test: testCoerce2 'a' :,3~ -> "[VList [VList [VList [VChr]]]]"
-- Test: testCoerce2 'a' :"abc"~ -> "[VList [VList [VChr]]]"

-- Test: testCoerce2 ,3 ,3 -> "[VList [VInt]]"
-- Test: testCoerce2 ,3 "abc" -> "[VList [VChr]]"
-- Test: testCoerce2 ,3 :,3~ -> "[VList [VList [VInt]]]"
-- Test: testCoerce2 ,3 :"abc"~ -> "[VList [VList [VChr]]]"

-- Test: testCoerce2 "abc" "abc" -> "[VList [VChr]]"
-- Test: testCoerce2 "abc" :,3~ -> "[VList [VChr]]"
-- Test: testCoerce2 "abc" :"abc"~ -> "[VList [VList [VChr]]]"

-- Test: testCoerce2 :,3~ :,3~ -> "[VList [VList [VInt]]]"
-- Test: testCoerce2 :,3~ :"abc"~ -> "[VList [VList [VChr]]]"

-- Test: testCoerce2 :"abc"~ :"abc"~ -> "[VList [VList [VChr]]]"

-- Test: testCoerce2 old_zip_,3"abc" ,3 -> "[VList [VInt]]"
-- Test: testCoerce2 ,3 old_zip_,3"abc" -> "[VList [VInt]]"
-- Test: testCoerce2 old_zip_,3"abc" old_zip_"abc",3 -> "[VList [VList [VChr],VList [VChr]]]"
------------------------------------------------------------------
-- Test: testCoerceToInt 1 -> 1
-- Test: testCoerceToInt 'a' -> 97
-- Test: testCoerceToInt ,3 -> 6
-- Test: testCoerceToInt "abc" -> 0
-- Test: testCoerceToInt "12" -> 12
-- Test: testCoerceToInt ^3:,3~ -> 18
-- Test: testCoerceToInt ^3:"3"~ -> 9

-- Test: testCoerceToChr 100 -> 'd'
-- Test: testCoerceToChr 'a' -> 'a'
-- Test: testCoerceToChr :100 200 -> 'd'
-- Test: testCoerceToChr "abc" -> 'a'
-- Test: testCoerceToChr .,3 .,3 +*100$@ -> 'e'
-- Test: testCoerceToChr ^3:"abc"~ -> 'a'

-- Test: testCoerceToListInt 100 -> [100]
-- Test: testCoerceToListInt 'a' -> [97]
-- Test: testCoerceToListInt ,3 -> [1,2,3]
-- Test: testCoerceToListInt "abc" -> [97,98,99]
-- Test: testCoerceToListInt ^3:,3~ -> [1,2,3,1,2,3,1,2,3]
-- Test: testCoerceToListInt ^3:"abc"~ -> [0,0,0]
-- Test: testCoerceToListInt ^3:"12"~ -> [12,12,12]

-- Test: testCoerceToStr 100 -> "100"
-- Test: testCoerceToStr 'a' -> "a"
-- Test: testCoerceToStr ,3 -> "123"
-- Test: testCoerceToStr "abc" -> "abc"
-- Test: testCoerceToStr ^3:,3~ -> "123123123"
-- Test: testCoerceToStr ^3:"abc"~ -> "abcabcabc"

-- Test: testCoerceToListListInt 100 -> [[100]]
-- Test: testCoerceToListListInt 'a' -> [[97]]
-- Test: testCoerceToListListInt ,3 -> [[1,2,3]]
-- Test: testCoerceToListListInt "abc" -> [[97,98,99]]
-- Test: testCoerceToListListInt ^3:,3~ -> [[1,2,3],[1,2,3],[1,2,3]]
-- Test: testCoerceToListListInt ^3:"abc"~ -> [[97,98,99],[97,98,99],[97,98,99]]

-- Test: testCoerceToListStr 100 -> ["100"]
-- Test: testCoerceToListStr 'a' -> ["a"]
-- Test: testCoerceToListStr ,3 -> ["1","2","3"]
-- Test: testCoerceToListStr "abc" -> ["abc"]
-- Test: testCoerceToListStr ^3:,3~ -> ["1","2","3","1","2","3","1","2","3"]
-- Test: testCoerceToListStr ^3:"abc"~ -> ["abc","abc","abc"]

-- Test: testCoerceToX 4 "5" -> 5
-- Test: testCoerceToX ~""4 ~4 "3" $ -> "4",3
-- Test: testCoerceToX ~""4 5 $ -> "5",0
-- Test: testCoerceToX 4 ~"5"3 $ -> 5,100
-- Test: testCoerceToX ,3 old_zip_\,3"abc" -> [3,2,1]
-- Test: testCoerceToX old_zip_,3"abc" \,3 -> [(3,' '),(2,' '),(1,' ')]

-- Test: testFinish 123 -> "123"
-- Test: testFinish 'c' -> "c"
-- Test: testFinish "abc" -> "abc"
-- Test: testFinish ,3 -> "1\n2\n3\n"
-- Test: testFinish ^3:,3~ -> "1 2 3\n1 2 3\n1 2 3\n"
-- Test: testFinish ^3:"abc"~ -> "abc\nabc\nabc\n"
-- Test: testFinish ^2:^2:,2~~ -> "12 12\n12 12\n"
-- Test: testFinish ^2:^2:"ab"~~ -> "ab ab\nab ab\n"
-- Test: testFinish old_zip_ ,3 "abc" -> "1 a\n2 b\n3 c\n"

-- Test "hi\n": \@ -> "ih"
-- RawTest: 1 2 -> "12\n"

--- Test implicit args
-- Test: ;7.,5+$ -> 7,[8,9,10,11,12]
-- Test: /,5+ -> 15
-- Test: /,5++@$ -> 25

--- Test special ints if stdin empty
-- Test "": $ -> 100
-- Test "": ;$ -> 1000

-- RawTest "1 2 3": *2$ -> "2\n4\n6\n"

-- RawTest implicit foldL: 3 @ -> "1\n"

----- Test implicit ops with tuples:
-- RawTest: old_zip_,3"abc" ~$;$ "" $ -> "3a\n"
-- RawTest: old_zip_,3"abc" $ -> "1\n2\n3\n"
-- RawTest: ,3 ~$"c" -> "1 c\n2 c\n3 c\n"
--- RawTest: old_zip_,3"abc" "-" -> "1-a\n2-b\n3-c\n"
-- RawTest: old_zip_,3"abc" 5 -> "1 a\n2 b\n3 c\n5\n"
-- RawTest: 3~$"c" -> "1 c\n2 c\n3 c\n"

---- Test input types
-- RawTest "12a": $ -> "12\n"
-- RawTest "12a": @ -> "12a\n"
-- RawTest "1 2 3": p_ -> "[1,2,3]\n"
-- RawTest "1\n2\n3": p_ -> "[1,2,3]\n"
-- RawTest "1\n2a": p;$ -> "2\n"
--- RawTest "1\n2a\n3": p;$ -> "2\n" -- todo if use 2nd but not first, don't map again?
-- RawTest "1\n2a\n3": ;@ -> "2a\n"
-- RawTest "ab\ncd": ;_ -> "ab\ncd\n"
-- RawTest "1 2\n3": p;;$ -> "[[1,2],[3]]\n"
-- RawTest "1 2\n3": p;;@ -> "[\"1 2\",\"3\"]\n"

-- RawTest odd nibble data (size 3): $~5 -> "5\n"
-- RawTest empty nibble data (size 2): $~0 -> "0\n"

---- Extension collision tests ----
-- 66 is safe since pointless to assign twice
-- 666 make it mean 6 66 (; letfn) but this is a collision
-- Test: ;~; 0 +1$ -> 1
-- 660 make it mean 66 0 (letfn ~) but this is a collision
-- Test: ;~ ~1 2 +@$ -> 3
