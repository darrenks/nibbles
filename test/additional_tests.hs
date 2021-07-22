	-- Test: .,3 ~$1 -> [(1,1),(2,1),(3,1)]
	-- Test: .,1 ~~1 2 3 -> [(1,2,3)]

	-- Test: testCoerce2 1 1 -> "VInt"
	-- Test: testCoerce2 1 'a' -> "VInt"
	-- Test: testCoerce2 1 ,3 -> "VList [VInt]"
	-- Test: testCoerce2 1 "abc" -> "VList [VChr]"
	-- Test: testCoerce2 1 .,3,3 -> "VList [VList [VInt]]"
	-- Test: testCoerce2 1 .,3"abc" -> "VList [VList [VChr]]"

	-- Test: testCoerce2 'a' 'a' -> "VChr"
	-- Test: testCoerce2 'a' ,3 -> "VList [VInt]"
	-- Test: testCoerce2 'a' "abc" -> "VList [VChr]"
	-- Test: testCoerce2 'a' .,3,3 -> "VList [VList [VInt]]"
	-- Test: testCoerce2 'a' .,3"abc" -> "VList [VList [VChr]]"

	-- Test: testCoerce2 ,3 ,3 -> "VList [VInt]"
	-- Test: testCoerce2 ,3 "abc" -> "VList [VChr]"
	-- Test: testCoerce2 ,3 .,3,3 -> "VList [VList [VInt]]"
	-- Test: testCoerce2 ,3 .,3"abc" -> "VList [VList [VChr]]"

	-- Test: testCoerce2 "abc" "abc" -> "VList [VChr]"
	-- Test: testCoerce2 "abc" .,3,3 -> "VList [VChr]"
	-- Test: testCoerce2 "abc" .,3"abc" -> "VList [VList [VChr]]"

	-- Test: testCoerce2 .,3,3 .,3,3 -> "VList [VList [VInt]]"
	-- Test: testCoerce2 .,3,3 .,3"abc" -> "VList [VList [VChr]]"

	-- Test: testCoerce2 .,3"abc" .,3"abc" -> "VList [VList [VChr]]"
------------------------------------------------------------------	
	-- Test: testCoerceToInt 1 -> 1
	-- Test: testCoerceToInt 'a' -> 97
	-- Test: testCoerceToInt ,3 -> 6
	-- Test: testCoerceToInt "abc" -> 0
	-- Test: testCoerceToInt "12" -> 12
	-- Test: testCoerceToInt .,3,3 -> 18
	-- Test: testCoerceToInt .,3"3" -> 9
	
	-- Test: testCoerceToChr 100 -> 'd'
	-- Test: testCoerceToChr 'a' -> 'a'
	-- Test: testCoerceToChr :100 200 -> 'd'
	-- Test: testCoerceToChr "abc" -> 'a'
	-- Test: testCoerceToChr .,3:100 200 -> 'd'
	-- Test: testCoerceToChr .,3"abc" -> 'a'
	
	-- Test: testCoerceToListInt 100 -> [100]
	-- Test: testCoerceToListInt 'a' -> [97]
	-- Test: testCoerceToListInt ,3 -> [1,2,3]
	-- Test: testCoerceToListInt "abc" -> [97,98,99]
	-- Test: testCoerceToListInt .,3,3 -> [1,2,3,1,2,3,1,2,3]
	-- Test: testCoerceToListInt .,3"abc" -> [0,0,0]
	-- Test: testCoerceToListInt .,3"12" -> [12,12,12]

	-- Test: testCoerceToStr 100 -> "100"
	-- Test: testCoerceToStr 'a' -> "a"
	-- Test: testCoerceToStr ,3 -> "123"
	-- Test: testCoerceToStr "abc" -> "abc"
	-- Test: testCoerceToStr .,3,3 -> "123123123"
	-- Test: testCoerceToStr .,3"abc" -> "abcabcabc"

	-- Test: testCoerceToListListInt 100 -> [[100]]
	-- Test: testCoerceToListListInt 'a' -> [[97]]
	-- Test: testCoerceToListListInt ,3 -> [[1,2,3]]
	-- Test: testCoerceToListListInt "abc" -> [[97,98,99]]
	-- Test: testCoerceToListListInt .,3,3 -> [[1,2,3],[1,2,3],[1,2,3]]
	-- Test: testCoerceToListListInt .,3"abc" -> [[97,98,99],[97,98,99],[97,98,99]]
	
	-- Test: testCoerceToListStr 100 -> ["100"]
	-- Test: testCoerceToListStr 'a' -> ["a"]
	-- Test: testCoerceToListStr ,3 -> ["1","2","3"]
	-- Test: testCoerceToListStr "abc" -> ["abc"]
	-- Test: testCoerceToListStr .,3,3 -> ["1","2","3","1","2","3","1","2","3"]
	-- Test: testCoerceToListStr .,3"abc" -> ["abc","abc","abc"]
	
	-- Test: testFinish 123 -> "123"
	-- Test: testFinish 'c' -> "c"
	-- Test: testFinish "abc" -> "abc"
	-- Test: testFinish ,3 -> "1\n2\n3\n"
	-- Test: testFinish .,3,3 -> "1 2 3\n1 2 3\n1 2 3\n"
	-- Test: testFinish .,3"abc" -> "abc\nabc\nabc\n"
	-- Test: testFinish .,2.,2,2 -> "12 12\n12 12\n"
	-- Test: testFinish .,2.,2"ab" -> "ab ab\nab ab\n"
	--- Test: testFinish z ,3 "abc" -> "1\n2\n3\n"
	
	-- Test "hi\n": \@ -> "ih"
	-- RawTest: 1 2 -> "12\n"
	
	--- Test implicit args
	-- Test: ;7.,5+$ -> 7,[8,9,10,11,12]
	-- Test: /,5+ -> 15
	-- Test: /,5++$@ -> 25
	
	--- Test special ints if stdin empty
	-- Test "": $ -> 100
	-- Test "": ;$ -> 1000

