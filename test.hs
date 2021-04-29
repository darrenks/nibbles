import Compile
import Expr
import Types
import Polylib

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Process
import System.IO
import Data.Maybe
import Data.List
import Data.Char (isSpace)
import Data.List.Split -- needs cabal install -lib split

--todo remove spaces from lit == test

testCases = [
	(" 0", "0", 2),
	(" 1", "1", 2),
	(" -1", "-1", 2),
	(" 7", "7", 2),
	(" 8", "8", 3),
	(" 100", "100", 4),
	
	("\"abc\"", "\"abc\"", 7),
	("\"\\n\"", "\"\\n\"", 2),
	("\" \"", "\" \"", 2),
	("\"\"", "\"\"", 3),
	("\"\\200\"", "\"\\200\"", 5),
	
	("+ 1 2", "3", 5),
	("+\" - a -  - b - c - \"\" - \"", "[\"a\",\"b\",\"c\"]",33),
	(" 1","1",2)]

getExample (c:s) | isSpace c = getExample s
getExample s | isPrefixOf "-- Example: " s || isPrefixOf "-- Test: " s = Just (
	case elemIndex ':' s of
		Just i -> splitOn " -> " (drop (i + 2) s)
	)
getExample _ = Nothing

toTestFromEx [input, output] = (input, output, 0)

getTestsFromAnnotations = do
	ops <- readFile "ops.hs"
	let tests = catMaybes $ map getExample (lines ops)
	return $ map toTestFromEx tests

runHs prog = do
	writeFile "out.hs" prog
	(_, Just hout, _, _) <- createProcess (proc "runhaskell" ["out.hs"]){ std_out = CreatePipe }
	hGetContents hout

toTest(origLit, expect, size) =
	("putStrLn$aToS$"++(inspect t)++hs++";", (expect,
	outLit, origLit,
	length nib, size,
	hsFromNib, hs))
	where
		Expr t nib outLit hs = compile (NibLit origLit)
		Expr _ _ _ hsFromNib = compile (Nibbles nib)

removeSpaces = filter (not.isSpace)

printTestResult (result, (expected, outLit, origLit, nibSize, expectedSize, hsFromNib, hsFromLit)) = do
-- 	putStrLn $ "Running test: " ++ origLit
	assertEq expected result "Output mismatch"
	assertEq (removeSpaces origLit) (removeSpaces outLit) "Literate mismatch"
	if expectedSize == 0 then return ()
		else assertEq (show expectedSize) (show nibSize) "Nibble size mismatch"
	assertEq hsFromLit hsFromNib "HS mismatch"
	where assertEq expected actual reason = do
		if expected == actual then return ()
		else putStrLn $ "Test failed: " ++ origLit ++ "\nReason: " ++ reason ++ "\nExpected: " ++ expected ++ "\nActual  : " ++ actual

main=do
	additionalTests <- getTestsFromAnnotations
	let tests = map toTest (testCases ++ additionalTests)
	header <- readFile "header.hs"
	let prog = header ++ "\nmain=do;" ++ concatMap fst tests
	result <- runHs prog
	-- todo check result size matches
	mapM printTestResult $ zip (lines result) (map snd tests)
	putStrLn $ "Ran " ++ show (length tests) ++ " tests."

-- todo
-- check that consume all and only 1 expr
-- 
-- ---
-- 
-- check that literate form is non overlapping
-- check that binary form is non overlapping
-- check that binary form is exaustive

-- test main
-- test multiples output values
-- test output of different types