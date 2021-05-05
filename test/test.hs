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

getExample (c:s) | isSpace c = getExample s
getExample s | isPrefixOf "-- Example" s || isPrefixOf "-- Test" s = Just (
	case elemIndex ':' s of
		Just i -> (input, output, size) where
			[input,output] = splitOn " -> " (drop (i + 2) s)
			size = case splitOn "(size" (take i s) of
				[pre,post] -> read (head $ splitOn ")" post)
				otherwise -> 0
	)
getExample _ = Nothing


getTestsFromAnnotations = do
	ops <- readFile "ops.hs"
	return $ catMaybes $ map getExample (lines ops)

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
		Expr t nib outLit hs = compile (Lit origLit 0)
		Expr _ _ _ hsFromNib = compile (Nib nib 0)

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
	testCases <- getTestsFromAnnotations
	let tests = map toTest testCases
	header <- readFile "header.hs"
	let prog = header ++ "\nmain=do;" ++ concatMap fst tests
	result <- runHs prog
	-- todo check result size matches
	mapM printTestResult $ zip (lines result) (map snd tests)
	putStrLn $ "Ran " ++ show (length tests) ++ " tests."

-- todo
-- 
-- check that literate form is non overlapping
-- check that binary form is non overlapping
-- check that binary form is exaustive

-- test main
-- test multiples output values
-- test output of different types

-- test finish