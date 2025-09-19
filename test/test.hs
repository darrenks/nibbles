{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

import Compile
import Expr
import Types
import Polylib

import System.Process -- needs cabal install --lib process
import System.IO
import Control.Monad (when)
import Data.Maybe
import Data.List
import Data.Char (isSpace,isAlpha)
import Data.List.Split -- needs cabal install --lib split
import Hs(flatHs)

getExample (c:s) | isSpace c = getExample s
getExample s | isPrefixOf "-- Example" s || isPrefixOf "-- Test" s || isPrefixOf "-- RawTest" s || isPrefixOf "-- hidden Example" s = Just (
   case elemIndex ':' s of
      Just i -> (input, stdin, output, size, isPrefixOf "-- RawTest" s) where
         beforeColon = take i s
         afterColon = drop (i + 2) s
         [input,output] = splitOn " -> " afterColon
         stdin = case elemIndex '"' beforeColon of
            Just quoteIndex -> fst $ head $ reads $ drop quoteIndex beforeColon:: String
            Nothing -> ""
         size = case splitOn "(size" beforeColon of
            [pre,post] -> read (head $ splitOn ")" post)
            otherwise -> 0
   )
getExample _ = Nothing

isErrorResult (_, _, output, _, _) = isPrefixOf "error" output

getTestsFromAnnotations = do
   ops1 <- readFile "Ops.hs"
   ops2 <- readFile "test/AdditionalTests.hs"
   ops3 <- readFile "test/TutorialTests.hs"
   return $ catMaybes $ map getExample (lines ops1 ++ lines ops2 ++ lines ops3)

runHs prog = do
   writeFile "out.hs" prog
   (_, Just hout, _, _) <- createProcess (proc "runhaskell" ["out.hs"]){ std_out = CreatePipe }
   hGetContents hout

toTest(origLit, stdin, expect, size, isRaw) =
   if null litWarnings then ("\n   let input=sToA"++show stdin++"\n   let rawProgArgs=[]\n   "++(if isRaw then "print$aToS$finishLn" else "putStrLn$aToS")++testHs, (expect,
      outLit, origLit,
      length nib, if any (==16) nib then -1 else size, -- ignore only lit for bin rep
      flatHs (implCode implFromNib), testHs))
   else errorWithoutStackTrace $ intercalate "\n" litWarnings
   where
      cc = if isRaw then compile finish "" [] else compile (\t->(inspect t,False))  "," []
      (implFromLit, nib, outLit, litWarnings) =  cc $ Lit origLit origLit 0
      testHs = flatHs (implCode implFromLit)
      (implFromNib, _, _, _) = cc $ Nib (padToEvenNibbles nib) 0

removeSpaces = filter (not.isSpace)

printTestResult (result, (expected, outLit, origLit, nibSize, expectedSize, hsFromNib, hsFromLit)) = do
--    putStrLn $ "Running test: " ++ origLit
   assertEq expected result "Output mismatch"
   when (not $ litOnlyFeatures origLit) $ assertEq (removeSpaces origLit) (removeSpaces outLit) "Literate mismatch"
   when (expectedSize /= -1) $ do
      when (expectedSize /= 0) $ assertEq (show expectedSize) (show nibSize) "Nibble size mismatch"
      assertEq hsFromLit hsFromNib "HS mismatch"
   where
      assertEq expected actual reason = do
         if expected == actual then return ()
         else putStrLn $ "Test failed: " ++ origLit ++ "\nReason: " ++ reason ++ "\nExpected: " ++ expected ++ "\nActual  : " ++ actual
      litOnlyFeatures lit = isInfixOf "sets " lit
         || isInfixOf "let " lit
         || any (\next -> not $ null $ takeWhile isAlpha next) (splitOn "\\" lit)

main=do
   let ?isSimple = False
   testCases <- getTestsFromAnnotations
   let tests = map toTest $ take 1000 $ drop 0 $ filter (not.isErrorResult) testCases
   let (_,(_,_,x,_,_,_,_)) = last tests
   putStrLn $ x
   header <- readFile "Header.hs"
   let prog = header ++ "\nmain=do" ++ concatMap fst tests
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