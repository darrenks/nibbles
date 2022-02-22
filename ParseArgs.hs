{-# LANGUAGE ScopedTypeVariables #-} -- to declare type of read result
module ParseArgs(isNibblesArg, toLetArgs) where

import Data.Char
import Data.List (nub,stripPrefix,intercalate,isSuffixOf)
import Data.Maybe
import Polylib(tupleLambda)
import Parse(errorUnderLine)

import Types

data RBracket = EofBracket | ListBracket | TupleBracket
rBracketStr EofBracket = "EOF"
rBracketStr ListBracket = "]"
rBracketStr TupleBracket = ")"


isNibblesArg :: String -> Bool
isNibblesArg ('(':_) = True
isNibblesArg ('[':_) = True
isNibblesArg ('"':_) = True
isNibblesArg ('\'':_) = True
isNibblesArg ('-':s) = isPositiveIntegerArg s
isNibblesArg s = isPositiveIntegerArg s

isPositiveIntegerArg (c:s) = isDigit c
   && not (isSuffixOf ".nbl" s)
   && not (isSuffixOf ".nbb" s)

getArgTypes :: String -> [VT]
getArgTypes s = case getArgTypesH [EofBracket] s of
   (t, "") -> t
   (_, s) -> argError s "expecting eof"
 where
   argError rest msg = errorWithoutStackTrace $ "arg io error: "++msg ++ errorUnderLine s (length s - length rest)

   appendFst x (fst, rest) = (x:fst, rest)
   catFst x (fst, rest) = (x++fst, rest)
   lstrip s = fromMaybe s $ stripPrefix " " s
   assert1 s [] = argError s "need at least one value"
   assert1 _ [t] = t
   assert1 s _ = argError s "no nested tupes, use flat tuples"

   getArgTypesH rbrackets a@(c:s)
      | isSpace c = getArgTypesH rbrackets s
      | isDigit c || c=='-' = let [(nn::Integer,rest)] = reads a in
      ([VInt], rest)

   getArgTypesH rbrackets a@('"':s) = let [(ss::String,rest)] = reads a in
      ([vstr], rest)

   getArgTypesH rbrackets a@('\'':s) = let [(ss::Char,rest)] = reads a in
      ([VChr], rest)

   getArgTypesH rbrackets a@('[':s) =
      let (et, rest) = parseMultiple (ListBracket:rbrackets) s True in case nub et of
         [] -> argError s $ "lists cannot be empty todo deduce type if needed or assume ints"
         [uniqEt] -> ([VList uniqEt], rest)
         otherwise -> argError s $ "lists types must be heterogeneous, did you mean to use a tuple?"

   getArgTypesH rbrackets a@('(':s) =
      let (ts,rest) = parseMultiple (TupleBracket:rbrackets) s False in
      (map head ts, rest)

   getArgTypesH _ s = argError s $ "cannot parse args (need - | [0-9] | [ | \" | ' ) to start a value\nat: " ++ s

   parseMultiple :: [RBracket] -> [Char] -> Bool -> ([[VT]], [Char])
   parseMultiple rbrackets s allowMult = parseMultipleH rbrackets [] (',':s) allowMult
   parseMultipleH rbrackets typesSoFar (',':s) allowMult =
      let (et, rest) = getArgTypesH rbrackets s in
         parseMultipleH rbrackets (typesSoFar++[if allowMult then et else [assert1 s et]]) rest allowMult
   parseMultipleH [EofBracket] typesSoFar "" allowMult = (typesSoFar, "")
   parseMultipleH (ListBracket:_) typesSoFar (']':rest) allowMult = (typesSoFar, rest)
   parseMultipleH (TupleBracket:_) typesSoFar (')':rest) allowMult = (typesSoFar, rest)
   parseMultipleH rbrackets _ rest _ = argError rest $ "mismatched brackets, expecting \"" ++ rBracketStr (head rbrackets) ++ "\""

convertToNibblesType :: [VT] -> String
convertToNibblesType [VChr] = "myOrd"
convertToNibblesType [VList et] = "(map "++convertToNibblesType et++")"
convertToNibblesType [t] = "id"
convertToNibblesType ts = tupleLambda (length ts) $ \varNames -> toTuple $
   zipWith (\var t->convertToNibblesType [t]++" "++var) varNames ts

toLetArgs :: [String] -> ([(String, VT, String)], String)
toLetArgs args = let
   argTypes = map getArgTypes args
   argNames = zipWith (\ts i->
      map (\j->"carg"++show i++"t"++show j) [1..length ts]
      ) argTypes [1..]
   argNamez = map toTuple argNames
   readers = zipWith (\t i->"("++toParser t++")(args!!"++show i++")") argTypes [0..]
   reader = "when (length args /= "++show (length args)++") $ errorWithoutStackTrace $ \"Error: "++show (length args)++" args found at compile time, but \"++show (length args)++\" args found at runtime (pass them in at compile time too!)\";\
      \let "++toTuple argNamez++"="++toTuple readers
   cargs = concat $ zipWith toCargs argNames argTypes
   in (cargs, reader)

toCargs :: [String] -> [VT] -> [(String, VT, String)]
toCargs argNames argTypes =
   zipWith (\n t->
         ("z"++n, t, convertToNibblesType [t] ++ " " ++ n)
      ) argNames argTypes
