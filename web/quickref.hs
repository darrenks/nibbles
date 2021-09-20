{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

-- runhaskell web/quickref.hs > web/site/quickref.html

-- needs cabal install --lib blaze-html
import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_, mapM_)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List (intercalate)

import Ops
import OpsHelper
import Expr
import Types
import Data.List.Split -- needs cabal install -lib split
import Data.List
import Data.Char
import Data.Maybe
import System.Environment
import System.IO

-- todo javascript show examples/source if hover over

main=do
	args <- getArgs
	let isSimple = args == ["simple"]
	let ifNotSimple f = if not isSimple then f else return ()
	let ifNotSimpleL l = if not isSimple then l else []
	descs <- getDescs
	examples <- getExamples
	let convertToTdList ((lit, nib, impl), desc, [exI, exO]) =
			[td $ styleCode $ renameIntLit lit]
			++ifNotSimpleL[td $ styleCode $ toHex nib ++ isBinOnlyAuto impl]
			++[td $ toHtml desc] ++
			toQuickRef isSimple impl ++
			[td $ styleCode $ do
				toHtml exI
				preEscapedToHtml " &#8594; "
				toHtml exO]
	
	let ops = concatMap selectNonWarningOps $ rawOps -- the others are for invalid literate warnings
	let opsInfo = zip3 ops descs examples
	let opsInfo2 = if isSimple then filter (\(a,_,_)->isOpSimple a) opsInfo else opsInfo
	hPutStrLn stderr $ show (length opsInfo2) ++ " " ++ show args ++ " ops"
	let opsInfo3 = map (\(a,b,c)->(concatLit $ convertNullNib a,b,c)) opsInfo2
	let opsTable = map convertToTdList $ sortOn (\(a,b,c)->isExtension a) opsInfo3
	putStrLn $ intercalate "\n<tr>" $ splitOn "<tr>" $ Char8.unpack $ renderHtml $ docTypeHtml $ do
		textComment "This file is automatically generated from quickref.hs by reading ops.hs"
		H.head $ do
			H.title (if isSimple then "Nibbles Simple Ref" else "Nibbles Quick Ref")
			link ! rel "stylesheet" ! href "quickref.css"
		body $ do
			script "" ! src "sorttable.js"
			table ! class_ "sortable" $ do
				tr $ do
					th "Lit"
					ifNotSimple $ do
						th ! class_ "sorttable_alpha sorttable_sorted" $ do
							"Bin"
							H.span (preEscapedToHtml "&nbsp;&#x25BE;") ! A.id "sorttable_sortfwdind"
					th "Desc"
					th "Arg Types"
					ifNotSimple $ th "Autos"
					th "Example"
				forM_ opsTable $ (tr . mapM_ Prelude.id)
-- 			toHtml $ do
-- 				H.span "*" ! class_ "code"
-- 				toHtml " = vectorizable, "
-- 				H.span "^" ! class_ "code"
-- 				toHtml " = promote to list if nonlist, "
-- 				H.span "~" ! class_ "code"
-- 				toHtml " = coerce"
	where
		toHex (16:_) = ""
		toHex s = map intToDigit s
		styleCode html = H.div (toHtml html) ! class_ "code"
		renameIntLit " " = "0-9"
		renameIntLit l = l

toSubtable strs = do
	table ! class_ "subtable" $ tr $ mapM_ (td . toHtml) strs

toQuickRef isSimple ((types,_)) = [
	td ! customAttribute "sorttable_customkey" sort_type $ H.div ! class_ (if length types < 2 then "center code" else "stretch code") $ do
		toSubtable $ map (replaceComplexType isSimple) typeStrs]++
		if not isSimple then [
	td $ H.div ! class_ (if length autos < 2 then "center" else "stretch") $ toSubtable autos] else []
	where
		autos = getAutos types
		replaceComplexType True "vec" = "num"
		replaceComplexType True "fn1" = "fn"
		replaceComplexType _ s@_ = s
		typeStrs = catMaybes $ map typeToStr types
		sort_type = if null types then "" else rootType $ Data.List.head typeStrs

typeToStr (Cond desc _) = Just desc
typeToStr (Auto binOnly) = if binOnly then Nothing else Just "~"
typeToStr (AutoOption _) = Nothing
typeToStr (AutoNot (Fn _ _ _)) = Nothing
typeToStr (OrAuto _ a) = typeToStr a
typeToStr (Fn True _ _) = Just "fn"
typeToStr (Fn False _ _) = Just "fn|C"
typeToStr (AutoDefault t _) = typeToStr t
typeToStr (AutoData t) = typeToStr t
typeToStr (ParseArg desc _) = Just $ "{"++desc++"}"
typeToStr (OptionalFn _) = Just $ "fn?"

prettyType VInt = "int"
prettyType (VList [VChr]) = "str"
prettyType VChr = "chr"

getAutos args = catMaybes $ flip map args $ \arg -> case arg of
	AutoDefault _ v -> Just $ showAuto v
	AutoData _ -> Just "data"
	AutoNot _ -> Just "not."
	AutoOption desc -> Just desc
	OrAuto desc _ -> Just desc
	otherwise -> Nothing

showAuto i
		| i == autoTodoValue = "tbd"
		| i >= 2^128 = "inf"
		| otherwise = show i

isBinOnlyAuto (args,_) = if any (\t->case t of Auto True -> True; otherwise -> False) args then " 0" else ""

rootType t = stringValue $ filter (\x->isAlpha x || x=='[') $ t

getExample (c:s) | isSpace c = getExample s
getExample s | isPrefixOf "-- Example" s || isPrefixOf "-- untested example: " s = Just (
	case elemIndex ':' s of
		Just i -> splitOn " -> " (drop (i + 2) s)
	)
getExample _ = Nothing


getDesc (c:s) | isSpace c = getDesc s
getDesc s | isPrefixOf "-- Desc: " s = Just (
	case elemIndex ':' s of
		Just i -> Data.List.head $ splitOn "." (drop (i + 2) s)
	)
getDesc _ = Nothing

getDescs = do
	ops <- readFile "ops.hs"
	return $ catMaybes $ map getDesc (lines ops)

getExamples = do
	ops <- readFile "ops.hs"
	return $ catMaybes $ map getExample (lines ops)

selectNonWarningOps ops = if length ops > 1 then filter hasBin ops else ops
	where hasBin (_,_,nib,_) = not (null nib)

concatLit :: (Bool, [String], [Int], Operation) -> (String, [Int], Operation)
concatLit (b, lit, i, o) = (concat lit, i, o)