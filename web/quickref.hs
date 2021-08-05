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
import Expr
import Types
import Data.List.Split -- needs cabal install -lib split
import Data.List
import Data.Char
import Data.Maybe
import System.Environment

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
			++ifNotSimpleL[td $ styleCode $ toHex nib ++ binOnlyAuto impl]
			++[td $ toHtml desc] ++
			toQuickRef isSimple impl ++
			[td $ styleCode $ do
				toHtml exI
				preEscapedToHtml " &#8594; "
				toHtml exO]
	
	let opsInfo = zip3 ops descs examples
	let opsInfo2 = if isSimple then filter isOpSimple opsInfo else opsInfo
	let opsTable = map convertToTdList $ sortOn isExtension opsInfo2
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
		isExtension ((lit, nib, op), _, _) = length nib > 1 && length lit > 1 || isExtOpt op || elem '~' lit 
		isExtOpt (Op types _ _) = any (\t -> case t of
			(Exact VAuto) -> True
			BinAuto -> True
			otherwise -> False) types
		isExtOpt _ = False
		toHex (16:_) = ""
		toHex s = map intToDigit s
		styleCode html = H.div (toHtml html) ! class_ "code"
		renameIntLit " " = "0-9"
		renameIntLit l = l
		isOpSimple a@((lit, _, op), _, _) = (not (isExtension a)||lit=="ct") && lit /= ";" && lit /= "tbd" && lit /= "z"

toQuickRef isSimple (Op types impl autos) = [
	td ! customAttribute "sorttable_customkey" sort_type $ H.div ! class_ (if length types < 2 then "center code" else "stretch code") $ do
		toHtml $ unwords $ map (replaceComplexType isSimple) $ zipWith typeToStr types ['a'..]]++
		if not isSimple then [
	td $ H.div ! class_ (if length usableAutos < 2 then "center" else "stretch") $ toHtml (unwords $ map showAuto usableAutos)] else []
	where 
		replaceComplexType True "vec" = "num"
		replaceComplexType True "fn1" = "fn"
		replaceComplexType _ s@_ = s
		usableAutos = filter (/= impossibleAuto) autos
		showAuto i | i == autoTodo = "tbd"
		           | i >= 2^128 = "inf"
		           | otherwise = show i
		sort_type = if null types then "" else rootType $ Data.List.head types
toQuickRef False _ = [td "",td ""]
toQuickRef True _ = [td ""]

typeToStr (Cond "[list]" _) n = "[[" ++ [n] ++ "]]"
typeToStr (Cond "list" _) n = "[" ++ [n] ++ "]"
typeToStr (Cond desc _) _ = desc
-- typeToStr (Coerce t) n = "~"++typeToStr t n
-- typeToStr (Vec t) n = "*"++typeToStr t n
-- typeToStr (PromoteList t) n = "^"++typeToStr t n
typeToStr (Exact (VList [VChr])) _ = "str"
typeToStr (Exact (VList [t])) n = "["++typeToStr (Exact t) n++"]"
typeToStr (Exact VInt) _ = "int"
typeToStr (Exact VChr) _ = "chr"
typeToStr (Exact VAuto) _ = "~"
typeToStr BinAuto _ = ""
typeToStr (Fn _) _ = "fn"

binOnlyAuto (Op types impl autos) = if any (\t->case t of BinAuto -> True; otherwise -> False) types then ".~" else ""
binOnlyAuto _ = ""

rootType t = stringValue $ filter (\x->isAlpha x || x=='[') $ typeToStr t 'a'

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