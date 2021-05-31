{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

-- runhaskell web/quickref.hs > web/site/quickref.html

-- needs cabal install --lib blaze-html
import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_, mapM_)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.ByteString.Lazy.Char8 as Char8 (putStrLn)

import Ops
import Expr
import Types
import Data.List.Split -- needs cabal install -lib split
import Data.List
import Data.Char
import Data.Maybe

-- todo javascript show examples/source if hover over

main=do
	descs <- getDescs
	examples <- getExamples
	let opsInfo = zip3 ops descs examples
	let opsTable = map convertToTdList $ sortOn isExtension opsInfo
	Char8.putStrLn $ renderHtml $ docTypeHtml $ do
		textComment "This file is automatically generated from quickref.hs by reading ops.hs"
		H.head $ do
			H.title "Nibbles Quick Ref"
			link ! rel "stylesheet" ! href "quickref.css"
		body $ do
			script "" ! src "sorttable.js"
			table ! class_ "sortable" $ do
				tr $ do
					th "Lit"
					th ! class_ "sorttable_alpha sorttable_sorted" $ do
						"Bin"
						H.span (preEscapedToHtml "&nbsp;&#x25BE;") ! A.id "sorttable_sortfwdind"
					th "Desc"
					th "Arg Types"
					th "Autos"
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
		isExtension ((_, lit, nib, op), _, _) = length nib > 1 && length lit > 1 || isExtOpt op
		isExtOpt (Op types _ _) = any (\t -> case t of
			(Exact VAuto) -> True
			otherwise -> False) types
		isExtOpt _ = False
		convertToTdList ((_, lit, nib, impl), desc, [exI, exO]) =
			[td $ styleCode $ lit, -- styleCode
			td $ styleCode $ toHex nib, -- styleCode
			td $ toHtml desc] ++
			toQuickRef impl ++
			[td $ styleCode $ do -- styleCode
				toHtml exI
				preEscapedToHtml " &#8594; "
				toHtml exO]
		toHex (16:_) = ""
		toHex s = map intToDigit s
		styleCode html = H.div (toHtml html) ! class_ "code"

toQuickRef (Op types impl autos) = [
	td ! customAttribute "sorttable_customkey" sort_type $ H.div ! class_ (if length types < 2 then "center code" else "stretch code") $ do
		toHtml $ unwords $ zipWith typeToStr types ['a'..],
	td $ H.div ! class_ (if length usableAutos < 2 then "center" else "stretch") $ toHtml (unwords $ map showAuto usableAutos)]
	where 
		usableAutos = filter (/= impossibleAuto) autos
		showAuto i | i == autoTodo = "tbd"
		           | otherwise = show i
		sort_type = if null types then "" else rootType $ Data.List.head types
toQuickRef _ = [td "",td ""]

typeToStr (Cond "[list]" _) n = "[[" ++ [n] ++ "]]"
typeToStr (Cond "list" _) n = "[" ++ [n] ++ "]"
typeToStr (Cond desc _) _ = desc
-- typeToStr (Coerce t) n = "~"++typeToStr t n
-- typeToStr (Vec t) n = "*"++typeToStr t n
-- typeToStr (PromoteList t) n = "^"++typeToStr t n
typeToStr (Exact (VList VChr)) _ = "str"
typeToStr (Exact (VList t)) n = "["++typeToStr (Exact t) n++"]"
typeToStr (Exact VInt) _ = "int"
typeToStr (Exact VChr) _ = "chr"
typeToStr (Exact VAuto) _ = "~"
typeToStr (Fn nargs f) _ = "fn" -- ++show nargs

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