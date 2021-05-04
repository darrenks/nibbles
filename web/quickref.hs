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
	let opsTable = zipWith3 convertToTdList ops descs examples
	Char8.putStrLn $ renderHtml $ docTypeHtml $ do
		textComment "This file is automatically generated from quickref.hs by reading ops.hs"
		H.head $ do
			H.title "Quick ref"
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
				forM_ opsTable $ (tr . mapM_ td)
			toHtml $ do
				H.span "*" ! class_ "code"
				toHtml " = vectorizable, "
				H.span "^" ! class_ "code"
				toHtml " = promote to list if nonlist, "
				H.span "~" ! class_ "code"
				toHtml " = coerce"
	where
		convertToTdList (lit, nib, impl) desc [exI, exO] =
			[styleCode lit,
			styleCode $ toHex nib,
			toHtml desc] ++
			toQuickRef impl ++
			[styleCode $ toHtml $ do
				toHtml exI
				preEscapedToHtml " &#8594; "
				toHtml exO]
		toHex (16:_) = ""
		toHex s = map intToDigit s
		styleCode html = H.div (toHtml html) ! class_ "code"

toQuickRef (Atom _) = [toHtml "",toHtml ""]
toQuickRef (Op types impl autos) = [
	H.div ! class_ (if length types < 2 then "center code" else "stretch code") $ do
		toHtml $ unwords $ zipWith typeToStr types ['a'..],
	H.div ! class_ (if length usableAutos < 2 then "center" else "stretch") $ toHtml (unwords $ map show usableAutos)]
	where usableAutos = filter (/= impossibleAuto) autos
	

typeToStr (Cond "list" _) n = "[" ++ [n] ++ "]"
typeToStr (Cond desc _) _ = desc
typeToStr (Coerce t) n = "~"++typeToStr t n
typeToStr (Vec t) n = "*"++typeToStr t n
typeToStr (PromoteList t) n = "^"++typeToStr t n
typeToStr (VList (VInt True)) _ = "str"
typeToStr (VList t) n = "["++typeToStr t n++"]"
typeToStr (VInt False) _ = "int"
typeToStr (VInt True) _ = "chr"
typeToStr (Fn f) _ = "fn"

getExample (c:s) | isSpace c = getExample s
getExample s | isPrefixOf "-- Example: " s || isPrefixOf "-- untested example: " s = Just (
	case elemIndex ':' s of
		Just i -> splitOn " -> " (drop (i + 2) s)
	)
getExample _ = Nothing


getDesc (c:s) | isSpace c = getDesc s
getDesc s | isPrefixOf "-- Desc: " s = Just (
	case elemIndex ':' s of
		Just i -> drop (i + 2) s
	)
getDesc _ = Nothing

getDescs = do
	ops <- readFile "ops.hs"
	return $ catMaybes $ map getDesc (lines ops)

getExamples = do
	ops <- readFile "ops.hs"
	return $ catMaybes $ map getExample (lines ops)