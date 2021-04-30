{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

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

--todo style with class=code

main=do
	descs <- getDescs
	examples <- getExamples
	let opsTable = zipWith3 convertToTdList (getOps (Thunk (NibLit "") [])) descs examples
	Char8.putStrLn $ renderHtml $ docTypeHtml $ do
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
				forM_ opsTable $ (tr . mapM_ (td . toHtml))
			p $ toHtml "* = vectorizable, ^ = promote to list if needed, ~ = coerce"
	where
		convertToTdList (lit, nib, impl) desc ex =
			[lit, toHex nib, desc] ++ toQuickRef impl ++ [intercalate " ->  " ex]
		toHex (16:_) = ""
		toHex s = map intToDigit s

toQuickRef (Atom _) = ["",""]
toQuickRef (Op types impl autos) = [intercalate ", " $ zipWith typeToStr types ['a'..], unwords $ map show autos]

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