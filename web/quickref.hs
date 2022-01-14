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
import Compile(charClassesDefs)
import Expr
import Types
import Data.List.Split -- needs cabal install -lib split
import Data.List
import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import Numeric (showHex)
import Control.Monad (when)

-- todo javascript show examples/source if hover over

main=do
	args <- getArgs
	let isSimple = args == ["simple"]
	let isFull = args == ["full"]
	let ifNotSimple f = if not isSimple then f else return ()
	let ifNotSimpleL l = if not isSimple then l else []
	descs <- getDescs
	examples <- getExamples
	let convertToTdList ((lit, nib, impl), desc, [exI, exO]) =
			[td $ styleCode $ renameIntLit lit]
			++ifNotSimpleL[td $ styleCode $ toHex nib ++ isBinOnly (fst impl)]
			++[td $ toHtml desc] ++
			toQuickRef isSimple impl lit ++
			[td $ styleCode $ do
				toHtml exI
				preEscapedToHtml " &#8594; "
				toHtml exO]
	
	let ops = concatMap selectNonWarningOps $ rawOps -- the others are for invalid literate warnings
	let (visibleOpsAndDescs,hiddenOpsAndDescs) = partition (\(o,d)->not $ isPrefixOf "hidden" d) $ zip ops descs
	let opsInfo = map (\((a,b),c) -> (a,b,c)) $ zip (visibleOpsAndDescs++if isFull then hiddenOpsAndDescs else []) (examples++repeat ["",""])
	let opsInfo2 = if isSimple then map simplifyDesc $ filter (\(a,_,_)->isOpSimple a) opsInfo else opsInfo
	hPutStrLn stderr $ show (length opsInfo2) ++ " " ++ show args ++ " ops"
	let opsInfo3 = map (\(a,b,c)->(concatLit $ convertNullNib a,b,c)) opsInfo2
	let opsTable = map convertToTdList $ sortOn (\(a,b,c)->isExtension isExtOptLite a) opsInfo3
	putStrLn $ intercalate "\n<tr>" $ splitOn "<tr>" $ Char8.unpack $ renderHtml $ docTypeHtml $ do
		textComment "This file is automatically generated from quickref.hs by reading ops.hs"
		H.head $ do
			H.title (if isSimple then "Nibbles Simple Ref" else if isFull then "Nibbles Full Ref" else "Nibbles Quick Ref")
			link ! rel "stylesheet" ! href "quickref.css"
		body $ do
			script "" ! src "sorttable.js"
			table ! class_ "sortable" $ do
				tr $ do
					th "Lit"
					ifNotSimple $ th ! class_ "sorttable_alpha" $ "Bin" 
					th "Desc"
					th "Arg Types"
					ifNotSimple $ th "Autos"
					th "Example"
				forM_ opsTable $ (tr . mapM_ Prelude.id)
			when (not isSimple) drawLegend
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
	table ! class_ "subtable" $ tr $ mapM_ (td . preEscapedToHtml) strs

toQuickRef isSimple ((types,_)) name = [
	td ! customAttribute "sorttable_customkey" sort_type $ H.div ! class_ (if length types < 2 then "center code" else "stretch code") $ do
		toSubtable $ map (replaceComplexType isSimple name) typeStrs]++
		if not isSimple then [
	td $ H.div ! class_ (if length autos < 2 then "center" else "stretch") $ toSubtable autos] else []
	where
		autos = getAutos types
		replaceComplexType True _ "vec" = "num"
		replaceComplexType True _ "fn1" = "fn"
		replaceComplexType _ "`?" "reqfn" = "fn|const"
		replaceComplexType True _ "reqfn" = "fn"
		replaceComplexType True _ "any*" = "any"
		replaceComplexType _ _ s = s
		typeStrs = catMaybes $ map typeToStr types
		sort_type = if null types then "" else rootType $ Data.List.head typeStrs

getAutos args = catMaybes $ flip map args $ \arg -> case arg of
	AutoDefault _ v -> Just $ showAuto v
	AutoData _ -> Just "data"
	AutoNot _ -> Just $ "not&#8728;"
	AutoOption desc -> Just $ italic desc
	FakeAuto desc -> Just desc
	OrAuto desc _ -> Just desc
	otherwise -> Nothing

showAuto i
		| i == autoTodoValue = "tbd"
		| i >= 2^128 = "&infin;"
		| otherwise = show i

isBinOnly :: [ArgSpec] -> String
isBinOnly args = concatMap (\t->case toBinCode t of BinCode b -> " "++showHex b ""; otherwise -> "") args

-- like isExtOpt , but allows const/req fn and commutative (>) args through
isExtOptLite t = case t of
	Cond desc _ -> False
	Fn ReqConst _ _ -> False
	otherwise -> isExtOpt t

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

simplifyDesc (a,desc,c) = (a, case elemIndex '(' desc of
	Just i -> take i desc
	Nothing -> desc
	, c)

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

drawLegend = do
	br
	b "Legend:"
	table $ do
		tr $ do
			th "symbol"
			th "meaning"
		tr $ td "{type}" >> td "parse a value of that type"
		tr $ td "any*" >> td "any type or ~ for a tuple"
		tr $ td ">type" >> td "this argument must be longer in length than the preceding (for commutative extensions)"
		tr $ td "vec" >> td "same as \"any\" but used to denote that it will vectorize if a list is given"
		tr $ td "num" >> td "int|chr"
		tr $ td "[*]" >> td "list of any type"
		tr $ td "[1]" >> td "list of non tuple"
		tr $ td "[a]" >> td "list of type variable \"a\""
		tr $ td "reqfn" >> td "fn but its argument must be used"
		tr $ td "const" >> td "fn but its argument must not be used (for extensions)"
		tr $ td "fn?" >> td "~ to denote take an extra fn argument here"
		tr $ td (i "itatlic") >> td "auto specifies \"option present\" but does not replace the arg"
	let chrows = flip map charClassesDefs $ \(symbol,meaning) -> td (toHtml symbol) ! class_ "code" >> td (toHtml meaning) ! class_ "code"
	
	let bothops = [
		("]","max"),
		("[","min"),
		("+","add"),
		("*","mult"),
		("-","sub"),
		("/","div"),
		("%","mod"),
		("^","pow"),
		(":","cons")]
	
	let foldops = [
		(">","max by fn"),
		("<","min by fn")]
		++ repeat ("","")
	
	let zipops = [
		("~","by"),
		(",","make tuple"),
		("!","abs diff"),
		("=","subscript"),
		("?","index")]
		
	let opsrows = 
		(flip map bothops $ \(sym,meaning) ->
			td sym ! class_ "code" >> td meaning ! colspan "3" ! class_ "center") ++
		(flip map (zip zipops foldops) $ \((zsym,zmean),(fsym,fmean)) ->
			td zsym ! class_ "code" >> td zmean >> td fsym ! class_ "code" >> td fmean)
		 ++ repeat (do;return())

	H.div ! class_ "wrapper" $ do
		section ! class_ "left" $ do
			br
			b "Special args:"
			table $ do	
				tr $ th "chclass" ! colspan "2" >> th "zipop" ! colspan "2" >> th "foldop" ! colspan "2"
				flip mapM_ (zip chrows opsrows) $ \(chrow,opsrow) -> tr $ do
					chrow
					opsrow
		
		section ! class_ "right" $ do
			br
			b "Inputs:"
			table $ do
				tr $ do; th "sym*";th "type";th "meaning";th "default"
				-- remind you can use args
				tr $ do;td "$" ! class_ "code";td "int";td "first int";td "100"
				tr $ do;td "@" ! class_ "code";td "str";td "first line";td "printable chars"
				tr $ do;td "_" ! class_ "code";td "[int]";td "int list";td "[]"
				tr $ do;td ";$" ! class_ "code";td "int";td "second int";td "1000"
				tr $ do;td ";@" ! class_ "code";td "str";td "second line";td "\"\""
				tr $ do;td ";_" ! class_ "code";td "str";td "all stdin";td "\"\""
				tr $ do;td ";;$" ! class_ "code";td "[[int]]";td "int matrix";td "[]"
				tr $ do;td ";;@" ! class_ "code";td "[str]";td "all lines";td "[]"
				tr $ do;td ! colspan "4" $ do
					"* command args can be of any type"
					br
					" and precede these (use Haskell syntax)"
			br
			b "Implicit Ops (top level multiple expressions):"
			table $ do
				tr $ do; th "1st type"; th "arg used"; th "meaning"
				tr $ do; td "int"; td "$ or @"; td "range"
				tr $ do; td "list"; td "@"; td "foldl"
				tr $ do; td "list"; td "$"; td "map"
				tr $ do; td "~"; td ""; td "encode data"
				