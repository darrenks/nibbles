import Data.Char
import Data.List
import System.Environment
import System.IO
-- import GHC.IO.Encoding
import System.FilePath
import Numeric (showOct, readDec)
import Compile
import Stdlib

intToNib n=cpind '0':init digits ++ [last digits + 8]
	where digits = map digitToInt $ showOct n ""

strToNib :: String -> Code
strToNib "" = [2,0]
-- \n
-- ' '
-- raw byte
strToNib s=concatMap (\(c,last)->let oc = ord c in case c of
	'\n' -> [last]
	' ' -> [1+last]
	c | oc > 126 || oc < 32 -> [last+7, 15, div oc 16, mod oc 16]
	otherwise -> [last+div oc 16, mod oc 16]
	) (zip s $ take (length s - 1) (repeat 0) ++ [8])

fromLit :: String -> Code
fromLit [] = []
fromLit (c:s)
	| c=='#' = fromLit (tail $ dropWhile (/='\n') s)
	| isSpace c = fromLit s
	| isDigit c = intToNib dec ++ fromLit decs
	| c=='-' && head s=='1' = [cpind '0', 0] ++ fromLit (tail s)
	| c=='"' = case (reads::ReadS String)$c:s of
	   [(str, strs)] -> cpind '"':strToNib str ++ fromLit strs
	   otherwise -> error $ "unparsable string: " ++ (c:s)
	| otherwise = cpind c : fromLit s
	where [(dec, decs)] = readDec$c:s

toBytes :: Code -> String
toBytes s = map toByte $ init $ reshape 2 (s ++ [cpind '~', undefined])

fromBytes :: String -> Code
fromBytes = concatMap fromByte

usage = "\
\Usage: nibbles [-c|-e|-v] [filename]\n\
\\n\
\Nibbles - a minimalistic-functional code golf language.\n\
\\n\
\Modes:\n\
\  default = run the nibbles program\n\
\  -c = compactify into bytes (2 nibbles each), save to base filename.nbb\n\
\  -e = expand and print the literate form\n\
\  -v = version\n\
\\n\
\Filename and extension specifies source location and input format.\n\
\  .nbl file = literate form\n\
\  .nbb file = binary form\n\
\  empty = use stdin in literate form\n"

main=do
-- 	setLocaleEncoding char8
	args <- getArgs
	let (contentsIO, basename, parseMode) = case filter (not.isOpt) args of
		[] -> (getContents, "a", fromLit)
		[f] -> (readFile f, basename, case ext of
			".nbb" -> fromBytes
			".nbl" -> fromLit
			otherwise -> error "file extension must be .nbb or .nbl so we know input format")
				where (basename, ext) = splitExtension f
		e -> error $ "too many filename args:" ++ (show e) ++ "\n" ++ usage
	contents <- contentsIO
	let nibbles = parseMode contents
	let (Expr rest t lit hs) = getValue(nibbles, [], 0, "", [])
	-- todo for adding args, need to add types, depth, and hs setters
	case filter isOpt args of
 		[] -> do
 			putStrLn $ (show t) ++ ", size = " ++ (show $ length nibbles) ++ ", unused = " ++ (show $ length rest) 
 			putStrLn $ lit ++ "\n" ++ hs
 			writeFile "out.hs" $ "\
 				\import Data.List\n\
				\import Data.Char\n\
				\import Data.Maybe\n\
				\import Data.List.Split -- needs install\n\
				\import Stdlib\n\
				\main=putStrLn$"++finish t++hs
		["-c"] -> do
			let bytes = toBytes nibbles
			let outname = (basename ++ ".nbb")
			hPutStrLn stderr $ "wrote " ++ (show $ length bytes) ++ " bytes to " ++ outname
			writeFile outname bytes
		["-e"] -> putStrLn lit
		["-v"] -> putStrLn "nibbles alpha"
		e -> error $ "invalid option " ++ (show e) ++ "\n" ++ usage
	where isOpt = isPrefixOf "-"

--   hSetBuffering stdout NoBuffering
