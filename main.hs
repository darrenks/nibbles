import Data.List
import System.Environment
import System.IO
import GHC.IO.Encoding
import System.FilePath

import Compile
import Header
import Polylib
import Expr
import Parse (toByte, fromByte)

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

data ParseMode = FromLit | FromBytes

main=do
	setLocaleEncoding char8
	args <- getArgs
	let (contentsIO, basename, parseMode) = case filter (not.isOpt) args of
		[] -> (getContents, "a", FromLit)
		[f] -> (readFile f, basename, case ext of
			".nbb" -> FromBytes
			".nbl" -> FromLit
			otherwise -> error "file extension must be .nbb or .nbl")
				where (basename, ext) = splitExtension f
		e -> error $ "too many filename args:" ++ (show e) ++ "\n" ++ usage
	contents <- contentsIO
	let Expr t b lit hs = case parseMode of
		FromLit -> compile $ Lit contents 0
		FromBytes -> compile $  Nib (concatMap fromByte contents) 0
	-- todo for adding args, need to add types, depth, and hs setters
	case filter isOpt args of
 		[] -> do
 			putStrLn $ (show t) ++ ", size = " ++ (show $ length b) ++ ", unused = " ++ (show $ "length rest") 
 			putStrLn $ lit ++ "\n" ++ hs
 			header <- readFile "header.hs"
 			writeFile "out.hs" $ header ++ "\nmain=putStrLn$"++finish t++hs
		["-c"] -> do
			let bytes = toBytes b
			let outname = (basename ++ ".nbb")
			hPutStrLn stderr $ "wrote " ++ (show $ length bytes) ++ " bytes to " ++ outname
			writeFile outname bytes
		["-e"] -> putStrLn lit
		["-v"] -> putStrLn "nibbles alpha"
		e -> error $ "invalid option " ++ (show e) ++ "\n" ++ usage
	where isOpt = isPrefixOf "-"
	      toBytes s = map toByte $ init $ reshape 2 (s ++ [0, undefined])

--   hSetBuffering stdout NoBuffering
