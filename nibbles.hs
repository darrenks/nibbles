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
\  default = compile the nibbles program to out.hs\n\
\  -c = compactify into bytes (2 nibbles each), save to base filename.nbb\n\
\  -e = expand and print the literate form\n\
\  -v = version\n\
\\n\
\Filename and extension specifies source location and input format.\n\
\  .nbl file = literate form\n\
\  .nbb file = binary form\n\
\  empty = use stdin in literate form\n"

data ParseMode = FromLit | FromBytes deriving Eq

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
	let SExpr (Rep b lit) (SImpl t hs _) = compileIt $ case parseMode of
		FromLit -> Lit contents
		FromBytes -> Nib (concatMap fromByte contents)
	-- todo for adding args, need to add types, depth, and hs setters
	let realLit = getLit (compileIt $ Nib b) in
		if parseMode == FromLit && noOnlyLits b && realLit /= lit
		then error $ "You used an op combo that has been remapped to an extension in the binary form.\nYou wrote:\n" ++ lit ++ "\nBut this actually will mean:\n" ++ realLit ++ "\nThis usually means there is an alternative (likely shorter) way to do what you are trying to. For more infromation see the Extensions section of the docs"
		else return ()
	case filter isOpt args of
 		[] -> do
--  			putStrLn $ show t
 			hPutStrLn stderr $ "size = " ++ (show $ length b) ++ " nibbles"
 			putStrLn lit
 			putStrLn $ flatHs hs
 			header <- readFile "header.hs"
 			writeFile "out.hs" $ header ++ "\nmain=putStrLn$aToS$"++ flatHs hs
		["-c"] -> do
			let bytes = toBytes b
			let outname = (basename ++ ".nbb")
			hPutStrLn stderr $ "wrote " ++ (show $ length bytes) ++ " bytes to " ++ outname
			writeFile outname bytes
		["-e"] -> putStrLn lit
		["-v"] -> putStrLn "nibbles alpha (unstable)"
		e -> error $ "invalid option " ++ (show e) ++ "\n" ++ usage
	where isOpt = isPrefixOf "-"
	      toBytes s = map toByte $ init $ reshape 2 (s ++ [uselessOp, undefined])
	      compileIt rep = compile finish "" (rep 0)
	      getLit (SExpr (Rep _ lit) _) = lit
	      noOnlyLits b = all (/=16) b

--   hSetBuffering stdout NoBuffering
