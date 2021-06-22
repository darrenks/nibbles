import Data.List
import System.Environment
import System.IO
import GHC.IO.Encoding
import System.FilePath
import System.Process

import Compile
import Header
import Polylib
import Expr
import Parse (toByte, fromByte)
import Hs(flatHs)

usage = "\
\Usage: nibbles [-c|-e|-v] [filename]\n\
\\n\
\Nibbles - a minimalistic-functional code golf language.\n\
\\n\
\Modes:\n\
\  default = compile the nibbles program to out.hs and run it\n\
\  -c = compactify into bytes (2 nibbles each), save to base filename.nbb\n\
\  -e = expand and print the literate form\n\
\  -v = version\n\
\  -norun = only generate out.hs\n\
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
	let (Impl t hs _, b, lit)  = compileIt $ case parseMode of
		FromLit -> Lit contents 0
		FromBytes -> Nib (concatMap fromByte contents) 0

	let (_, _, realLit) = compileIt $ Nib b 0
	if parseMode == FromLit && noOnlyLits b && realLit /= lit
	then error $ "You used an op combo that has been remapped to an extension in the binary form.\nYou wrote:\n" ++ lit ++ "\nBut this actually will mean:\n" ++ realLit ++ "\nThis usually means there is an alternative (likely shorter) way to do what you are trying to. For more infromation see the Extensions section of the docs"
	else return ()

	case filter isOpt args of
		ops | ops == [] || ops == ["-norun"] -> do
			if parseMode == FromLit
	 			then hPutStrLn stderr $ "size = " ++ (show $ length b) ++ " nibbles"
	 			else return ()
--  			hPutStrLn stderr lit
--  			hPutStrLn stderr $ flatHs hs
 			header <- readFile "header.hs"
 			writeFile "out.hs" $ header ++ "\nmain=putStrLn$aToS$"++ flatHs hs
 			if ops /= ["-norun"] then do
 				(_, Just hout, _, _) <- createProcess (proc "runhaskell" ["out.hs"]){ std_out = CreatePipe }
 				progOut <- hGetContents hout
 				putStr progOut
 			else do return ()
		["-c"] -> do
			let bytes = toBytes b
			let outname = (basename ++ ".nbb")
			hPutStrLn stderr $ "wrote " ++ (show $ length bytes) ++ " bytes to " ++ outname
			writeFile outname bytes
		["-e"] -> putStrLn lit
		["-v"] -> putStrLn "nibbles alpha (unstable)"
		e -> error $ "invalid option " ++ (show e) ++ "\n" ++ usage
	where
		isOpt = isPrefixOf "-"
		toBytes s = map toByte $ init $ reshape 2 (s ++ [uselessOp, undefined])
		compileIt = compile finish ""
		noOnlyLits b = all (/=16) b

--   hSetBuffering stdout NoBuffering
