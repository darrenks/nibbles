{-# LANGUAGE QuasiQuotes #-}

import Data.List(isPrefixOf)
import System.Environment
import System.IO
import GHC.IO.Encoding
import System.FilePath
import System.Process
import System.Exit

import FileQuoter
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

-- Set this as a constant rather than read it to create a stand-alone binary.
headerRaw :: String
headerRaw = [litFile|header.hs|]

main=do
	setLocaleEncoding char8
	args <- getArgs
	let (contentsIO, basename, parseMode) = case filter (not.isOpt) args of
		[] -> (getContents, "a", FromLit)
		[f] -> (readFile f, base, case ext of
			".nbb" -> FromBytes
			".nbl" -> FromLit
			_ -> error "file extension must be .nbb or .nbl")
				where (base, ext) = splitExtension f
		e -> error $ "too many filename args:" ++ (show e) ++ "\n" ++ usage
	contents <- contentsIO
	let (impl, b, lit)  = compile finish "" $ case parseMode of
		FromLit -> Lit contents contents 0
		FromBytes -> Nib (concatMap fromByte contents) 0

	case filter isOpt args of
		ops | ops == [] || ops == ["-norun"] -> do
			if parseMode == FromLit
	 			then do
	 				hPutStrLn stderr $ "size = " ++ (show $ length b) ++ " nibbles"
	 				let (_,_,binLit) = compile finish "" (Nib b 0)
	 				if any (==16) b then do
	 					hPutStrLn stderr "Warning: you are using literal only ops"
	 				-- This warning is necessary because the current accidental extension detection is vulnerable to spaces/etc between ops or possibly other issues. This should be fullproof but will provide a less useful error (and may in fact even cause a parse instead)
	 				else if binLit /= lit then do
	 					hPutStrLn stderr "Warning: your code's binary would actual extract to:"
	 					hPutStrLn stderr binLit
						hPutStrLn stderr "instead of:"
	 					hPutStrLn stderr lit
	 				else return ()
	 			else return ()
--  			hPutStrLn stderr lit
--  			hPutStrLn stderr $ flatHs hs
 			let header = unlines $ tail $ lines headerRaw -- remove "module Header"
 			writeFile "out.hs" $ header ++ "\nmain=interact ((\\input->finishLn$aToS$"++ flatHs (implCode impl) ++ ").sToA)"
 			if ops /= ["-norun"] then do
 				-- Compile with -O for full laziness rather than using runhaskell
 				(_, _, Just hsErr, p) <- createProcess (proc "ghc" ["-O", "out.hs"]){ std_out = CreatePipe, std_err = CreatePipe }
 				ex <- waitForProcess p
 				case ex of
 					ExitSuccess -> do
 						(_, Just hout, _, _) <- createProcess (proc "out" []){ std_out = CreatePipe }
 						hGetContents hout >>= putStr
 					ExitFailure _ -> do
 						hGetContents hsErr >>= hPutStr stderr
	 					error "failed to compile hs (likely an internal nibbles bug, please report it!)"
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
