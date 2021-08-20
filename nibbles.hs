{-# LANGUAGE QuasiQuotes #-} -- for loading header.hs
{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

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
\Usage: nibbles [-c|-e|-v|-hs] [-simple] [filename]\n\
\\n\
\Nibbles - a minimalistic-functional code golf language.\n\
\\n\
\Modes:\n\
\  default = compile the nibbles program to out.hs and run it\n\
\  -c = compactify into bytes (2 nibbles each), save to base filename.nbb\n\
\  -e = expand and print the literate form\n\
\  -hs = only generate out.hs\n\
\  -v = version\n\
\\n\
\Other options:\n\
\  -simple = disable extensions and implicit things\n\
\\n\
\Filename and extension specifies source location and input format.\n\
\  .nbl file = literate form\n\
\  .nbb file = binary form\n\
\  empty = use stdin in literate form\n"

data ParseMode = FromLit | FromBytes deriving Eq

-- Set this as a constant rather than read it to create a stand-alone binary.
headerRaw :: String
headerRaw = [litFile|Header.hs|]

main=do
	setLocaleEncoding char8
	args <- getArgs
	let (contentsIO, basename, parseMode) = case filter (not.isOpt) args of
		[] -> (getContents, "a", FromLit)
		[f] -> (readFile f, base, case ext of
			".nbb" -> FromBytes
			".nbl" -> FromLit
			_ -> errorWithoutStackTrace "file extension must be .nbb or .nbl")
				where (base, ext) = splitExtension f
		e -> errorWithoutStackTrace $ "too many filename args:" ++ (show e) ++ "\n" ++ usage
	contents <- contentsIO
	let ?isSimple = isSimple args
	let compileFn = compile finish ""
	let (impl, bRaw, lit)  = compileFn $ case parseMode of
		FromLit -> Lit contents contents 0
		FromBytes -> Nib (concatMap fromByte contents) 0

	case filter isOpt args of
		ops | null $ filter (\opt -> not (isOtherOption opt) && opt /= "-hs") ops -> do
			if parseMode == FromLit then extraLitInfo compileFn lit bRaw else return ()
 			fullHs <- toFullHs impl
 			writeFile "out.hs" fullHs
 			if ops /= ["-hs"] then runHs "out.hs" else return ()
		["-c"] -> do
			let bytes = toBytes $ padToEvenNibbles bRaw
			let outname = (basename ++ ".nbb")
			hPutStrLn stderr $ "wrote " ++ (show $ length bytes) ++ " bytes to " ++ outname
			writeFile outname bytes
		["-e"] -> putStrLn lit
		["-v"] -> putStrLn "nibbles alpha (unstable)"
		e -> errorWithoutStackTrace $ "invalid option " ++ (show e) ++ "\n" ++ usage

isOpt = isPrefixOf "-"
toBytes = map toByte . reshape 2
isOtherOption = flip elem ["-simple"]
isSimple = elem "-simple"

extraLitInfo compileFn lit bRaw = do
	hPutStrLn stderr $ reverse $ reverse $ "size = " ++ (show $ length bRaw) ++ " nibbles"
	let (_,_,binLit) = compileFn (Nib (padToEvenNibbles bRaw) 0)
	if any (==16) bRaw then do
		hPutStrLn stderr "Warning: you are using literal only ops"
	-- This warning is necessary because the current accidental extension detection is vulnerable to spaces/etc between ops or possibly other issues. This should be fullproof but will provide a less useful error (and may in fact even cause a parse instead)
	else if binLit /= lit then do
		hPutStrLn stderr "Warning: your code's binary would actual extract to:"
		hPutStrLn stderr binLit
		hPutStrLn stderr "instead of:"
		hPutStrLn stderr lit
	else return ()

toFullHs impl = do
 	let header = unlines $ tail $ lines headerRaw -- remove "module Header"
 	return $ header ++ "\nmain=interact ((\\input->finishLn$aToS$"++ flatHs (implCode impl) ++ ").sToA)"

runHs filename = do
	-- Compile with -O for full laziness rather than using runhaskell
	(_, _, Just hsErr, p) <- createProcess (proc "ghc" ["-O", filename]){ std_out = CreatePipe, std_err = CreatePipe }
	ex <- waitForProcess p
	case ex of
		ExitSuccess -> do
			(_, Just hout, _, _) <- createProcess (proc "out" []){ std_out = CreatePipe }
			hGetContents hout >>= putStr
		ExitFailure _ -> do
			hGetContents hsErr >>= hPutStr stderr
			error "failed to compile hs (likely an internal nibbles bug, please report it!)"
