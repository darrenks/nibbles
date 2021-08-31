{-# LANGUAGE QuasiQuotes #-} -- for loading header.hs
{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

import Data.List(isPrefixOf)
import System.Environment
import System.IO
import GHC.IO.Encoding
import System.FilePath
import System.Process
import System.Exit
import Control.Monad
import Data.List.Split (chunksOf) -- needs cabal install --lib split

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
	defaultEncoding <- getLocaleEncoding
	setLocaleEncoding char8 -- (only for file reading/writing)
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
	let (impl, bRaw, lit, litWarnings)  = compileFn $ case parseMode of
		FromLit -> Lit contents contents 0
		FromBytes -> Nib (concatMap fromByte contents) 0
	let binOnlyOps = any (==16) bRaw
	let paddedNibs = padToEvenNibbles bRaw
	let nibBytes = toBytes paddedNibs
	
	case filter isOpt args of
		-- if no c/e/v option specified then assume it means run
		ops | null $ filter (\opt -> not (isOtherOption opt) && opt /= "-hs") ops -> do
			errored <- litErrorHandle "Warning" binOnlyOps litWarnings
			maybeNibBytes <- if errored then return []
			else do
				when (parseMode == FromLit) $ do
					let (_,_,binLit,_) = compileFn (Nib (paddedNibs) 0)
					-- This warning is necessary because the current accidental extension detection is vulnerable to spaces/etc between ops or possibly other issues. This should be fullproof but will provide a less useful error (and may in fact even cause a parse instead)
					when (binLit /= lit) $ do
						hPutStrLn stderr "Warning: your code's binary would actual extract to:"
						hPutStrLn stderr binLit
						hPutStrLn stderr "instead of:"
						hPutStrLn stderr lit
					hPutStrLn stderr $ reverse $ reverse $ "size = " ++ (show $ length bRaw) ++ " nibbles"
				return nibBytes
 			fullHs <- toFullHs impl maybeNibBytes
 			writeFile "out.hs" fullHs
 			setLocaleEncoding defaultEncoding
 			when (ops /= ["-hs"]) $ runHs "out.hs"
		["-c"] -> do
			errored <- litErrorHandle "Error" binOnlyOps litWarnings
			when errored $ errorWithoutStackTrace "aborting"
			let outname = (basename ++ ".nbb")
			hPutStrLn stderr $ "wrote " ++ (show $ length nibBytes) ++ " bytes to " ++ outname
			writeFile outname nibBytes
		["-e"] -> putStrLn lit
		["-v"] -> putStrLn "nibbles alpha (unstable)"
		e -> errorWithoutStackTrace $ "invalid option " ++ (show e) ++ "\n" ++ usage
	
litErrorHandle msgPrefix binOnlyOps litWarnings = do
	if binOnlyOps && null litWarnings then do
		hPutStrLn stderr $ msgPrefix ++ ": you are using literal only ops"
		return True
	else if not (null litWarnings) then do
		mapM_ (\msg -> hPutStrLn stderr $ msgPrefix++": " ++msg) litWarnings
		return True
	else return False

isOpt = isPrefixOf "-"
toBytes = map toByte . chunksOf 2
isOtherOption = flip elem ["-simple"]
isSimple = elem "-simple"

toFullHs impl nibBytes = do
 	let header = unlines $ tail $ lines headerRaw -- remove "module Header"
 	return $ header ++ "\n\
	\progSource = "++show nibBytes++"\n\
 	\main=interact ((\\input->let output=aToS$"++ flatHs (implCode impl) ++ "\n\
 	\ -- don't print a newline to a quine! \n\
 	\ in if output == progSource\n\
 	\    then output else finishLn output).sToA)"

runHs filename = do
	-- Compile with -O for full laziness rather than using runhaskell
	(_, _, _, p) <- createProcess (proc "ghc" ["-O", "-Wno-tabs", filename]){ std_out = CreatePipe }
	ex <- waitForProcess p
	case ex of
		ExitSuccess -> do
			(_, Just hout, _, _) <- createProcess (proc "out" []){ std_out = CreatePipe }
			hGetContents hout >>= putStr
		ExitFailure _ -> do
			error "failed to compile hs (likely an internal nibbles bug, please report it!)"
