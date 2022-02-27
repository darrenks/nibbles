{-# LANGUAGE QuasiQuotes #-} -- for loading header.hs
{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

import Data.List(isPrefixOf,partition)
import System.Environment
import System.IO
import GHC.IO.Encoding
import System.FilePath
import System.Process
import System.Exit
import Control.Monad
import Data.List(intercalate)
import Data.List.Split (chunksOf,splitOn) -- needs cabal install --lib split

import FileQuoter
import Compile
import Header
import Polylib
import Expr
import Parse (toByte, fromByte)
import Hs(flatHs)
import ParseArgs

usage = "\
\Usage: nibbles [-c|-e|-v|-hs] [-simple] [filename] [args]*\n\
\\n\
\Nibbles - a functional code golf language for mortals.\n\
\\n\
\Documentation at "++webpage++"\n\
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
   setLocaleEncoding latin1 -- use raw bytes for .nbl, .nbb, and .hs files
   args <- getArgs
   let (progArgs, nonProgArgs) = partition isNibblesArg args
   let (opts,fileArgs) = partition isOpt nonProgArgs
   (contents, basename, parseMode) <- case fileArgs of
         [] -> do
            when (not$null progArgs) (hPutStrLn stderr "Warning: args are being treated as input args and nibbles code is being read from stdin")
            contents <- getContents
            return (contents, "a", FromLit)
         [f] -> case ext of
            ".nbb" -> do
               contents <- readFile f
               return (contents, base, FromBytes)
            ".nbl" -> do
               contents <- readFile f
               return (contents, base, FromLit)
            _ -> errorWithoutStackTrace "file extension must be .nbb or .nbl"
            where (base, ext) = splitExtension f
         e -> errorWithoutStackTrace $ "too many filename args:" ++ (show e) ++ "\n" ++ usage
   let ?isSimple = isSimple args
   let (cargs, reader) = toLetArgs progArgs
   let compileFn = compile finish "" cargs
   let (impl, bRaw, lit, litWarnings)  = compileFn $ case parseMode of
         FromLit -> Lit contents contents 0
         FromBytes -> Nib (concatMap fromByte contents) 0
   let paddedNibs = padToEvenNibbles bRaw
   let nibBytes = toBytes paddedNibs
   let (_,_,binLit,_) = compileFn (Nib (paddedNibs) 0)

   case opts of
      -- if no c/e/v option specified then assume it means run
      ops | null $ filter (\opt -> not (isOtherOption opt) && opt /= "-hs") ops -> do
         errored <- litErrorHandle "Warning" bRaw litWarnings
         maybeNibBytes <- if errored then return []
         else do
            when (parseMode == FromLit) $ do
               checkWouldExtractCorrectly binLit lit
               hPutStrLn stderr $ reverse $ reverse {-make strict to print at same time-} $
                  "size = " ++ (show $ length bRaw) ++ " nibbles ("++ (show $ div (length bRaw + 1) 2) ++ " bytes)"
            return nibBytes
         fullHs <- toFullHs impl maybeNibBytes reader
         writeFile "out.hs" fullHs
         when (ops /= ["-hs"]) $ runHs "out.hs" progArgs
      ["-c"] -> do
         checkWouldExtractCorrectly binLit lit
         errored <- litErrorHandle "Error" bRaw litWarnings
         when errored $ errorWithoutStackTrace "aborting"
         let outname = (basename ++ ".nbb")
         hPutStrLn stderr $ "wrote " ++ (show $ length nibBytes) ++ " bytes to " ++ outname
         writeFile outname nibBytes
      ["-e"] -> putStrLn lit
      ["-v"] -> putStrLn version
      e -> errorWithoutStackTrace $ "invalid option " ++ (show e) ++ "\n" ++ usage

estimateBinLength [] = 0
-- next op causes an error to avoid accidentally trying to convert lit only ops to bin
estimateBinLength (16:_:s) = 2 + estimateBinLength s
estimateBinLength (_:s) = 1 + estimateBinLength s

litErrorHandle msgPrefix bRaw litWarnings = do
   let binOnlyOps = any (==16) bRaw
   if binOnlyOps && null litWarnings then do
      hPutStrLn stderr $ msgPrefix ++ ": you are using literal only ops, estimated size: " ++ show (estimateBinLength bRaw) ++ " nibbles"
      return True
   else if not (null litWarnings) then do
      mapM_ (\msg -> hPutStrLn stderr $ msgPrefix++": " ++msg) litWarnings
      return True
   else return False

checkWouldExtractCorrectly binLit lit = do
   -- This warning is necessary because the current accidental extension detection is vulnerable to spaces/etc between ops or possibly other issues. This should be fullproof but will provide a less useful error (and may in fact even cause a parse error instead)
   when (binLit /= lit) $ do
      hPutStrLn stderr "Warning: your code's binary would actual extract to:"
      hPutStrLn stderr binLit
      hPutStrLn stderr "instead of:"
      hPutStrLn stderr lit
      hPutStrLn stderr "Please report this as it is a bug (there is supposed to be a more clear accidental extension detection, this is just a fail safe)"


isOpt arg = isPrefixOf "-" arg
toBytes = map toByte . chunksOf 2
isOtherOption = flip elem ["-simple"]
isSimple = elem "-simple"

toFullHs impl nibBytes reader = do
   let header = unlines $ tail $ lines headerRaw -- remove "module Header"
   return $ header ++ "\n\
   \progSource = "++show nibBytes++"\n\
   \main=do\n\
   \ hSetEncoding stdin char8\n\
   \ hSetEncoding stdout char8\n\
   \ args <- getArgs \n\
   \ "++reader++"\n\
   \ interact ((\\input->let output=" ++ flatHs (implCode impl) ++ "\n\
   \  -- don't print a newline to a quine! \n\
   \  in aToS $ if output == sToA progSource\n\
   \    then output else finishLn output).sToA)"

runHs :: String -> [String] -> IO ()
runHs filename args = do
   setLocaleEncoding initLocaleEncoding
   -- Compile with -O for full laziness rather than using runhaskell
   (_, _, _, p) <- createProcess (proc "ghc" ["-O", filename]){ std_out = CreatePipe }
   ex <- waitForProcess p
   case ex of
      ExitSuccess -> callProcess "out" args
      ExitFailure _ -> error "failed to compile hs (likely an internal nibbles bug, please report it!)"
