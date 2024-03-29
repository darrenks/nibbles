module Expr where

import State
import qualified Data.DList as DList -- needs cabal install --lib dlist

import Types
import Hs
import SmartList
import qualified Data.Set as Set


-- if change, change downloads.md
version = "nibbles 1.01+ (unnumbered release)"
webpage = "http://golfscript.com/nibbles"

data ArgUsedness = UnusedArg | OptionalArg | UsedArg | UsednessDoesntMatter deriving (Show, Eq)
data OptionalLets = OptionalLets [VT] -- used to denote that the extra rets should be marked optional

data Impl = Impl { implType::VT
                 , implCode::HsCode
                 , implDeps::Set.Set Int
                 , implName::Maybe String -- to go into arg
                 , implUsed::ArgUsedness -- to go into arg
                 } deriving (Eq, Show)
noArgsUsed = Impl (error "undefined impl type") (hsAtom "(error \"undefined impl code\")") (Set.singleton 0) Nothing UnusedArg

data ArgKind = LambdaArg | LetArg { argKindDef::HsCode } deriving (Show, Eq)
data Args = Args { argsImpls::[Impl], argsKind::ArgKind, argsFrom::String } deriving Show

-- cp is the number of characters consumed so far
data Code = Lit { fullLit::String, codeLit::String, litcp::Int }
          | Nib { codeNib::[Int], nibcp::Int } deriving Show
isBinary (Nib _ _) = True
isBinary _ = False

uselessOp = 6 :: Int -- for padding odd nibbles into bytes

app1Hs :: String -> Impl -> Impl
app1Hs s impl = impl { implCode=hsApp (hsAtom s) (implCode impl) }

data ParseData = ParseData { pdCode :: Code
                           , pdContext :: [Args]
                           , pdNib :: SmartList Int
                           , pdLit :: DList.DList Char
                           , pdDataUsed :: Bool -- (used directly, not through $)
                           , pdImplicitArgUsed :: Bool -- to disable commutative extensions
                           , pdLitWarnings :: [String]
                           , pdNextUniqLetId :: Int}
type ParseState = State ParseData

-- all relevant information for determining arg match in a more abridge form than arg spec
data MatchTestData = MatchTestData { mtdTypes :: [VT]
                                   , mtdNibs :: [SmartList Int]
                                   , mtdState :: ParseData }

data ReqArg = ReqDontCare | ReqArg | ReqConst deriving Eq

-- https://stackoverflow.com/questions/7787317/list-of-different-types
data ArgSpec
   = Cond String {-desc-} (MatchTestData -> Bool)
   | ParseArg String {-desc-} (ParseState (VT, String))
   | FakeAuto String -- an auto for quickref purposes only, it is implemented by a hidden op
   | Auto
   | AutoData ArgSpec
   | AutoDefault ArgSpec Integer -- todo make any type
   | Fn ReqArg ArgUsedness ([VT] -> (Int, [VT]))
   | OrAuto String {- desc -} ArgSpec
   | AutoNot ArgSpec {- only Fn -}
   | AutoOption String {- desc -}
   | OptionalFn ([VT] -> (Int, [VT])) -- returns a fn if its args are used, otherwise an impl with type ItWasAConstant
   | ZipMode
   | FoldMode
   | CharClassMode
   | AnyS -- basically a const fn, but shown as any*
   | BinCodeRep (Char, Int) -- only used in spec, converted to BinCode/LitCode
   | BinCode Int
   | LitCode Char
   | NotEOF
   | EOF

data OpBehavior = LitWarn String | CodeGen ([VT]->ParseState ([VT], Impl))
type Operation = ([ArgSpec], OpBehavior)

dToList = DList.toList

addLitWarning :: String -> ParseState ()
addLitWarning msg = do
   modify $ \s -> s { pdLitWarnings = msg : pdLitWarnings s }
   return ()

appendRepH :: (SmartList Int,DList.DList Char) -> ParseState ()
appendRepH (nib2,lit2) = do
   nib <- gets pdNib
   lit <- gets pdLit
   modify $ \s -> s { pdNib = smartAppend nib nib2
                    , pdLit = DList.append lit lit2 }

appendRep :: ([Int],String) -> ParseState ()
appendRep (nib2,lit2) = appendRepH (newSmartList nib2, DList.fromList lit2)

appendRepA :: ([Int],[String]) -> ParseState ()
appendRepA (nib,lit) = appendRep (nib,concat lit)

blankRep :: Code -> [Args] -> Int -> ParseData
blankRep code context nextUniqLetId =
   ParseData code context (newSmartList []) (DList.fromList "") False False [] nextUniqLetId

getNib :: ParseData -> [Int]
getNib = smartToList . pdNib

getLit :: ParseData -> String
getLit = DList.toList . pdLit
