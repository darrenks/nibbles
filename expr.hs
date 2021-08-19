module Expr where

import State
import qualified Data.DList as DList -- needs cabal install --lib dlist

import Types
import Hs
import SmartList

data ArgUsedness = UnusedArg | OptionalArg | UsedArg | UsednessDoesntMatter deriving (Show, Eq)

data Impl = Impl { implType::VT
                 , implCode::HsCode
                 , minUsedDepth::Int
                 , implName::Maybe String
                 , implUsed::ArgUsedness
                 } deriving (Eq, Show)
noArgsUsed = Impl undefined undefined 0 Nothing UnusedArg

data ArgKind = LambdaArg | LetArg { argKindDef::HsCode } deriving (Show, Eq)
data Arg = Arg { argImpls::[Impl], argKind::ArgKind } deriving Show

-- cp is the number of characters consumed so far
data Code = Lit { fullLit::String, codeLit::String, litcp::Int }
          | Nib { codeNib::[Int], nibcp::Int } deriving Show

uselessOp = 6 :: Int -- for padding odd nibbles into bytes

app1Hs :: String -> Impl -> Impl
app1Hs s impl = impl { implCode=hsApp (hsAtom s) (implCode impl) }

data ParseData = ParseData { pdCode :: Code
                           , pdContext :: [Arg]
                           , pdNib :: SmartList Int
                           , pdLit :: DList.DList Char }
type ParseState = State ParseData

-- all relevant information for determining arg match in a more abridge form than arg spec
data MatchTestData = MatchTestData { mtdTypes :: [VT]
                                   , mtdNibs :: [SmartList Int]
                                   , mtdState :: ParseData }

-- https://stackoverflow.com/questions/7787317/list-of-different-types
data ArgSpec
	= Cond String (MatchTestData -> Bool)
	| ParseArg (ParseState (VT, String))
	| Auto
	| AutoDefault ArgSpec Integer -- todo make any type
	| Fn ([VT] -> (Int, [VT]))

type Operation = ([ArgSpec], [VT]->ParseState ([VT], Impl))

dToList = DList.toList

appendRepH :: (SmartList Int,DList.DList Char) -> ParseState ()
appendRepH (nib2,lit2) = do
	nib <- gets pdNib
	lit <- gets pdLit
	modify $ \s -> s { pdNib = smartAppend nib nib2
	                 , pdLit = DList.append lit lit2 }
	                 
appendRep :: ([Int],String) -> ParseState ()
appendRep (nib2,lit2) = appendRepH (newSmartList nib2, DList.fromList lit2)

blankRep :: Code -> [Arg] -> ParseData
blankRep code context =
	ParseData code context (newSmartList []) (DList.fromList "")

getNib :: ParseData -> [Int]
getNib = smartToList . pdNib

getLit :: ParseData -> String
getLit = DList.toList . pdLit
