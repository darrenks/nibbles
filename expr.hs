module Expr where

import State
import qualified Data.DList as DList -- needs cabal install --lib dlist

import Types
import Hs

data Impl = Impl { implType::VT
                 , implCode::HsCode
                 , minUsedDepth::Int
                 } deriving (Eq, Show)
noArgsUsed = Impl { implType=undefined, implCode=undefined, minUsedDepth=0 }

data ArgKind = LambdaArg | LetArg { argKindDef::HsCode } deriving (Show,Eq)
data Arg = Arg { argImpls::[Impl], argKind::ArgKind } deriving (Show,Eq)

-- cp is the number of characters consumed so far
data Code = Lit { codeLit::String, litcp::Int }
          | Nib { codeNib::[Int], nibcp::Int } deriving Show

uselessOp = 6 :: Int -- for padding odd nibbles into bytes

app1Hs :: String -> Impl -> Impl
app1Hs s (Impl t hs d) = Impl t (hsApp (hsAtom s) hs) d

data ParseData = ParseData { pdCode :: Code
                           , pdContext :: [Arg]
                           , pdNib :: DList.DList Int
                           , pdLit :: DList.DList Char }
type ParseState = State ParseData

appendRepH :: (DList.DList Int,DList.DList Char) -> ParseState ()
appendRepH (nib2,lit2) = do
	nib <- gets pdNib
	lit <- gets pdLit
	modify $ \s -> s { pdNib = DList.append nib nib2
	                 , pdLit = DList.append lit lit2 }
	                 
appendRep :: ([Int],String) -> ParseState ()
appendRep (nib2,lit2) = appendRepH (DList.fromList nib2, DList.fromList lit2)

blankRep :: Code -> [Arg] -> ParseData
blankRep code context =
	ParseData code context (DList.fromList []) (DList.fromList "")

getNib :: ParseData -> [Int]
getNib = DList.toList . pdNib

getLit :: ParseData -> String
getLit = DList.toList . pdLit
