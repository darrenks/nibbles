module Expr where

import Types
import Hs

type NibLit = String
type Nibble = Int
data Rep = Rep [Nibble] NibLit deriving Show
addRep (Rep b1 l1) (Rep b2 l2) = Rep (b1++b2) (l1++l2)

data Impl = Impl VT HsCode Int {-min used depth-} deriving (Eq, Show)
noArgsUsed = 0 :: Int
getImplType (Impl t _ _) = t
getHs (Impl _ hs _) = hs
data Expr = Expr Rep Impl deriving Show
getExprType (Expr _ impl) = getImplType impl

data Thunk = Thunk Code [Arg]
getContext (Thunk _ context) = context
getCode (Thunk code _) = code

data ArgKind = LambdaArg | LetArg HsCode {-def-} deriving (Show,Eq)
data Arg = Arg [Impl] ArgKind deriving (Show,Eq)
getArgImpls (Arg impls _) = impls
isLet (Arg _ LambdaArg) = False
isLet (Arg _ (LetArg _)) = True

-- The Int is the number of characters consumed so far
-- convention cp (code pointer) = this number
data Code = Lit NibLit Int | Nib [Nibble] Int deriving Show

uselessOp = 6 :: Int -- for padding odd nibbles into bytes

app1Hs :: String -> Impl -> Impl
app1Hs s (Impl t hs d) = Impl t (hsApp (hsAtom s) hs) d

retT (Expr _ (Impl t _ _)) = t
modifyImpl f (Expr r i) = Expr r (f i)

setType newT (Impl t hs d) = Impl newT hs d
