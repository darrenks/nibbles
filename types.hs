module Types where

import Text.Show.Functions

-- data ArgType = Specific VT -- todo or second line of VT

data VT = VInt Bool | VList VT | VPair VT VT | VAuto | VMaybe (Maybe VT)
	| Any | Fn ([VT] -> VT) | Cond ([VT] -> VT -> Bool) | Coerce VT | NoType | PromoteList VT | Vec VT
	deriving (Show, Eq)

instance Eq (a -> b) where
	a==b = False

-- nil = VMaybe Nothing

int = VInt False
char = VInt True
str = VList char

isNum (VInt _) = True
isNum _ = False

isList (VList _) = True
isList _ = False

num = Cond $ const isNum
list = Cond $ const isList
anyT = Cond $ const $ const True
listOf = VList

elemT (VList e) = e