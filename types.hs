module Types where

import Text.Show.Functions

data VT = VInt | VChr | VList VT | VPair VT VT | VAuto | VMaybe (Maybe VT) | VVec VT | NoType
	deriving (Show, Eq)

vstr = VList VChr

data ArgSpec = Exact VT | Any | Fn ([VT] -> VT) | Cond String ([VT] -> VT -> Bool) | Coerce ArgSpec | PromoteList ArgSpec | Vec ArgSpec deriving Show

instance Eq (a -> b) where
	a==b = False

-- nil = VMaybe Nothing

isNum VInt = True
isNum VChr = True
isNum _ = False

isList (VList _) = True
isList _ = False
