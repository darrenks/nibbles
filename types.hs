module Types where

import Text.Show.Functions

data VT = VInt | VChr | VList VT | VPair VT VT | VAuto | VMaybe (Maybe VT) | NoType
	deriving (Show, Eq)

vstr = VList VChr

data ArgSpec = Exact VT | Fn ([VT] -> VT) | Cond String ([VT] -> Bool) deriving Show

-- data ArgMatchResult = ArgMatches | ArgFnOf VT

instance Eq (a -> b) where
	a==b = False

-- nil = VMaybe Nothing

isNum VInt = True
isNum VChr = True
isNum VAuto = True
isNum _ = False

getBaseElem (VList e) = getBaseElem e
getBaseElem t = t
isVec = isNum . getBaseElem

isList (VList _) = True
isList _ = False
