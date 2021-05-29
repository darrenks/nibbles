module Types where

import Text.Show.Functions

data VT = VInt | VChr | VList VT | VPair VT VT | VAuto -- | VMaybe VT | Nothing
	deriving (Show, Eq)

vstr = VList VChr

--                       prev args -> fn arg
data ArgSpec = Exact VT | Fn ([VT] -> [VT]) | Cond String ([VT] -> Bool) deriving (Eq, Show)

data ArgMatchResult = ArgMatches | ArgFnOf [VT]

instance Eq (a -> b) where
	a==b = False

isNum VInt = True
isNum VChr = True
isNum VAuto = True
isNum _ = False

-- todo this is a tautology
isVec = isNum . baseElem

baseElem (VList e) = baseElem e
baseElem t = t
	
isList (VList _) = True
isList _ = False
