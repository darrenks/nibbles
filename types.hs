module Types where

import Text.Show.Functions

data VT = VInt | VChr | VList VT | VPair VT VT | VMultRet VT VT | VAuto | VFn [VT] VT -- | VMaybe VT | Nothing
	deriving (Show, Eq)

vstr = VList VChr

--                           prev args -> fn arg
data ArgSpec = Exact VT | Fn Int ([VT] -> VT) | Cond String ([VT] -> Bool) deriving (Eq, Show)
--                     num ret ^

--                                     num ret arg type
data ArgMatchResult = ArgMatches | ArgFnOf Int VT

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
