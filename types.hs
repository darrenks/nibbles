module Types where

-- todo could create a simple/complex type, forcing maybe/pair conversion before use
-- todo could get rid of VPair and use [VT] everywhere a VT is used
--                                                     recursive fn place holder
data VT = VInt | VChr | VList VT | VPair VT VT | VTuple0 | VRec | VMultRet VT VT | VAuto | VFn [VT] VT -- | VMaybe VT | Nothing
	deriving (Show, Eq)

vstr = VList VChr

--                           prev args -> fn arg
data ArgSpec = Exact VT | Fn Int ([VT] -> VT) | Cond String ([VT] -> Bool)
--                     num ret ^

--                                     num ret arg type
data ArgMatchResult = ArgMatches | ArgFnOf Int VT deriving Show

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
