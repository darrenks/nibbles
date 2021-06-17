module Types where

data VT = VInt | VChr | VList [VT] | VAuto | VFn [VT] [VT] -- | VMaybe VT | Nothing
	deriving (Show, Eq)

vstr = VList [VChr]

--                           prev args -> fn arg
data ArgSpec = Exact VT | Fn Int ([VT] -> [VT]) | Cond String ([VT] -> Bool)
--                     num ret ^

data ArgMatchResult = ArgMatches | ArgFn ArgSpec

isNum VInt = True
isNum VChr = True
isNum VAuto = True
isNum _ = False

todoAssumeFst ts = head ts
	
isList (VList _) = True
isList _ = False

ret (VFn from to) = to
