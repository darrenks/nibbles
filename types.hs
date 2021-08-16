module Types where

data VT = VInt | VChr | VList [VT] | VAuto | VFn [VT] [VT] -- | VMaybe VT | Nothing
	deriving (Show, Eq)

vstr = VList [VChr]

--                       prev args -> num ret,fn arg
data ArgSpec = Exact VT | Fn ([VT] -> (Int,[VT])) | Cond String ([(VT, [Int])] -> Bool)

data ArgMatchResult = ArgMatches | ArgFn ArgSpec

isNum VInt = True
isNum VChr = True
isNum VAuto = True
isNum _ = False
	
isList (VList _) = True
isList _ = False

ret (VFn from to) = to

elemT :: VT -> [VT]
elemT (VList e) = e
elemT s = error $ "is not a list: " ++ show s
