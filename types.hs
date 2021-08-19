module Types where

data VT = VInt | VChr | VList [VT] | VFn [VT] [VT] | InvalidType -- | VMaybe VT | Nothing
	deriving (Show, Eq)

vstr = VList [VChr]

isNum VInt = True
isNum VChr = True
isNum _ = False
	
isList (VList _) = True
isList _ = False

ret (VFn from to) = to

elemT :: VT -> [VT]
elemT (VList e) = e
elemT s = error $ "is not a list: " ++ show s
