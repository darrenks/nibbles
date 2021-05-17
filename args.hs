module Args(getArg, getArgN, addLambda) where

import Expr
import Types
import Numeric (showHex)
import Parse (nextHex)

argStr :: Int -> String
argStr n = "arg" ++ show n
argLen = sum . (map argsize)

--todo what about pairs of pairs
argn :: [VT] -> Int -> (VT, HsCode)
argn [] _ = error $ "negative arg index"
argn (VPair a b:r) 0 = (b, HsAtom $ argStr $ argLen r+1)
argn (VPair a b:r) 1 = (a, HsAtom $ argStr $ argLen r)
argn (a:r) 0 = (a, HsAtom $ argStr $ argLen r)
argn (a:r) n = argn r $ n - argsize a

argsize (VPair a b) = argsize a + argsize b
argsize _ = 1

getDepth = sum . (map argsize)

-- todo pair of pair (argsize instead of +1)
argLhs :: Int -> VT -> [String]
argLhs depth (VPair a b) = argLhs depth a ++ argLhs (depth+1) b
argLhs depth _ = [argStr depth]

addLambda :: [VT] -> VT -> Expr -> Expr
addLambda context argT (Expr t b l hs) =
	Expr t b l (HsFn (argLhs (getDepth context) argT) hs)

getArg n e (Thunk code vt) = (Thunk code vt, setTAndHs e argT argHs) where (argT, argHs) = argn vt n

getArgN :: Expr -> Thunk -> (Thunk, Expr)
getArgN (Expr _ nib lit _) (Thunk code vt) = (Thunk afterSlashCode vt, Expr argT (nib++[v]) (lit++showHex v "") argHs) 
	where 
		(argT, argHs) = argn vt v
		(v, afterSlashCode) = nextHex code
