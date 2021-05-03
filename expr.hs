module Expr where

import Types

type HsCode = String
type OldNibLit = String
type Nibble = Int
data Expr = Expr VT [Nibble] OldNibLit HsCode deriving Show
data Thunk = Thunk Code [VT]
data Code = Lit String Int | Nib [Int] Int deriving Show
uselessOp = 7 :: Int

app1 op hs1 = "(" ++ op ++ " " ++ hs1 ++ ")"

retT (Expr t _ _ _) = t
setT t (Expr _ b l hs) = Expr t b l hs
appT f e = setT (f (retT e)) e
setTAndHs (Expr _ b l _) t hs = Expr t b l hs

i s = if s < 0 then "(" ++ show s ++ ")" else show s --"("++show s++"::Integer)"