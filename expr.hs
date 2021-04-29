module Expr where

import Types

type HsCode = String
type OldNibLit = String
newtype NibLit = NibLit String deriving (Show, Eq)
type Nibble = Int
newtype Nibbles = Nibbles [Int] deriving Show
--todo put InputCode into Expr
data Expr = Expr VT [Nibble] OldNibLit HsCode deriving Show
data Thunk ic = Thunk ic [VT]

app1 op hs1 = "(" ++ op ++ " " ++ hs1 ++ ")"
-- vectorizeApp1 (V v) (VList t) op hs = vectorizeApp1 (V v) t (app1 "map" op) hs
-- vectorizeApp1 _ _ op hs = app1 op hs


retT (Expr t _ _ _) = t
setT t (Expr _ b l hs) = Expr t b l hs
appT f e = setT (f (retT e)) e
setTAndHs (Expr _ b l _) t hs = Expr t b l hs

i s = if s < 0 then "(" ++ show s ++ ")" else show s --"("++show s++"::Integer)"