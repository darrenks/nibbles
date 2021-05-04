module Expr where

import Types

type HsCode = String
type NibLit = String
type Nibble = Int
data Expr = Expr VT [Nibble] NibLit HsCode deriving Show

data Thunk = Thunk Code [VT]

-- The Int is the number of characters consumed so far
-- convention cp (code pointer) = this number
data Code = Lit NibLit Int | Nib [Nibble] Int deriving Show

uselessOp = 7 :: Int -- for padding odd nibbles into bytes

app1 :: String -> HsCode -> HsCode
app1 op hs1 = "(" ++ op ++ " " ++ hs1 ++ ")"

retT (Expr t _ _ _) = t
setT t (Expr _ b l hs) = Expr t b l hs
appT f e = setT (f (retT e)) e
setTAndHs (Expr _ b l _) t hs = Expr t b l hs

i :: Integer -> HsCode
i s = if s < 0 then "(" ++ show s ++ ")" else show s --"("++show s++"::Integer)"