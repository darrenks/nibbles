module Expr where

import Types
import Data.List (intercalate)

-- assume HsCode is parenthesized if precedence is less than apply (only need parens for rhs)
data HsCode = HsAtom String | HsLet HsCode | HsApp HsCode HsCode | HsFn [String] HsCode deriving Show
type NibLit = String
type Nibble = Int
data Expr = Expr VT [Nibble] NibLit HsCode deriving Show

data Thunk = Thunk Code [VT]

-- The Int is the number of characters consumed so far
-- convention cp (code pointer) = this number
data Code = Lit NibLit Int | Nib [Nibble] Int deriving Show

uselessOp = 7 :: Int -- for padding odd nibbles into bytes

app1 :: String -> HsCode -> HsCode
app1 = HsApp . HsAtom

retT (Expr t _ _ _) = t
setT t (Expr _ b l hs) = Expr t b l hs
appT f e = setT (f (retT e)) e
setTAndHs (Expr _ b l _) t hs = Expr t b l hs

toArgList [arg] = arg
toArgList args = "(" ++ intercalate "," args ++ ")"

flatHs :: HsCode -> String
flatHs (HsAtom s) = s
flatHs (HsApp (HsApp a (HsLet b)) c) =
	"(let arg0 = " ++ flatHs b ++ " in " ++ flatHs (HsApp (HsApp a (HsAtom "arg0")) c) ++ ")"
-- flatHs (HsApp a (HsApp b c)) = flatHs a ++ " (" ++ flatHs (HsApp b c) ++ ")"
flatHs (HsApp a b) = "(" ++ flatHs a ++ " " ++ flatHs b ++ ")"
flatHs (HsFn args body) = "(\\" ++ toArgList args ++ "->" ++ flatHs body ++ ")"
-- flatHs (HsLet a) = flatHs "arg0"

-- if body, if have let that has all vars, put in a let/in statement, replace HsLet with argn

i :: Integer -> HsCode
i s = if s < 0 then HsAtom $ "(" ++ show s ++ ")" else HsAtom $ show s --"("++show s++"::Integer)"