module Expr where

import Types
import Data.List (intercalate)

-- assume HsCode is parenthesized if precedence is less than apply (only need parens for rhs)
data HsCode = HsAtom String | HsApp HsCode HsCode | HsFn String HsCode deriving (Eq, Show)

type NibLit = String
type Nibble = Int
data Rep = Rep [Nibble] NibLit deriving Show
addRep (Rep b1 l1) (Rep b2 l2) = Rep (b1++b2) (l1++l2)

--                           min used depth
data Impl = Impl VT HsCode Int deriving (Eq, Show)
noArgsUsed = 0 :: Int
getImplType (Impl t _ _) = t
data Expr = Expr Rep Impl deriving Show
getExprType (Expr _ impl) = getImplType impl

data Thunk = Thunk Code [Arg]
getContext (Thunk _ context) = context

data VisibleFirst = Visible | Hidden deriving (Eq, Show)
--                                dep def
data ArgKind = LambdaArg | LetArg Int HsCode VisibleFirst deriving Eq
--                  depth
data Arg = Arg Impl Int ArgKind deriving Eq
-- getArgType (Arg impl _ _) | getType impl==VPair= impl
getArgData (Arg _ _ kind) = kind
getArgDepth (Arg _ depth _) = depth
getArgImpl (Arg impl _ _) = impl
isLet (Arg _ _ LambdaArg) = False
isLet (Arg _ _ (LetArg _ _ _)) = True

-- The Int is the number of characters consumed so far
-- convention cp (code pointer) = this number
data Code = Lit NibLit Int | Nib [Nibble] Int deriving Show

uselessOp = 6 :: Int -- for padding odd nibbles into bytes

app1 :: String -> HsCode -> HsCode
app1 = HsApp . HsAtom

app1Hs :: String -> Impl -> Impl
app1Hs s (Impl t hs d) = Impl t (app1 s hs) d

retT (Expr _ (Impl t _ _)) = t
setImpl (Expr r _) impl = Expr r impl
modifyImpl f (Expr r i) = Expr r (f i)

-- toArgList [arg] = arg
-- toArgList args = "(" ++ intercalate "," args ++ ")"

-- We didn't need to generate an AST first afterall, but could be useful if wanted
-- an O(n) code gen, could avoid the ++
flatHs :: HsCode -> String
flatHs (HsAtom s) = s
-- flatHs (HsApp (HsApp a (HsLet b)) c) =
-- 	"(let arg0 = " ++ flatHs b ++ " in " ++ flatHs (HsApp (HsApp a (HsAtom "arg0")) c) ++ ")"
-- flatHs (HsApp a (HsApp b c)) = flatHs a ++ " (" ++ flatHs (HsApp b c) ++ ")"
flatHs (HsApp a b) = "(" ++ flatHs a ++ " " ++ flatHs b ++ ")"
flatHs (HsFn arg body) = "(\\" ++ arg ++ "->" ++ flatHs body ++ ")"

i :: Integer -> HsCode
i s = HsAtom $ "(" ++ show s ++ ")" -- "::Integer)"
