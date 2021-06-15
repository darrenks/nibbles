module Expr where

import Types

-- assume HsCode is parenthesized if precedence is less than apply (only need parens for rhs)
data HsCode = HsAtom String | HsApp HsCode HsCode | HsFn String HsCode deriving (Eq, Show)

type NibLit = String
type Nibble = Int
data Rep = Rep [Nibble] NibLit deriving Show
addRep (Rep b1 l1) (Rep b2 l2) = Rep (b1++b2) (l1++l2)

data SImpl = SImpl VT HsCode Int deriving (Eq, Show)
--                           min used depth
data Impl = Impl [VT] HsCode Int deriving (Eq, Show)
noArgsUsed = 0 :: Int
getImplType (Impl t _ _) = t
getImplType2 (SImpl t _ _) = t
getHs (SImpl _ hs _) = hs
data SExpr = SExpr Rep SImpl deriving Show
data Expr = Expr Rep Impl deriving Show
getRep (Expr rep _) = rep
getExprImpl (Expr _ impl) = impl
getExprType (SExpr _ impl) = getImplType2 impl

sToImpl (SImpl t hs dep) = Impl [t] hs dep
sToExpr (SExpr r i) = Expr r (sToImpl i)

data Thunk = Thunk Code [Arg]
getContext (Thunk _ context) = context

--                                def
data ArgKind = LambdaArg | LetArg HsCode deriving Eq
data Arg = Arg [SImpl] ArgKind deriving Eq
-- getArgType (Arg impl _ _) | getType impl==VPair= impl
-- getArgData (Arg _ kind _ ) = kind
-- getArgDepth (Arg _ depth _) = depth
getArgImpls (Arg impls _) = impls
isLet (Arg _ LambdaArg) = False
isLet (Arg _ (LetArg _)) = True

-- The Int is the number of characters consumed so far
-- convention cp (code pointer) = this number
data Code = Lit NibLit Int | Nib [Nibble] Int deriving Show

uselessOp = 6 :: Int -- for padding odd nibbles into bytes

app1 :: String -> HsCode -> HsCode
app1 = HsApp . HsAtom

app1Hs :: String -> SImpl -> SImpl
app1Hs s (SImpl t hs d) = SImpl t (app1 s hs) d

retT (Expr _ (Impl t _ _)) = t
retT2 (SExpr _ (SImpl t _ _)) = t
setImpl (Expr r _) impl = Expr r impl
modifyImpl f (SExpr r i) = SExpr r (f i)

setType newT (SImpl t hs d) = SImpl newT hs d

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
