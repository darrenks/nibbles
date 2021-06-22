module Hs where

import Data.DList -- needs cabal install --lib dlist

single = singleton
flist = fromList
a +++ b = append a b

-- assume HsCode is parenthesized if precedence is less than apply (only need parens for rhs)
newtype HsCode = HsCode (DList Char) deriving (Show, Eq)
getHsCode (HsCode hs) = hs

hsAtom :: String -> HsCode
hsAtom = HsCode . fromList

hsApp :: HsCode -> HsCode -> HsCode
hsApp (HsCode a) (HsCode b) = hsParen $ HsCode $ a +++ single ' ' +++ b

hsFn :: [HsCode] -> HsCode -> HsCode
hsFn args (HsCode body) = HsCode $ flist "(\\" +++ argsLhs args +++ flist"->" +++ body +++ single ')' where
	argsLhs [] = flist"()"
	argsLhs hss = intercalate (single ' ') $ Prelude.map getHsCode hss

hsLet :: [HsCode] -> HsCode -> HsCode -> HsCode
hsLet vars (HsCode def) (HsCode body) = 
	HsCode $ flist"(let (" +++ lhs +++ flist")="
		+++ def +++ flist" in "
		+++ body +++ single ')'
			where lhs = intercalate (single ',') $ Prelude.map getHsCode vars

hsParen :: HsCode -> HsCode
hsParen (HsCode hs) = HsCode $ single '(' +++ hs +++ single ')'

flatHs :: HsCode -> String
flatHs (HsCode hs) = toList hs

i :: Integer -> HsCode
i = hsParen . hsAtom . show -- "::Integer)"
