module Types where

import Data.List(intercalate)
import Data.Maybe

data VT = VInt | VChr | VList [VT] | VFn [VT] [VT] | AutoType | InvalidType
   | OptionYes | OptionNo | ItWasAConstant | StaticInt Integer -- only for code gen, not real types
   -- | VMaybe VT | Nothing -- (other ideas)
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

toTuple :: [String] -> String
toTuple [t] = t
toTuple s = "(" ++ intercalate "," s ++ ")"

toHsType :: VT -> Maybe String
toHsType VInt = Just "Integer"
toHsType VChr = toHsType VInt
toHsType (VList ts) =
   toHsTypes ts >>= \s -> Just $ "["++s++"]"
toHsType (VFn a b) = Nothing
toHsType (ItWasAConstant) = Nothing
toHsType (AutoType) = Nothing
toHsType (InvalidType) = Nothing
toHsType (StaticInt _) = Nothing
toHsType e = error $ "cant toHsType " ++ show e
toHsTypes ts =
   let elems = map toHsType ts in
   if all isJust elems then
      Just $ toTuple $ catMaybes elems
   else Nothing

-- won't work since sometimes its curried and others not?
--toHsType (VFn a b) = "((" ++ (intercalate "->" $ map toHsType a) ++ ")->"++(toTuple $ map toHsType b)++ ")"

-- todo remove and use toHsType when Strings use Char internally instead of Integer
toHsReadType :: VT -> String
toHsReadType VInt = "Integer"
toHsReadType VChr = "Char"
toHsReadType (VList [VChr]) = "String"
toHsReadType (VList ts) = "["++toHsReadTypes ts++"]"
toHsReadType (VFn from to) = toHsReadTypes from ++ " -> " ++ toHsReadTypes to
toHsReadType e = error $ "cant toHsReadType " ++ show e
toHsReadTypes ts = toTuple $ map toHsReadType ts
toParser t = "read::String->"++toHsReadTypes t

xorChr [VInt, VChr] = VChr
xorChr [VChr, VInt] = VChr
xorChr _ = VInt

orChr [_, VChr] = VChr
orChr [VChr, _] = VChr
orChr _ = VInt
