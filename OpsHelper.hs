{-# LANGUAGE FlexibleInstances #-} -- for String instances

module OpsHelper where

import Expr
import Types
import Polylib
import Parse
import Hs

makeOp :: (ToLitSpec lit) => Bool -> (lit, [Int], [ArgSpec], OpBehavior) -> [(Bool, [String], [Int], Operation)]
makeOp priority (lit, nib, t, behavior) = [(priority, toLitSpec lit, nib, (t, behavior))]

op :: (OpImpl impl, ToLitSpec lit) => (lit, [Int], [ArgSpec], impl) -> [(Bool, [String], [Int], Operation)]
op (lit,nib,args,impl) = makeOp True (lit,nib,args,CodeGen $ toImpl impl)
lowPriorityOp :: (OpImpl impl, ToLitSpec lit) => (lit, [Int], [ArgSpec], impl) -> [(Bool, [String], [Int], Operation)]
lowPriorityOp (lit,nib,args,impl) = makeOp False (lit,nib,args,CodeGen $ toImpl impl)

genericReason = "This usually means there is an alternative (likely shorter) way to do what you are trying to."
associativeReason = "Use the other operation order for this associative op to accomplish this. E.g. a+(b+c) instead of (a+b)+c."
commutativeReason = "Use the other operator order for this commutative op to accomplish this. E.g. (b+a) instead of (a+b)."

litExtError invalidLit lit reason = LitWarn $ "You used an op combo that has been remapped to an extension in the binary form.\nYou wrote:\n" ++ formatInvalidLit invalidLit ++ "\nBut this actually will mean:\n" ++ lit ++ "\n" ++ reason ++ " For more infromation see https://nibbles.golf/tutorial_ancillary.html#extensions or if you are learning try \"nibbles -simple\" to disable all extensions." where
	formatInvalidLit = concatMap $ \l -> if l==litDigit then "[digit]" else l

makeExtendOp :: (OpImpl impl) => Bool -> [String] -> String -> (String, [Int], [ArgSpec], impl) -> [(Bool, [String], [Int], Operation)]
makeExtendOp priority invalidLit reason (lit, nib, t, impl) =
	makeOp priority (invalidLit, [], t, litExtError invalidLit lit reason) 
		++ op (lit, nib, t, impl)
extendOp :: (OpImpl impl) => [String] -> String -> (String, [Int], [ArgSpec], impl) -> [(Bool, [String], [Int], Operation)]
extendOp = makeExtendOp True
lowPriorityExtendOp :: (OpImpl impl) => [String] -> String -> (String, [Int], [ArgSpec], impl) -> [(Bool, [String], [Int], Operation)]
lowPriorityExtendOp = makeExtendOp False

undefinedImpl = (VInt,"asdf")

toUntypedImpl hs = noArgsUsed { implCode=hsParen $ hsAtom hs }
toUntypedImpl2 (ts,hs) = (ts,toUntypedImpl hs)

class OpImpl impl where
	toImpl :: impl -> [VT] -> ParseState ([VT], Impl) -- also include [VT] since some ops can produce multiple values
instance OpImpl ([VT] -> ParseState ([VT], Impl)) where toImpl = id
instance OpImpl ([VT] -> VT, [VT] -> String) where
	toImpl (f1,f2) context = return ([f1 context], toUntypedImpl $ f2 context)
instance OpImpl ([VT] -> VT, String) where
	toImpl (f1,s) context = return ([f1 context], toUntypedImpl s)
instance OpImpl (VT, [VT] -> String) where
	toImpl (t,f2) context = return ([t], toUntypedImpl $ f2 context)
instance OpImpl (VT, String) where
	toImpl (t,s) context = return ([t], toUntypedImpl s)
instance OpImpl ([VT] -> (VT, String)) where
	toImpl f context = return ([t],toUntypedImpl s) where (t, s) = f context

instance OpImpl ([VT] -> [VT], [VT] -> String) where
	toImpl (f1,f2) context = return (f1 context, toUntypedImpl $ f2 context)
instance OpImpl ([VT] -> [VT], String) where
	toImpl (f1,s) context = return (f1 context, toUntypedImpl s)
instance OpImpl ([VT], [VT] -> String) where
	toImpl (t,f2) context = return (t, toUntypedImpl $ f2 context)
instance OpImpl ([VT], String) where
	toImpl (t,s) context = return (t, toUntypedImpl s)
-- todo there could be other instances where we want to support optionallets...
instance OpImpl ([VT] -> (OptionalLets, String)) where
	toImpl f context = do
		let (OptionalLets t,impl)=toUntypedImpl2 $ f context
		return (t,impl { implUsed = OptionalArg } )
instance OpImpl ([VT] -> ([VT], String)) where
	toImpl f context = return $ toUntypedImpl2 $ f context

instance OpImpl ([VT] -> ParseState (VT,String)) where
	toImpl implMonad ts = do
		(t,hs) <- implMonad ts
		return ([t], toUntypedImpl hs)

instance OpImpl () where toImpl _ ts = return $ (ts, toUntypedImpl "id")

instance OpImpl (ParseState Impl) where
	toImpl implMonad ts = do
		impl <- implMonad
		return ([implType impl], impl)

class ToLitSpec lit where toLitSpec :: lit -> [String]
instance ToLitSpec [String] where toLitSpec s = s
instance ToLitSpec String where toLitSpec s = [s]

-- class ToImpl impl where toImpl2 :: impl -> [VT] -> ParseState ([VT], impl)

infixr 1 ~>
a~>b = (b,a)

-- 16 makes it so that parsing bin will never try it
convertNullNib (isPriority, lit, nib, op) = (isPriority, lit, if null nib
		then [16, error $ "attempt to convert "++(concat lit)++" to bin (it is only for literate mode)"]
		else nib
	, op)

a1 = head :: [VT] -> VT
a2 = (!!1) :: [VT] -> VT

vList1 x = VList [x]

dup a = [a,a]

byType :: (VT -> Bool) -> MatchTestData -> Bool
byType ft = ft . last . mtdTypes

exactType :: VT -> MatchTestData -> Bool
exactType t = byType (==t)
int = Cond "int" $ exactType VInt
char = Cond "chr" $ exactType VChr
str = Cond "str" $ exactType vstr

fn f = fnx $ \ts->(1, f ts)
fn2 f = fnx $ \ts->(2, f ts)
fnx = Fn False UnusedArg

num = Cond "num" $ byType isNum
vec = Cond "vec" $ byType $ const True
list = Cond "[*]" $ byType isList
listToBeReferenced = Cond "[a]" $ byType isList
any1 = Cond "any" $ byType $ const True
auto = Auto False
binOnlyAuto = Auto True

listOf (Cond desc t) = Cond ("["++desc++"]") $ \mtd -> let lastArg = last $ mtdTypes mtd in
	isList lastArg && t (mtd { mtdTypes=reverse $ elemT $ lastArg })

-- -- todo consider arg matching in opcode 15
elemOfA1 = Cond "a" $ \mtd -> let [a1,a2] = mtdTypes mtd in
	isList a1 && head (elemT a1) == a2
sameAsA1 = Cond "[a]" $ \mtd -> let [a1,a2] = mtdTypes mtd in
	a1 == a2

sndArgLarger = Cond ">" $ \mtd -> let [op1b,op2b] = mtdNibs mtd
	in op2b > op1b

andC (Cond as af) (Cond bs bf) = Cond (bs++as) (\mtd -> af mtd && bf mtd)
orC (Cond as af) (Cond bs bf) = Cond (as++"|"++bs) (\mtd -> af mtd || bf mtd)

testCoerce2 :: [VT] -> String
testCoerce2 [a1,a2] = "const $ const $ sToA $ " ++ show (if ct1 == ct2
	then show ct1
	else "flipped mismatch: " ++ show ct1 ++ "," ++ show ct2)
	where
		ct1 = coerce2[a1][a2]
		ct2 = coerce2[a2][a1]

testCoerceTo :: [VT] -> [VT] -> ([VT], String)
testCoerceTo to a1 =  (to, coerceTo to a1)

isExtension (lit, nib, op) = length nib > 1 && length lit > 1 || isExtOpt op || elem '~' lit 
isExtOpt (types,_) = any (\t -> case t of
	Auto _ -> True
	Cond desc _ -> elem '>' desc
	otherwise -> False) types
isOpSimple (isPriority, lits, nib, op@(types,_)) =
	(not (isExtension (lit, nib, op) || null nib)
	||elem lit whitelist)
		&& not (elem lit blacklist)
		&& not (any isSpecialMode types)
	where
		lit = concat lits
		whitelist = ["ct","p"]
		blacklist = ["tbd"]
		isSpecialMode (CharClassMode) = True
		isSpecialMode _ = False