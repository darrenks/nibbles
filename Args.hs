{-# LANGUAGE ImplicitParams #-} -- for tracking isSimple option

module Args(argn, getArgByName, newLambdaArg, addLambda, newLetArg, popArg, debugContext, argImplicit, deBruijnArgReps, getAllArgs) where

import Expr
import Types
import Header
import Hs
import Parse (parseError,onlyCheckMatchIdentifier)

import Data.List
import Data.Maybe
import State
import qualified Data.Set as Set

argStr n tn = "arg" ++ show n ++ "t" ++ show tn
letStr n tn = "larg" ++ show n ++ "t" ++ show tn

newLambdaArg :: [VT] -> Maybe [String] -> ArgUsedness -> String -> ParseState Args
newLambdaArg argT names argUsedness from = do
   context <- gets pdContext
   let depth = 1 + length context
   let impls = zipWith3 (\t name tn -> Impl {
      implType = t,
      implCode = hsAtom $ argStr depth tn,
      implDeps = Set.singleton depth,
      implName = name,
      implUsed = argUsedness
      } ) argT (transposeMaybe names) [1..]
   let newArg = Args impls LambdaArg from
   modify $ \s -> s { pdContext=newArg:context }
   return newArg

transposeMaybe :: Maybe [String] -> [Maybe String]
transposeMaybe names = maybe (repeat Nothing) (map Just) names

newLetArg :: Maybe [String] -> ArgUsedness -> Impl -> [VT] -> String -> ParseState Args
newLetArg names argUsedness (Impl _ defHs defDepth _ usedness) defTypes from = do
   context <- gets pdContext
   nextLetId <- gets pdNextUniqLetId
   modify $ \s -> s { pdNextUniqLetId=nextLetId+1 }
   let impls = zipWith3 (\t name tn -> Impl t (hsAtom $ letStr nextLetId tn) defDepth name argUsedness) defTypes (transposeMaybe names) [1..]
   let newArg = Args impls (LetArg defHs) from
   modify $ \s -> s { pdContext=newArg:context }
   return newArg


argn :: Int -> ParseState Impl
argn deBruijnIndex = do
   context <- gets pdContext
   flattenedArgs <- getAllArgs
   case at flattenedArgs (deBruijnIndex-1) of
      Nothing -> parseError $ "Attempt to access " ++ (snd $ indexToOp (deBruijnIndex-1)) ++ debugContext context
      Just impl -> setUsed impl

getArgByName :: ParseState (Maybe Impl)
getArgByName = do
   allArgs <- getAllArgs
   getArgByNameH 0 allArgs
   where
      getArgByNameH :: Int -> [Impl] -> ParseState (Maybe Impl)
      getArgByNameH _ [] = return Nothing
      getArgByNameH n (impl:rest) =
         case implName impl of
            Just argName -> do
               code <- gets pdCode
               case onlyCheckMatchIdentifier code of
                  Just (name, nextCode) | name == argName -> do
                     modify $ \s -> s { pdCode = nextCode }
                     appendRep $ indexToOp n
                     argn (n+1) >>= return.Just
                  otherwise -> getArgByNameH (n+1) rest
            otherwise -> getArgByNameH (n+1) rest

setUsed :: Impl -> ParseState Impl
setUsed impl = do
   context <- gets pdContext
   let newContext = map (\arg -> arg { argsImpls=setUsedImpl (argsImpls arg) } ) context
   modify $ \s -> s { pdContext=newContext }
   return impl where

   setUsedImpl :: [Impl] -> [Impl]
   setUsedImpl = map (\argImpl ->
      if implCode argImpl == implCode impl then
         argImpl { implUsed=UsedArg }
      else argImpl)

argImplicit :: (?isSimple::Bool) => ParseState Impl
argImplicit = do
   if ?isSimple
   then parseError "Expecting more expressions at EOF"
   else do
      allArgs <- getAllArgs
      modify $ \s -> s { pdImplicitArgUsed = True }
      argn $ 1 + (fromMaybe 0 $ findIndex ((==UnusedArg).implUsed) allArgs)

flattenArg (Args impls (LambdaArg) _) = impls
flattenArg (Args impls (LetArg _) _) = tail impls

getAllArgs :: ParseState [Impl]
getAllArgs = do
   context <- gets pdContext
   return $ concatMap flattenArg context

addLambda :: Args -> Impl -> Impl
addLambda arg (Impl t body d _ _) = Impl {
   implType = t,
   implCode = hsFn (map implCode $ argsImpls arg) body,
   implDeps = Set.difference d (getArgDeps arg),
   implName = Nothing,
   implUsed = UsednessDoesntMatter }

-- Remove arg # and all its dependent let args (adding let statements for them).
popArg :: Int -> Impl -> ParseState Impl
popArg depth impl = do
   context <- gets pdContext
   let (finalImpl, finalContext) = mapAccumL maybePopIt impl context
   modify $ \s -> s { pdContext=concat finalContext }
   return finalImpl where

   maybePopIt :: Impl -> Args -> (Impl, [Args])
   maybePopIt impl eachArg
      | Set.member depth (getArgDeps eachArg) = (popIt eachArg impl, [])
      | otherwise = (impl, [eachArg])

   popIt :: Args -> Impl -> Impl
   popIt (Args _ LambdaArg _) impl = impl
   popIt (Args varImpls (LetArg refHs) _) (Impl retT bodyHs dep _ _) =
      Impl retT (hsLet (map implCode varImpls) refHs bodyHs) dep Nothing UsednessDoesntMatter

getArgDeps (Args (impl : _) _ _) = implDeps impl -- They should all be the same
getArgDeps (Args [] _ _) = implDeps noArgsUsed
--------- for debugging // errors -----------

debugContext :: [Args] -> String
debugContext context = "\nContext:\n" ++ (unlines $ snd $ mapAccumL (\count arg ->
   let args = flattenArg arg in
   (count+length args, unlines $ showArgType arg : zipWith (\n a->("  "++showArg n a)) [count..] args)
   ) 0 $ filter (not.null.flattenArg) context)

showArgType (Args _ LambdaArg from) = "LambdaArg " ++ from
showArgType (Args _ (LetArg _) from) = "LetArg " ++ from

showArg n impl = snd (indexToOp n) ++ " " ++ fromMaybe "" (implName impl) ++ " :: " ++ (toHsReadType $ implType impl) ++ " " ++ explainUsedness (implUsed impl)
-- only really useful for debugging code that checks where let statements go
-- ++ " deps: " ++ show (Set.toList $ implDeps impl)

explainUsedness OptionalArg = "" -- "(optionally used, equivalent to used in priority)"
explainUsedness UnusedArg = "(unused so far, prioritized for implicit args)"
explainUsedness UsedArg = "" -- ""
explainUsedness UsednessDoesntMatter = "usedness doesn't matter, should be impossible"

indexToOp :: Int -> ([Int], String)
indexToOp = fromMaybe (error "deBruijn index too high") . (at deBruijnArgReps) where

deBruijnArgReps :: [([Int], String)]
deBruijnArgReps = [(replicate unary 6 ++ [nib], replicate unary ';' ++ sym)
              | unary <- [0..11], (sym,nib) <- [("$",3),("@",4),("_",5)]]