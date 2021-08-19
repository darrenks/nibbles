-- A list with length of O(1) length checking
-- It is also a dlist for fast ++

-- Note order values length before lex

-- This really only exist for a faster commutative order check

module SmartList --(SmartList,newSmartList,smartLength)
	where

import qualified Data.DList as DList -- needs cabal install --lib dlist

data SmartList a = SmartList Int (DList.DList a) deriving (Show, Eq, Ord)

newSmartList t = SmartList (length t) (DList.fromList t)
smartLength (SmartList len _) = len
smartAppend (SmartList l1 v1) (SmartList l2 v2) = SmartList (l1+l2) (DList.append v1 v2)
smartToList (SmartList _ v) = DList.toList v