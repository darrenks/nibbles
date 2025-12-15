module FileQuoter where

import Language.Haskell.TH -- needs cabal install --lib template-haskell
import Language.Haskell.TH.Quote -- needs cabal install --lib template-haskell

-- Quasi-quoter for adding files as string constants
-- Taken from https://stackoverflow.com/a/12717160/7588488 via Husk
litFile :: QuasiQuoter
litFile = quoteFile $ QuasiQuoter {quoteExp = return . LitE . StringL,
                                   quotePat = undefined,
                                   quoteType = undefined,
                                   quoteDec = undefined}
