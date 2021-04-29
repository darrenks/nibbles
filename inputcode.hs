module InputCode where

import Expr

class InputCode c where
	parseStr :: c -> (String, c)
	parseInt :: c -> (Integer, c)
	empty :: c -> Bool
	nextHex :: c -> (Int, c)
	nextInstruction :: c -> c
	match :: c -> (String, [Nibble]) -> Bool
	parseError :: String -> Thunk c -> (c, Expr)
