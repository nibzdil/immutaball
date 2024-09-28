{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Ball.CLI
	(
		main,
		immutaballMain,
		immutaballCLIMain
	) where

import Immutaball.ImmutaballIO
--import Immutaball.Utils

main :: IO ()
main = immutaballMain

immutaballMain :: IO ()
immutaballMain = immutaballCLIMain

immutaballCLIMain :: IO ()
immutaballCLIMain = do
	runImmutaballIO mainImmutaballIO

mainImmutaballIO :: ImmutaballIO
mainImmutaballIO =
	mkPutStrLn "Internal error: unimplemented."
