{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Controller
	(
		controlImmutaball
	) where

import Prelude ()
--import Immutaball.Prelude

--import Control.Wire

import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.State

controlImmutaball :: IBContext -> Immutaball -> ImmutaballIO
controlImmutaball _cxt0 _immutaball =
	result
	where
		result :: ImmutaballIO
		result = mkPutStrLn "Internal error: unimplemented." <>> mkExitFailureImmutaballIO
