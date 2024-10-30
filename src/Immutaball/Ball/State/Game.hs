{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Play.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Ball.State.Game
	(
		stepGame
	) where

import Prelude ()
import Immutaball.Prelude

import Immutaball.Ball.Game
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Wire

-- | Clock step.
stepGame :: Wire ImmutaballM ((Double, GameState), IBStateContext) (GameState, IBStateContext)
stepGame = error "TODO: unimplemented."
