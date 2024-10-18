{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Ball.Game
	(
		ChallengeModeState(..), cmsTotalCoins, cmsTotalTimeCs, cmsTotalDeaths,
		initialChallenegModeState
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens

data ChallengeModeState = ChallengeModeState {
	_cmsTotalCoins  :: Integer,
	_cmsTotalTimeCs :: Integer,  -- ^ centiseconds
	_cmsTotalDeaths :: Integer
}
makeLenses ''ChallengeModeState

initialChallenegModeState :: ChallengeModeState
initialChallenegModeState = ChallengeModeState {
	_cmsTotalCoins  = 0,
	_cmsTotalTimeCs = 0,
	_cmsTotalDeaths = 0
}