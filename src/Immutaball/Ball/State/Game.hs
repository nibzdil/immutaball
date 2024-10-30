{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Play.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows #-}

-- | Game state and immutaball state interface.
module Immutaball.Ball.State.Game
	(
		GameRequest(..), giRequest, giIBStateContext, grRequest,
		GameResponse(..), goGameEvents, goIBStateContext, grGameEvents,
		GameEvent(..), AsGameEvent(..),
		stepGame
	) where

import Prelude ()
--import Immutaball.Prelude

import Control.Arrow
import Control.Lens

import Immutaball.Ball.Game
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Wire

-- TODO: implement.

data GameRequest = GameRequest {
	_giRequest        :: Request,
	_giIBStateContext :: IBStateContext
}
makeLenses ''GameRequest
grRequest :: Lens' GameRequest Request
grRequest = giRequest

data GameResponse = GameResponse {
	_goGameEvents     :: [GameEvent],
	_goIBStateContext :: IBStateContext
}
--makeLenses ''GameResponse

data GameEvent =
	  NewGameMode GameMode
	-- | PlaceholderEvent
makeLenses ''GameResponse
makeClassyPrisms ''GameEvent

grGameEvents :: Lens' GameResponse [GameEvent]
grGameEvents = goGameEvents

-- | Step the game.
-- TODO: implement.
stepGame :: Wire ImmutaballM GameRequest GameResponse
stepGame = proc gr -> do
	returnA -< GameResponse {
		_goGameEvents = [],
		_goIBStateContext = (gr^.giIBStateContext)
	}
