{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Play.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, ScopedTypeVariables #-}

-- | Game state and immutaball state interface.
module Immutaball.Ball.State.Game
	(
		GameRequest(..), giRequest, giGameState, giIBStateContext, grRequest,
		GameResponse(..), goGameEvents, goGameState, goIBStateContext, grGameEvents,
		GameEvent(..), AsGameEvent(..),
		stepGame,
		renderBall
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Int
import Data.Maybe

import Control.Arrow
import Control.Lens
import Data.Array.IArray

import Immutaball.Ball.Game
import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Video
import Immutaball.Share.Wire

-- TODO: implement.

data GameRequest = GameRequest {
	_giRequest        :: Request,
	_giGameState      :: GameState,
	_giIBStateContext :: IBStateContext
}
makeLenses ''GameRequest
grRequest :: Lens' GameRequest Request
grRequest = giRequest

data GameResponse = GameResponse {
	_goGameEvents     :: [GameEvent],
	_goGameState      :: GameState,
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

	-- TODO step game.

	-- TODO render ball.

	returnA -< GameResponse {
		_goGameEvents = [],
		_goGameState = (gr^.giGameState),
		_goIBStateContext = (gr^.giIBStateContext)
	}

renderBall :: Wire ImmutaballM (GameState, IBStateContext) IBStateContext
renderBall = proc (gs, cxtn) -> do
	hasInit <- delay False -< returnA True
	cxtnp1 <- arr snd ||| renderBallSetup -< if' hasInit Left Right (gs, cxtn)

	-- TODO

	let cxt = cxtnp1
	returnA -< cxt

renderBallSetup :: Wire ImmutaballM (GameState, IBStateContext) IBStateContext
renderBallSetup = proc (_gs0, cxtn) -> do
	-- A new game state hasn't had its ball render setup performed yet.
	-- Perform setup.

	-- First, set up ball elem vao and vbo.  See if a previous setup has
	-- already done it.  Race conditions are okay here, since setting the ball
	-- elem vao would discard an old elem vao in a race properly.
	-- (Alternatively, we could also always set it up here since it would only
	-- happen on a new game state, but checking probably saves a bit of
	-- performance for uploading the VAO.)
	(mballElemVaoVboEbo, cxtnp1) <- getBallElemVaoVboEbo -< cxtn

	-- Set up the data, ballElemVaoVboEboDataGPU.
	let (ballNumTriangles :: Integer) = cxtnp1^.ibContext.ibStaticConfig.x'cfgBallTriangles
	let (ballElemVaoVboEboData :: Array Int32 Int32) = genArray (0, 3 * fromIntegral ballNumTriangles - 1) $ \idx -> idx
	let (ballElemVaoVboEboDataGPU :: GLData) = gpuEncodeArray ballElemVaoVboEboData

	-- Upload ball elems vao and buf.  (Just returnA the cxtnp1 if there's already a ball VAO.)
	cxtnp2 <- arr (^._3) ||| setBallElemVaoVboEbo -< if' (isJust mballElemVaoVboEbo) Left Right (ballElemVaoVboEboDataGPU, True, cxtnp1)

	let cxt = cxtnp2
	returnA -< cxt
