{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Play.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, TemplateHaskell, ScopedTypeVariables #-}

module Immutaball.Ball.State.Play
	(
		mkPlayState,
		PlayWidget(..), AsPlayWidget(..),
		playGui
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Lens
--import Control.Monad
--import Data.Functor.Identity

--import Control.Lens
--import Data.Array.IArray
--import qualified Data.Map as M
import qualified SDL.Raw.Enum as Raw

import Immutaball.Ball.Game
import Immutaball.Ball.LevelSets
import Immutaball.Ball.State.Game
import Immutaball.Share.GUI
import Immutaball.Share.Level
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- TODO:
mkPlayState :: Maybe LevelSet -> String -> LevelIB -> (Either IBContext IBStateContext -> Immutaball) -> Either IBContext IBStateContext -> Immutaball
mkPlayState mlevelSet levelPath level mkBack baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)

		-- GUI: don't process here quite yet, only because our overal rendering
		-- plan requires the scene to render first, before GUI.

		-- Set up and step the game.
		-- TODO: implement hasLevelBeenCompleted bool for initialGameState call.  Just settting to False in the meantime.
		let theInitialGameState = initialGameState (cxtn^.ibContext) (cxtn^.ibNeverballrc) False mlevelSet levelPath level
		--lastGameState <- delay theInitialGameState -< gameState
		lastGameState <- delayWith -< (gameState, theInitialGameState)
		(GameResponse _gameEvents gameState cxtnp1) <- stepGame -< GameRequest request lastGameState cxtn
		let gameStateAnalysis = mkGameStateAnalysis cxtnp1 gameState

		-- Render the scene.
		let (mview :: MView) = gameStateAnalysis^.gsaView
		isPaint <- returnA -< ((const False) ||| (const True)) . matching (_Paint) $ request
		cxtnp2 <- returnA ||| renderLevel -< if' (not isPaint) (Left cxtnp1) (Right $ ((mview, (gameState^.gsSwa), gameState), cxtnp2))

		-- GUI.  Positioned after scene rendering.
		(_guiResponse, cxtnp3) <- mkGUI playGui -< (GUIDrive request, cxtnp2)
		let response = ContinueResponse

		let isEsc  = (const False ||| (== (fromIntegral Raw.SDLK_ESCAPE, True))) . matching _Keybd $ request
		let isBack = isEsc

		() <- finishFrame -< (request, cxtnp3)
		cxt <- returnA -< cxtnp3

	-- Switch on Back button.
	let switchTo = if' (not isBack) Nothing . Just . openSecondI $ mkBack (Right cxt)
	returnA -< (Identity response, switchTo)

	where cxt0 = either initialStateCxt id baseCxt0

data PlayWidget =
	  PlayRoot
	| Anonymous Integer
	deriving (Eq, Ord, Show)
--makeClassyPrisms ''PlayWidget

-- TODO:
playGui :: [Widget PlayWidget]
playGui =
	[
		RootWidget $ Root { _rootWid = PlayRoot }
	]

makeClassyPrisms ''PlayWidget
