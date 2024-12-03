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
import Data.Array.IArray
--import qualified Data.Map as M
import qualified SDL.Raw.Enum as Raw

import Immutaball.Ball.Game
import Immutaball.Ball.LevelSets
import Immutaball.Ball.State.Game
import Immutaball.Share.Config
import Immutaball.Share.GUI
import Immutaball.Share.Level
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- TODO DEBUG
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO

-- TODO:
mkPlayState :: Maybe LevelSet -> String -> LevelIB -> (Either IBContext IBStateContext -> Immutaball) -> Either IBContext IBStateContext -> Immutaball
mkPlayState mlevelSet levelPath level mkBack baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)

		-- GUI.
		(_guiResponse, cxtnp1) <- mkGUI playGui -< (GUIDrive request, cxtn)
		let response = ContinueResponse

		let isEsc  = (const False ||| (== (fromIntegral Raw.SDLK_ESCAPE, True))) . matching _Keybd $ request
		let isBack = isEsc

		-- Set up and step the game.
		-- TODO: implement hasLevelBeenCompleted bool.
		let theInitialGameState = initialGameState (cxtnp1^.ibContext) (cxtnp1^.ibNeverballrc) False mlevelSet levelPath level
		--lastGameState <- delay theInitialGameState -< gameState
		lastGameState <- delayWith -< (gameState, theInitialGameState)
		(GameResponse _gameEvents gameState cxtnp2) <- stepGame -< GameRequest request lastGameState cxtnp1

		-- Render the scene.
		let (mviewDefault :: MView) = MView {
			_mviewPos    = Vec3 0.0 0.0 0.0,
			_mviewTarget = Vec3 0.0 1.0 0.0,
			_mviewFov    = 2 * (fromIntegral $ cxtnp2^.ibNeverballrc.viewFov)
		}
		let (maybeView :: Maybe View) = (level^.solWv) !? 0
		let (mview :: MView) = (\f -> maybe mviewDefault f maybeView) $ \view_ -> MView {
			_mviewPos    = view_^.viewP,
			_mviewTarget = view_^.viewQ,
			_mviewFov    = 2 * (fromIntegral $ cxtnp2^.ibNeverballrc.viewFov)
		}
		-- TODO DEBUG
		() <- initial -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG0: mkPlayState: mview is " ++ show (mview)) ()
		() <- initial -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG0: mkPlayState: viewMat mview is " ++ show (viewMat mview)) ()
		isPaint <- returnA -< ((const False) ||| (const True)) . matching (_Paint) $ request
		cxtnp3 <- returnA ||| renderLevel -< if' (not isPaint) (Left cxtnp2) (Right $ ((mview, (gameState^.gsSwa), gameState), cxtnp2))

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
