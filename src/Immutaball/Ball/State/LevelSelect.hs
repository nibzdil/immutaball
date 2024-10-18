{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- LevelSelect.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, ScopedTypeVariables #-}

module Immutaball.Ball.State.LevelSelect
	(
		mkLevelSelectState,
		LevelSelectWidget(..),
		levelSelectGui,
		levelSelectBaseGui,
		levelSelectButtons
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Lens
import Control.Monad
--import Data.Functor.Identity

--import Control.Lens
--import qualified Data.Map as M
import qualified SDL.Raw.Enum as Raw

import Immutaball.Ball.LevelSets
import Immutaball.Share.GUI
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- TODO: switch to next state
mkLevelSelectState :: LevelSet -> (Either IBContext IBStateContext -> Immutaball) -> Either IBContext IBStateContext -> Immutaball
mkLevelSelectState levelSet mkBack baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)

		--let (levelSet :: LevelSet) = levelSet

		--(guiResponse, cxtnp1) <- mkGUI $ levelSelectGui levelSet -< (GUIDrive request, cxtn)
		(guiResponse, cxtnp1) <- withM (\gui -> second (mkGUI gui) >>> arr snd) (return . fst) -< (levelSelectGui levelSet, (GUIDrive request, cxtn))
		let response = ContinueResponse

		let isEsc  = (const False ||| (== (fromIntegral Raw.SDLK_ESCAPE, True))) . matching _Keybd $ request
		let isBack = isEsc || guiResponse == WidgetAction BackButton

		() <- finishFrame -< (request, cxtnp1)
		cxt <- returnA -< cxtnp1

	-- Switch on Back button.
	let switchTo = if' (not isBack) Nothing . Just . openSecondI $ mkBack (Right cxt)
	returnA -< (Identity response, switchTo)

	where cxt0 = either initialStateCxt id baseCxt0

data LevelSelectWidget =
	  LevelSelectRoot
	| BackButton
	| LevelsVstack
	| LevelButton String
	| Anonymous Integer
	deriving (Eq, Ord, Show)

-- TODO: make a better UI.  For now we just have a simple list of levels.

levelSelectGui :: LevelSet -> [Widget LevelSelectWidget]
levelSelectGui levelSet =
	levelSelectBaseGui ++
	levelSelectButtons levelSet

levelSelectBaseGui :: [Widget LevelSelectWidget]
levelSelectBaseGui =
	[
		RootWidget   $ Root   { _rootWid   = LevelSelectRoot                                },
		ButtonWidget $ Button { _buttonWid = BackButton, _buttonWparent = LevelSelectRoot,
			_buttonText = "Back", _buttonRect = Just $ Rect (Vec2 (-0.800) (0.720)) (Vec2 (-0.700) (0.800)) },
		VstackWidget $ Vstack { _vstackWid = LevelsVstack, _vstackWparent = LevelSelectRoot }
	]

levelSelectButtons :: LevelSet -> [Widget LevelSelectWidget]
levelSelectButtons levelSet = flip map (zip [0..] (levelSet^.lsLevels)) $ \((idx :: Integer), path) ->
	let (r, c) = join (***) fromIntegral $ idx `divMod` 5 in
	ButtonWidget $ Button { _buttonWid = LevelButton path, _buttonWparent = LevelsVstack,
		_buttonText = "Level", _buttonRect = Just $ Rect (Vec2 (-0.165 + c*0.070) (0.160 - r*0.100)) (Vec2 (-0.115 + c*0.070) (0.240 - r*0.100)) }