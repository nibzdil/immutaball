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
import Data.Functor.Identity

--import Control.Lens
--import qualified Data.Map as M

import Immutaball.Ball.LevelSets
import Immutaball.Share.GUI
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- TODO:
mkLevelSelectState :: LevelSet -> (Either IBContext IBStateContext -> Immutaball) -> Either IBContext IBStateContext -> Immutaball
mkLevelSelectState levelSet mkBack baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)

		--let (levelSet :: LevelSet) = levelSet

		--(guiResponse, cxtnp1) <- mkGUI $ levelSelectGui levelSet -< (GUIDrive request, cxtn)
		(guiResponse, cxtnp1) <- withM (\gui -> second (mkGUI gui) >>> arr snd) (return . fst) -< (levelSelectGui levelSet, (GUIDrive request, cxtn))
		response <- returnA -< case guiResponse of
			NoWidgetAction          -> ContinueResponse
			_                       -> ContinueResponse

		() <- finishFrame -< (request, cxtnp1)
		cxt <- returnA -< cxtnp1

	-- Switch on Back button.
	let switchTo = if' (guiResponse /= WidgetAction BackButton) Nothing . Just . openSecondI $ mkBack (Right cxt)
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

-- TODO
levelSelectButtons :: LevelSet -> [Widget LevelSelectWidget]
levelSelectButtons _ = []
{-
levelSelectButtons levelSet = flip map (zip [0..] . M.toList $ levelSets^.lsLevelSets) $ \((idx :: Integer), (path, levelSet)) ->
	let idx' = fromIntegral idx in
	ButtonWidget $ Button { _buttonWid = LevelSetButton path, _buttonWparent = LevelSetsVstack,
		_buttonText = (levelSet^.lsTitle), _buttonRect = Just $ Rect (Vec2 (-0.100) (0.620 - 0.100*idx')) (Vec2 (0.100) (0.700 - 0.100*idx')) }
-}
