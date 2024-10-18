{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- LevelSets.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, ScopedTypeVariables #-}

module Immutaball.Ball.State.LevelSets
	(
		mkLevelSetsState,
		LevelSetsWidget(..),
		levelSetsGui,
		levelSetsBaseGui,
		levelSetsButtons
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Applicative
import Control.Arrow
import Data.Functor.Identity

import Control.Lens
import qualified Data.Map as M
import qualified SDL.Raw.Enum as Raw

import Immutaball.Ball.LevelSets
import qualified Immutaball.Ball.State.LevelSelect as LevelSelect
import Immutaball.Share.GUI
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

mkLevelSetsState :: (Either IBContext IBStateContext -> Immutaball) -> Either IBContext IBStateContext -> Immutaball
mkLevelSetsState mkBack baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)

		(levelSets :: LevelSets) <- initial -< liftIBIO $ getLevelSets (cxtn^.ibContext)

		--(guiResponse, cxtnp1) <- mkGUI $ levelSetsGui levelSets -< (GUIDrive request, cxtn)
		(guiResponse, cxtnp1) <- withM (\gui -> second (mkGUI gui) >>> arr snd) (return . fst) -< (levelSetsGui levelSets, (GUIDrive request, cxtn))
		let response = ContinueResponse

		let isEsc  = (const False ||| (== (fromIntegral Raw.SDLK_ESCAPE, True))) . matching _Keybd $ request
		let isBack = isEsc || guiResponse == WidgetAction BackButton

		() <- finishFrame -< (request, cxtnp1)
		cxt <- returnA -< cxtnp1

	-- Switch on Back button.
	let switchTo0 = if' (not isBack) Nothing . Just . openSecondI $ mkBack (Right cxt)
	let onSet levelSetPath = flip M.lookup (levelSets^.lsLevelSets) levelSetPath >>= \levelSet -> return . openSecondI $ LevelSelect.mkLevelSelectState levelSet (mkLevelSetsState mkBack) (Right cxt)
	let switchTo = switchTo0 <|> case guiResponse of (WidgetAction (LevelSetButton levelSetPath)) -> onSet levelSetPath; _ -> Nothing
	returnA -< (Identity response, switchTo)

	where cxt0 = either initialStateCxt id baseCxt0

data LevelSetsWidget =
	  LevelSetsRoot
	| BackButton
	| LevelSetsVstack
	| LevelSetButton String
	| Anonymous Integer
	deriving (Eq, Ord, Show)

-- TODO: make a better UI.  For now we just have a simple list of level set titles.

levelSetsGui :: LevelSets -> [Widget LevelSetsWidget]
levelSetsGui levelSets =
	levelSetsBaseGui ++
	levelSetsButtons levelSets

levelSetsBaseGui :: [Widget LevelSetsWidget]
levelSetsBaseGui =
	[
		RootWidget   $ Root   { _rootWid   = LevelSetsRoot                               },
		ButtonWidget $ Button { _buttonWid = BackButton, _buttonWparent = LevelSetsRoot,
			_buttonText = "Back", _buttonRect = Just $ Rect (Vec2 (-0.800) (0.720)) (Vec2 (-0.700) (0.800)) },
		VstackWidget $ Vstack { _vstackWid = LevelSetsVstack, _vstackWparent = LevelSetsRoot  }
	]

levelSetsButtons :: LevelSets -> [Widget LevelSetsWidget]
levelSetsButtons levelSets = flip map (zip [0..] . M.toList $ levelSets^.lsLevelSets) $ \((idx :: Integer), (path, levelSet)) ->
	let idx' = fromIntegral idx in
	ButtonWidget $ Button { _buttonWid = LevelSetButton path, _buttonWparent = LevelSetsVstack,
		_buttonText = (levelSet^.lsTitle), _buttonRect = Just $ Rect (Vec2 (-0.100) (0.620 - 0.100*idx')) (Vec2 (0.100) (0.700 - 0.100*idx')) }
