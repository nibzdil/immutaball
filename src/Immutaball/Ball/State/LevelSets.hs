{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- LevelSets.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Immutaball.Ball.State.LevelSets
	(
		mkLevelSetsState,
		LevelSetsWidget(..),
		levelSetsGui
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Data.Functor.Identity

import Immutaball.Share.GUI
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- TODO:
mkLevelSetsState :: (Either IBContext IBStateContext -> Immutaball) -> Either IBContext IBStateContext -> Immutaball
mkLevelSetsState mkBack baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)
		(guiResponse, cxtnp1) <- mkGUI levelSetsGui -< (GUIDrive request, cxtn)
		response <- returnA -< case guiResponse of
			NoWidgetAction          -> ContinueResponse
			_                       -> ContinueResponse
		() <- finishFrame -< (request, cxtnp1)
		cxt <- returnA -< cxtnp1
	-- Switch on Back button.
	let switchTo = if' (guiResponse /= WidgetAction BackButton) Nothing . Just . openSecondI $ mkBack (Right cxt)
	returnA -< (Identity response, switchTo)
	where cxt0 = either initialStateCxt id baseCxt0

data LevelSetsWidget =
	  LevelSetsRoot
	| MenuVstack
	| BackButton
	| Anonymous Integer
	deriving (Eq, Ord, Show)

levelSetsGui :: [Widget LevelSetsWidget]
levelSetsGui =
	[
		RootWidget   $ Root   { _rootWid   = LevelSetsRoot                               },
		VstackWidget $ Vstack { _vstackWid = MenuVstack, _vstackWparent = LevelSetsRoot  },
		ButtonWidget $ Button { _buttonWid = BackButton, _buttonWparent = MenuVstack,
			_buttonText = "Back", _buttonRect = Just $ Rect (Vec2 0.45 0.48) (Vec2 0.55 0.52) }
	]
