{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, TemplateHaskell #-}

module Immutaball.Ball.State.Title
	(
		mkTitleState,
		TitleWidget(..), AsTitleWidget(..),
		titleGui
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Lens
--import Data.Functor.Identity

import qualified SDL.Raw.Enum as Raw

import qualified Immutaball.Ball.State.LevelSets as LevelSets
import Immutaball.Share.GUI
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

mkTitleState :: Either IBContext IBStateContext -> Immutaball
mkTitleState baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)

		(guiResponse, cxtnp1) <- mkGUI titleGui -< (GUIDrive request, cxtn)
		response0 <- returnA -< case guiResponse of
			NoWidgetAction          -> ContinueResponse
			WidgetAction QuitButton -> DoneResponse
			_                       -> ContinueResponse
		response <- returnA -< if' ((const False ||| (== (fromIntegral Raw.SDLK_ESCAPE, True))) . matching _Keybd $ request) DoneResponse response0

		() <- finishFrame -< (request, cxtnp1)
		cxt <- returnA -< cxtnp1

	-- Switch on Play button.
	let switchTo = if' (guiResponse /= WidgetAction PlayButton) Nothing . Just . openSecondI $ LevelSets.mkLevelSetsState mkTitleState (Right cxt)
	returnA -< (Identity response, switchTo)

	where cxt0 = either initialStateCxt id baseCxt0

data TitleWidget =
	  TitleRoot
	| MenuVstack
	| PlayButton
	| QuitButton
	| Anonymous Integer
	deriving (Eq, Ord, Show)
--makeClassyPrisms ''TitleWidget

titleGui :: [Widget TitleWidget]
titleGui =
	[
		RootWidget   $ Root   { _rootWid   = TitleRoot                               },
		VstackWidget $ Vstack { _vstackWid = MenuVstack, _vstackWparent = TitleRoot  },
		ButtonWidget $ Button { _buttonWid = PlayButton, _buttonWparent = MenuVstack,
			_buttonText = "Play", _buttonRect = Just $ Rect (Vec2 (-0.100) ( 0.010)) (Vec2 (0.100) ( 0.090)) },
		ButtonWidget $ Button { _buttonWid = QuitButton, _buttonWparent = MenuVstack,
			_buttonText = "Quit", _buttonRect = Just $ Rect (Vec2 (-0.100) (-0.090)) (Vec2 (0.100) (-0.010)) }
	]

makeClassyPrisms ''TitleWidget
