{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Immutaball.Ball.State.Title
	(
		mkTitleState,
		TitleWidget(..),
		titleGui
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Data.Functor.Identity

import Immutaball.Share.GUI
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Wire

import Debug.Trace as D -------------------------- TODO
import Text.Printf

-- TODO: go to next play state
mkTitleState :: Either IBContext IBStateContext -> Immutaball
mkTitleState baseCxt0 = fromImmutaballSingle $ proc (Identity request) -> do
	() <- returnA -< D.trace (printf "DEBUG mkTitleState step: request: %s" (show request)) ()
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)
		(guiResponse, cxtnp1) <- mkGUI titleGui -< (GUIDrive request, cxtn)
		cxt <- returnA -< cxtnp1
	response <- returnA -< case guiResponse of
		NoWidgetAction          -> ContinueResponse
		WidgetAction QuitButton -> DoneResponse
		_                       -> ContinueResponse
	--() <- finishFrame -< cxtn
	case request of
		Paint _ -> finishFrame -< cxtn
		_ -> returnA -< ()
	returnA -< Identity response
	where cxt0 = either initialStateCxt id baseCxt0

data TitleWidget =
	  TitleRoot
	| MenuVstack
	| PlayButton
	| QuitButton
	| Anonymous Integer
	deriving (Eq, Ord, Show)

titleGui :: [Widget TitleWidget]
titleGui =
	[
		RootWidget   $ Root   { _rootWid   = TitleRoot                               },
		VstackWidget $ Vstack { _vstackWid = MenuVstack, _vstackWparent = TitleRoot  },
		ButtonWidget $ Button { _buttonWid = PlayButton, _buttonWparent = MenuVstack,
			_buttonText = "Play", _buttonRect = Just $ Rect (Vec2 0.45 0.48) (Vec2 0.55 0.52) },
		ButtonWidget $ Button { _buttonWid = QuitButton, _buttonWparent = MenuVstack,
			_buttonText = "Quit", _buttonRect = Just $ Rect (Vec2 0.45 0.44) (Vec2 0.55 0.48) }
	]
