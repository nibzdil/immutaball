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
--import qualified Data.Map as M
import qualified SDL.Raw.Enum as Raw

import Immutaball.Ball.LevelSets
import Immutaball.Share.GUI
import Immutaball.Share.Level
--import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- TODO:
mkPlayState :: LevelSet -> LevelIB -> (Either IBContext IBStateContext -> Immutaball) -> Either IBContext IBStateContext -> Immutaball
mkPlayState _levelSet _level mkBack baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)

		(_guiResponse, cxtnp1) <- mkGUI playGui -< (GUIDrive request, cxtn)
		let response = ContinueResponse

		let isEsc  = (const False ||| (== (fromIntegral Raw.SDLK_ESCAPE, True))) . matching _Keybd $ request
		let isBack = isEsc

		() <- finishFrame -< (request, cxtnp1)
		cxt <- returnA -< cxtnp1

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
