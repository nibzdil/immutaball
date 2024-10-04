{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows #-}

module Immutaball.Share.State.Context
	(
		IBStateContext(..), ibContext, ibNeverballrc, ibSDLWindow,
		stateContextStorage,
		requireVideo
	) where

import Prelude ()
import Immutaball.Prelude

--import Control.Arrow
import Control.Lens
import SDL.Video as SDL

import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.State
import Immutaball.Share.Wire

-- | A running Immutaball context instance.
--
-- Normally the controller doesn't deal with this.
data IBStateContext = IBStateContext {
	_ibContext :: IBContext,

	_ibNeverballrc :: Neverballrc,

	-- TODO: Maybe Window and gl context.
	_ibSDLWindow :: Maybe (SDL.Window)
}
makeLenses ''IBStateContext

stateContextStorage :: IBStateContext -> Wire ImmutaballM (Maybe IBStateContext) IBStateContext
stateContextStorage y0 = proc cxt -> do
	hold y0 -< cxt

requireVideo :: Wire ImmutaballM IBStateContext Response
requireVideo = proc _cxt -> do
	error "TODO: unimplemented" -< ()
