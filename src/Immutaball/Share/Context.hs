{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Context
	(
		IBContext(..), ibStaticConfig, ibDirs, ibNeverballrc0,
		withSDL
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens
import qualified SDL.Init

import Immutaball.Share.Config
import Immutaball.Share.Context.Config
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.SDLIO

-- | An Immutaball context instance.
data IBContext = IBContext {
	_ibStaticConfig :: StaticConfig,
	_ibDirs         :: IBDirs,
	-- | The _initial_ neverballrc.
	_ibNeverballrc0 :: Neverballrc

	-- TODO: Maybe Window and gl context.
}
makeLenses ''IBContext

-- | Just set up SDL.
--
-- Does not create a window or set up OpenGL.
withSDL :: ContextConfig -> (IBContext -> ImmutaballIO () ()) -> ImmutaballIO () ()
withSDL cxtCfg withCxt =
	mkBasicImmutaballIO () . SDLIO . SDLInit [SDL.Init.InitVideo, SDL.Init.InitAudio, SDL.Init.InitJoystick] .
	withCxt $ IBContext {
		_ibStaticConfig = cxtCfg^.ctxCfgStaticConfig,
		_ibDirs         = cxtCfg^.ctxCfgDirs,
		_ibNeverballrc0 = cxtCfg^.ctxCfgNeverballrc
	}
