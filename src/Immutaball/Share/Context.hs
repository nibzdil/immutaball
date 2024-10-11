{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Context
	(
		IBContext'(..), ibStaticConfig, ibDirs, ibNeverballrc0, ibInitialWire,
			ibHeadless, ibSDLManagerHandle, ibGLManagerHandle,
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
import Immutaball.Share.GLManager
import Immutaball.Share.SDLManager
import Immutaball.Share.Utils

-- | An Immutaball context instance.
--
-- The controller has access to this.
data IBContext' initialWire = IBContext {
	_ibStaticConfig :: StaticConfig' (IBContext' initialWire -> Maybe initialWire),
	_ibDirs         :: IBDirs,
	-- | The _initial_ neverballrc.
	_ibNeverballrc0 :: Neverballrc,
	_ibInitialWire  :: IBContext' initialWire -> Maybe initialWire,
	_ibHeadless     :: Bool,

	_ibSDLManagerHandle :: SDLManagerHandle,
	_ibGLManagerHandle :: GLManagerHandle
}
makeLenses ''IBContext'

-- | Just set up SDL.
--
-- Does not create a window or set up OpenGL.
withSDL :: ContextConfig' (IBContext' initialWire) initialWire -> (IBContext' initialWire -> ImmutaballIO) -> ImmutaballIO
withSDL cxtCfg withCxt =
	let initFlags = if' (cxtCfg^.cxtCfgHeadless) [] [SDL.Init.InitVideo, SDL.Init.InitAudio] ++ [SDL.Init.InitJoystick] in
	mkBIO . SDLIO . SDLWithInit initFlags .
	mkBIO . SDLIO . SDLWithTTFInit .
	withSDLManager $ \sdlManagerHandle ->
	withGLManager $ \glManagerHandle ->
	(<>> quitSDLManager sdlManagerHandle) .
	(<>> quitGLManager glManagerHandle) .
	withCxt $ IBContext {
		_ibStaticConfig = cxtCfg^.cxtCfgStaticConfig,
		_ibDirs         = cxtCfg^.cxtCfgDirs,
		_ibNeverballrc0 = cxtCfg^.cxtCfgNeverballrc,
		_ibInitialWire  = cxtCfg^.cxtCfgInitialWire,
		_ibHeadless     = cxtCfg^.cxtCfgHeadless,

		_ibSDLManagerHandle = sdlManagerHandle,
		_ibGLManagerHandle  = glManagerHandle
	}
