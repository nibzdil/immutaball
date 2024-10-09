{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows #-}

module Immutaball.Share.State.Context
	(
		IBStateContext(..), ibContext, ibNeverballrc, ibSDLWindow,
			ibSDLGLContext,
		initialStateCxt,
		stateContextStorage,
		requireVideo,
		requireBasics,
		finishFrame
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Data.Bits

import Control.Lens
import qualified Data.Text as T
import Graphics.GL.Core45
import SDL.Vect as SDL
import SDL.Video as SDL
--import SDL.Video.OpenGL as SDL

import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.State
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- | A running Immutaball context instance.
--
-- Normally the controller doesn't deal with this.
data IBStateContext = IBStateContext {
	_ibContext :: IBContext,

	_ibNeverballrc :: Neverballrc,

	_ibSDLWindow :: Maybe (SDL.Window),
	_ibSDLGLContext :: Maybe (SDL.GLContext)
}
makeLenses ''IBStateContext

initialStateCxt :: IBContext -> IBStateContext
initialStateCxt cxt = IBStateContext {
	_ibContext = cxt,

	_ibNeverballrc = cxt^.ibNeverballrc0,

	_ibSDLWindow = Nothing,
	_ibSDLGLContext = Nothing
}

stateContextStorage :: IBStateContext -> Wire ImmutaballM (Maybe IBStateContext) IBStateContext
stateContextStorage y0 = proc cxt -> do
	hold y0 -< cxt

requireVideo :: Wire ImmutaballM IBStateContext IBStateContext
requireVideo = proc cxt0 -> do
	case ((cxt0^.ibContext.ibHeadless), (cxt0^.ibSDLWindow)) of
		(True, _)        -> returnA -< cxt0
		(False, Just _)  -> returnA -< cxt0
		(False, Nothing) -> do
			let windowCfg = defaultWindow {
				windowMode = if' (cxt0^.ibNeverballrc.fullscreen) SDL.Fullscreen SDL.Windowed,
				windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL,
				windowInitialSize = V2 (fromIntegral $ (cxt0^.ibNeverballrc.width)) (fromIntegral $ cxt0^.ibNeverballrc.height)
			}
			window <- monadic -< liftIBIO . BasicIBIOF . SDLIO $ SDLWithWindow (T.pack "Immutaball") windowCfg id
			context <- monadic -< liftIBIO . BasicIBIOF . SDLIO $ SDLWithGLContext window id
			let cxt1 = cxt0 & (ibSDLWindow.~Just (window :: SDL.Window)) . (ibSDLGLContext.~Just (context :: SDL.GLContext))
			returnA -< cxt1

-- | Also handles common set-up tasks like clearing the color for rendering.
requireBasics :: Wire ImmutaballM (IBStateContext, Request) IBStateContext
requireBasics = proc (cxt0, _request) -> do
	cxt <- requireVideo -< cxt0
	() <- monadic -< liftIBIO . BasicIBIOF . GLIO $ GLClearColor 0.1 0.1 0.9 1.0 ()
	() <- monadic -< liftIBIO . BasicIBIOF . GLIO $ GLClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT) ()
	returnA -< cxt

-- | Handles common frame finishing like swapping the scene on paint.
finishFrame :: Wire ImmutaballM IBStateContext ()
finishFrame = proc cxt -> do
	() <- monadic -< maybe (pure ()) (liftIBIO . BasicIBIOF . SDLIO . flip SDLGLSwapWindow ()) $ (cxt^.ibSDLWindow)
	returnA -< ()
