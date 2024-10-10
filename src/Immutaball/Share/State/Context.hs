{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows #-}

module Immutaball.Share.State.Context
	(
		IBStateContext(..), ibContext, ibNeverballrc, ibSDLWindow,
			ibSDLGLContext, ibSDLFont, ibGLTextureNames,
		initialStateCxt,
		stateContextStorage,
		requireVideo,
		requireFont,
		requireGLTextureNames,
		requireMisc,
		requireBasics,
		finishFrame,

		-- * Utils
		newTextureName,
		freeTextureName
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Data.Bits
import Data.Maybe

import Control.Lens
import Control.Concurrent.STM.TVar
import qualified Data.Set as S
import qualified Data.Text as T
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified SDL.Font as SDL.Font  -- (sdl2-ttf)
import SDL.Vect as SDL
import SDL.Video as SDL
--import SDL.Video.OpenGL as SDL
import System.FilePath

import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.Context.Config
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
	_ibSDLGLContext :: Maybe (SDL.GLContext),
	_ibSDLFont :: Maybe (SDL.Font.Font),

	-- | Used, freed.
	_ibGLTextureNames :: Maybe (TVar (S.Set GLuint, S.Set GLuint))
}
makeLenses ''IBStateContext

initialStateCxt :: IBContext -> IBStateContext
initialStateCxt cxt = IBStateContext {
	_ibContext = cxt,

	_ibNeverballrc = cxt^.ibNeverballrc0,

	_ibSDLWindow      = Nothing,
	_ibSDLGLContext   = Nothing,
	_ibSDLFont        = Nothing,
	_ibGLTextureNames = Nothing
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

requireFont :: Wire ImmutaballM IBStateContext IBStateContext
requireFont = proc cxt0 -> do
	case (cxt0^.ibSDLFont) of
		Just _ -> returnA -< cxt0
		Nothing -> do
			let path = cxt0^.ibContext.ibDirs.ibStaticDataDir </> cxt0^.ibContext.ibStaticConfig.immutaballFont
			let size = cxt0^.ibContext.ibStaticConfig.immutaballFontSize
			-- Since this is likely the first time a data file is accessed,
			-- check the path exists and warn if it doesn't with a helpful
			-- message that ‘-d data-path’ needs to be correct.
			exists <- monadic -< liftIBIO . BasicIBIOF $ DoesPathExistSync path id
			() <- warnIf -< (not exists, ("Warning: ./immutaball -d PATH_TO_DATA_DIR must be passed with a path to a compiled neverball data directory; failed to find font file: " ++ path))
			font <- monadic -< liftIBIO . BasicIBIOF . SDLIO $ SDLTTFLoad path (fromIntegral size) id
			let cxt1 = cxt0 & (ibSDLFont.~Just font)
			returnA -< cxt1
	where
		warnIf :: Wire ImmutaballM (Bool, String) ()
		warnIf = proc (condition, msg) -> do
			() <- monadic -< if' (not condition) (pure ()) . liftIBIO . BasicIBIOF $ PutStrLn msg ()
			returnA -< ()

requireGLTextureNames :: Wire ImmutaballM IBStateContext (TVar (S.Set GLuint, S.Set GLuint), IBStateContext)
requireGLTextureNames = proc cxt0 -> do
	case (cxt0^.ibGLTextureNames) of
		Just glTextureNames -> returnA -< (glTextureNames, cxt0)
		Nothing -> do
			glTextureNames <- monadic -< liftIBIO $ Atomically (newTVar (S.empty, S.empty)) id
			let cxt1 = cxt0 & (ibGLTextureNames.~Just glTextureNames)
			returnA -< (glTextureNames, cxt1)

requireMisc :: Wire ImmutaballM IBStateContext IBStateContext
requireMisc = snd <$> requireGLTextureNames <<< id

-- | Also handles common set-up tasks like clearing the color for rendering.
requireBasics :: Wire ImmutaballM (IBStateContext, Request) IBStateContext
requireBasics = proc (cxt0, _request) -> do
	cxt <- requireMisc <<< requireFont <<< requireVideo -< cxt0
	() <- monadic -< liftIBIO . BasicIBIOF . GLIO $ GLClearColor 0.1 0.1 0.9 1.0 ()
	() <- monadic -< liftIBIO . BasicIBIOF . GLIO $ GLClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT) ()
	returnA -< cxt

-- | Handles common frame finishing like swapping the scene on paint.
finishFrame :: Wire ImmutaballM IBStateContext ()
finishFrame = proc cxt -> do
	() <- monadic -< maybe (pure ()) (liftIBIO . BasicIBIOF . SDLIO . flip SDLGLSwapWindow ()) $ (cxt^.ibSDLWindow)
	returnA -< ()

-- * Utils

newTextureName :: Wire ImmutaballM IBStateContext (GLuint, IBStateContext)
newTextureName = proc cxtn -> do
	(glTextureNames, cxtnp1) <- requireGLTextureNames -< cxtn
	(name, err) <- monadic -< liftIBIO . flip Atomically id $ do
		(used, freed) <- readTVar glTextureNames
		let defaultName = fromMaybe 0 $ (+1) <$> S.lookupMax used
		let name = fromMaybe defaultName $ S.lookupMin freed
		let (used', freed') = (S.insert name used, S.delete name freed)
		writeTVar glTextureNames (used', freed')
		let err = if' (not $ name `S.member` used) Nothing $ Just ("Error: newTextureName: created texture name already in use!: " ++ show name)
		return (name, err)
	() <- monadic -< flip (maybe $ pure ()) err $ \errMsg -> liftIBIO $ (BasicIBIOF $ PutStrLn errMsg ()) <>>- BasicIBIOF ExitFailureBasicIOF
	returnA -< (name, cxtnp1)

freeTextureName :: Wire ImmutaballM (GLuint, IBStateContext) IBStateContext
freeTextureName = proc (name, cxtn) -> do
	(glTextureNames, cxtnp1) <- requireGLTextureNames -< cxtn
	err <- monadic -< liftIBIO . flip Atomically id $ do
		(used, freed) <- readTVar glTextureNames
		let (used', freed') = (S.delete name used, S.insert name freed)
		writeTVar glTextureNames (used', freed')
		let err =
			if' (name `S.member` freed)       (Just ("Error: freeTextureName: double free of texture name!: " ++ show name)) .
			if' (not $ name `S.member` used)  (Just ("Error: freeTextureName: free of unallocated texture name!: " ++ show name)) $
			Nothing
		return err
	() <- monadic -< flip (maybe $ pure ()) err $ \errMsg -> liftIBIO $ (BasicIBIOF $ PutStrLn errMsg ()) <>>- BasicIBIOF ExitFailureBasicIOF
	returnA -< cxtnp1

-- TODO: create and delete texture utils (they combine new/free and GL calls).
