{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, ScopedTypeVariables #-}

module Immutaball.Share.State.Context
	(
		IBStateContext(..), ibContext, ibNeverballrc, ibSDLWindow,
			ibSDLGLContext, ibSDLFont, ibGLTextureNames, ibGLTextTextures,
		initialStateCxt,
		stateContextStorage,
		requireVideo,
		requireFont,
		requireGLTextureNames,
		requireGLTextTextures,
		requireMisc,
		requireBasics,
		finishFrame,
		finishFramePaint,
		checkGLErrors,
		glErrType,

		-- * Utils
		newTextureNameWithoutGenText,
		freeTextureNameWithoutGenText,
		newTextureName,
		freeTextureName,
		createTexture,
		freeTexture,

		cachingRenderText,
		uncacheText,
		clearTextCache
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Maybe

import Control.Lens
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Text as T
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified SDL.Font as SDL.Font  -- (sdl2-ttf)
import SDL.Vect as SDL
import qualified SDL.Video as SDL  -- (ambiguous ‘createTexture’)
--import SDL.Video.OpenGL as SDL
import System.FilePath

import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.Context.Config
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.Math
import Immutaball.Share.SDLManager
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
	_ibGLTextureNames :: Maybe (TVar (S.Set GLuint, S.Set GLuint)),
	_ibGLTextTextures :: Maybe (TVar (M.Map T.Text (WidthHeightI, GLuint)))
}
makeLenses ''IBStateContext

initialStateCxt :: IBContext -> IBStateContext
initialStateCxt cxt = IBStateContext {
	_ibContext = cxt,

	_ibNeverballrc = cxt^.ibNeverballrc0,

	_ibSDLWindow      = Nothing,
	_ibSDLGLContext   = Nothing,
	_ibSDLFont        = Nothing,
	_ibGLTextureNames = Nothing,
	_ibGLTextTextures = Nothing
}

stateContextStorage :: IBStateContext -> Wire ImmutaballM (Maybe IBStateContext) IBStateContext
stateContextStorage y0 = proc cxt -> do
	hold y0 -< cxt

-- TODO: I forget SDL wants video mode setting in a special thread.  So send
-- these with window and with gl context calls over to SDLManager.
requireVideo :: Wire ImmutaballM IBStateContext IBStateContext
requireVideo = proc cxt0 -> do
	case ((cxt0^.ibContext.ibHeadless), (cxt0^.ibSDLWindow)) of
		(True, _)        -> returnA -< cxt0
		(False, Just _)  -> returnA -< cxt0
		(False, Nothing) -> do
			let windowTitle = T.pack "Immutaball"
			let windowCfg = SDL.defaultWindow {
				SDL.windowMode = if' (cxt0^.ibNeverballrc.fullscreen) SDL.Fullscreen SDL.Windowed,
				SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL,
				SDL.windowInitialSize = V2 (fromIntegral $ (cxt0^.ibNeverballrc.width)) (fromIntegral $ cxt0^.ibNeverballrc.height)
			}
			if not sdlNeedsSpecialThread
				then do
					window <- monadic -< liftIBIO . BasicIBIOF . SDLIO $ SDLWithWindow windowTitle windowCfg id
					context <- monadic -< liftIBIO . BasicIBIOF . SDLIO $ SDLWithGLContext window id
					let cxt1 = cxt0 & (ibSDLWindow.~Just (window :: SDL.Window)) . (ibSDLGLContext.~Just (context :: SDL.GLContext))
					returnA -< cxt1
				else do
					let h = cxt0^.ibContext.ibSDLManagerHandle
					mwindow <- monadic -< liftIBIO $ Atomically (newEmptyTMVar) id
					() <- monadic -< liftIBIO $ issueSDLCommand h (WithWindow windowTitle windowCfg mwindow) ()
					window <- monadic -< liftIBIO $ Atomically (readTMVar mwindow) id
					mcontext <- monadic -< liftIBIO $ Atomically (newEmptyTMVar) id
					() <- monadic -< liftIBIO $ issueSDLCommand h (WithGLContext window mcontext) ()
					context <- monadic -< liftIBIO $ Atomically (readTMVar mcontext) id
					let cxt1 = cxt0 & (ibSDLWindow.~Just (window :: SDL.Window)) . (ibSDLGLContext.~Just (context :: SDL.GLContext))
					returnA -< cxt1
	where
		sdlNeedsSpecialThread :: Bool
		sdlNeedsSpecialThread = True

requireFont :: Wire ImmutaballM IBStateContext (SDL.Font.Font, IBStateContext)
requireFont = proc cxt0 -> do
	case (cxt0^.ibSDLFont) of
		Just font -> returnA -< (font, cxt0)
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
			returnA -< (font, cxt1)
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

requireGLTextTextures :: Wire ImmutaballM IBStateContext (TVar (M.Map T.Text (WidthHeightI, GLuint)), IBStateContext)
requireGLTextTextures = proc cxt0 -> do
	case (cxt0^.ibGLTextTextures) of
		Just glTextTextures -> returnA -< (glTextTextures, cxt0)
		Nothing -> do
			glTextTextures <- monadic -< liftIBIO $ Atomically (newTVar M.empty) id
			let cxt1 = cxt0 & (ibGLTextTextures.~Just glTextTextures)
			returnA -< (glTextTextures, cxt1)

requireMisc :: Wire ImmutaballM IBStateContext IBStateContext
requireMisc = snd <$> requireGLTextTextures <<< snd <$> requireGLTextureNames <<< snd <$> requireFont <<< id

-- | Also handles common set-up tasks like clearing the color for rendering.
requireBasics :: Wire ImmutaballM (IBStateContext, Request) IBStateContext
requireBasics = proc (cxt0, _request) -> do
	cxt <- requireMisc <<< requireVideo -< cxt0
	() <- monadic -< liftIBIO . BasicIBIOF . GLIO $ GLClearColor 0.1 0.1 0.9 1.0 ()
	() <- monadic -< liftIBIO . BasicIBIOF . GLIO $ GLClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT) ()
	returnA -< cxt

-- | Handles common frame finishing like swapping the scene on paint.
--
-- Currently does not update the state context, so it outputs ().
finishFrame :: Wire ImmutaballM (Request, IBStateContext) ()
finishFrame = proc (request, cxt) -> do
	() <- nopA ||| finishFramePaint -< const () +++ const cxt $ matching _Paint request
	returnA -< ()

finishFramePaint :: Wire ImmutaballM IBStateContext ()
finishFramePaint = proc cxt -> do
	-- Swapping outside the SDL Manager thread on my platform didn't work.  So
	-- we'll have the SDL Manager thread do it.
	() <- monadic -<
		if' (not sdlNeedsSpecialThread)
			(maybe (pure ()) (liftIBIO . BasicIBIOF . SDLIO . flip SDLGLSwapWindow ()) $ (cxt^.ibSDLWindow))
			(maybe (pure ()) (liftIBIO . flip (sdlGLSwapWindow (cxt^.ibContext.ibSDLManagerHandle)) ()) $ (cxt^.ibSDLWindow))
	() <- checkGLErrors -< ()
	returnA -< ()
	where
		sdlNeedsSpecialThread :: Bool
		sdlNeedsSpecialThread = True

checkGLErrors :: Wire ImmutaballM () ()
checkGLErrors = proc () -> do
	error_ <- monadic -< liftIBIO . BasicIBIOF . GLIO $ GLGetError id
	case error_ of
		GL_NO_ERROR -> returnA -< ()
		err -> do
			() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn ("Error: an OpenGL error occurred (" ++ show err ++ "): " ++ glErrType err) ()
			() <- monadic -< liftIBIO . BasicIBIOF $ ExitFailureBasicIOF
			returnA -< ()

glErrType :: GLenum -> String
glErrType GL_NO_ERROR                      = "GL_NO_ERROR"
glErrType GL_INVALID_ENUM                  = "GL_INVALID_ENUM"
glErrType GL_INVALID_VALUE                 = "GL_INVALID_VALUE"
glErrType GL_INVALID_OPERATION             = "GL_INVALID_OPERATION"
glErrType GL_INVALID_FRAMEBUFFER_OPERATION = "GL_INVALID_FRAMEBUFFER_OPERATION"
glErrType GL_OUT_OF_MEMORY                 = "GL_OUT_OF_MEMORY"
glErrType GL_STACK_OVERFLOW                = "GL_STACK_OVERFLOW"
glErrType GL_STACK_UNDERFLOW               = "GL_STACK_UNDERLOW"
glErrType _                                = "unknown error type"

-- * Utils

-- Edit: actually we need glGenTextures to create names.
-- So don't use the 'WithoutGenText' versions since they won't work.
newTextureNameWithoutGenText :: Wire ImmutaballM IBStateContext (GLuint, IBStateContext)
newTextureNameWithoutGenText = proc cxtn -> do
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

freeTextureNameWithoutGenText :: Wire ImmutaballM (GLuint, IBStateContext) IBStateContext
freeTextureNameWithoutGenText = proc (name, cxtn) -> do
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

newTextureName :: Wire ImmutaballM IBStateContext (GLuint, IBStateContext)
newTextureName = proc cxtn -> do
	(sdlGL1' :: GLIOF me -> ImmutaballM me) <- returnA -< liftIBIO . sdlGL1 (cxtn^.ibContext.ibSDLManagerHandle)
	[name] <- monadic -< sdlGL1' $ GLGenTextures 1 id
	returnA -< (name, cxtn)

freeTextureName :: Wire ImmutaballM (GLuint, IBStateContext) IBStateContext
freeTextureName = proc (_name, cxtn) -> do
	returnA -< cxtn

-- | Tight RGBA.
-- TODO: SDL thread.
createTexture :: Wire ImmutaballM ((WidthHeightI, BL.ByteString), IBStateContext) (GLuint, IBStateContext)
createTexture = proc (((w, h), image), cxtn) -> do
	(name, cxtnp1) <- newTextureName -< cxtn
	(sdlGL1' :: GLIOF me -> ImmutaballM me) <- returnA -< liftIBIO . sdlGL1 (cxtnp1^.ibContext.ibSDLManagerHandle)
	() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG: createTexture: w, h is " ++ show (w, h, image)) ()
	() <- monadic -< sdlGL1' $ do
		GLEnable GL_TEXTURE_2D ()
		GLActiveTexture GL_TEXTURE0 ()
		GLClientActiveTexture GL_TEXTURE0 ()
		GLBindTexture GL_TEXTURE_2D name ()
		GLTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral w) (fromIntegral h) 0 GL_RGBA GL_UNSIGNED_BYTE image ()
		-- TODO: re-enable after debugging.
		--when (cxtnp1^.ibNeverballrc.mipmap) $ do
		--	GLGenerateMipmap name ()
	returnA -< (name, cxtnp1)

-- | This also frees the texture, not just the name.
freeTexture :: Wire ImmutaballM (GLuint, IBStateContext) IBStateContext
freeTexture = proc (name, cxtn) -> do
	cxtnp1 <- freeTextureName -< (name, cxtn)
	(sdlGL1' :: GLIOF me -> ImmutaballM me) <- returnA -< liftIBIO . sdlGL1 (cxtnp1^.ibContext.ibSDLManagerHandle)
	() <- monadic -< sdlGL1' $ GLDeleteTextures [name] ()
	returnA -< cxtnp1

-- | Render a text.
--
-- Since rending a text may have a cost, hold onto the texture until its freed
-- with 'uncacheText'.
cachingRenderText :: Wire ImmutaballM (T.Text, IBStateContext) ((WidthHeightI, GLuint), IBStateContext)
cachingRenderText = proc (text, cxtn) -> do
	(mglTextTextures, cxtnp1) <- requireGLTextTextures -< cxtn
	(sdl' :: SDLIOF me -> ImmutaballM me) <- returnA -< liftIBIO . sdl (cxtnp1^.ibContext.ibSDLManagerHandle)
	glTextTextures <- monadic -< liftIBIO $ Atomically (readTVar mglTextTextures) id
	case M.lookup text glTextTextures of
		Just ((w, h), name) -> do
			returnA -< (((w, h), name), cxtnp1)
		Nothing -> do
			(font, cxtnp2) <- requireFont -< cxtnp1
			-- We were doing it ourselves, but it's unclear if it's designed to
			-- work outside the SDL thread.
			--((w, h), image) <- monadic -< liftIBIO . BasicIBIOF . SDLIO $ SDLTTFRenderSync font text id
			((w, h), image) <- monadic -< sdl' $ SDLTTFRenderSync font text id
			(name, cxtnp3) <- createTexture -< (((w, h), image), cxtnp2)
			-- Also see if somebody already cached our text while we were
			-- creating the texture.
			raceAlreadyCached <- monadic -< liftIBIO . flip Atomically id $ do
				glTextTextures2 <- readTVar mglTextTextures
				case M.lookup text glTextTextures2 of
					Nothing -> do
						let glTextTextures3 = M.insert text ((w, h), name) glTextTextures2
						writeTVar mglTextTextures glTextTextures3
						return Nothing
					Just ((w2, h2), name2) -> do
						return $ Just ((w2, h2), name2)
			case raceAlreadyCached of
				Nothing -> do returnA -< (((w, h), name), cxtnp3)
				Just ((w2, h2), name2) -> do
					cxtnp4 <- freeTexture -< (name, cxtnp3)
					returnA -< (((w2, h2), name2), cxtnp4)

-- | Can be repeated; does not if no cache exists for the text.
--
-- Delete the texture and free its texture name if the text has a texture
-- created for it.
uncacheText :: Wire ImmutaballM (T.Text, IBStateContext) IBStateContext
uncacheText = proc (text, cxtn) -> do
	(mglTextTextures, cxtnp1) <- requireGLTextTextures -< cxtn
	mname <- monadic -< liftIBIO . flip Atomically id $ do
		glTextTextures <- readTVar mglTextTextures
		let glTextTextures2 = M.delete text glTextTextures
		writeTVar mglTextTextures glTextTextures2
		return $ snd <$> M.lookup text glTextTextures
	case mname of
		Nothing -> returnA -< cxtnp1
		Just name -> do
			cxtnp2 <- freeTexture -< (name, cxtnp1)
			returnA -< cxtnp2

clearTextCache :: Wire ImmutaballM IBStateContext IBStateContext
clearTextCache = proc cxtn -> do
	(mglTextTextures, cxtnp1) <- requireGLTextTextures -< cxtn
	glTextTextures <- monadic -< liftIBIO $ Atomically (readTVar mglTextTextures) id
	let (texts :: [T.Text]) = M.keys glTextTextures
	foldrA uncacheText -< (cxtnp1, texts)
