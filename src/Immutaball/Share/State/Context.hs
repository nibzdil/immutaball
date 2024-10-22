{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, ScopedTypeVariables #-}

module Immutaball.Share.State.Context
	(
		IBStateContext(..), ibContext, ibNeverballrc, ibSDLWindow,
			ibSDLGLContext, ibSDLFont, ibShader, ibGLTextureNames,
			ibGLTextTextures, ibGLAllocatedTextures,
		initialStateCxt,
		stateContextStorage,
		requireVideo,
		requireShader,
		requireFont,
		requireGLTextureNames,
		requireGLTextTextures,
		requireGLMtrlTextures,
		requireGLAllocatedTextures,
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
		clearTextCache,
		freeTextures,
		ibFreeTextures,

		uncacheMtrl,
		clearMtrlCache,
		freeAllTextures,
		ibFreeAllTextures,
		cachingRenderMtrl,

		juicyPixelsDynamicImageToJPImage,
		juicyPixelsImageToImage,

		checkPrecacheMtrls,
		precacheMtrls,
		precacheMtrlsIB,
		precacheMtrlsDirect
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Either
import Data.List
import Data.Maybe
import Text.Printf

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP
import Control.Lens
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
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
import Immutaball.Share.Video

-- | A running Immutaball context instance.
--
-- Normally the controller doesn't deal with this.
data IBStateContext = IBStateContext {
	_ibContext :: IBContext,

	_ibNeverballrc :: Neverballrc,

	_ibSDLWindow :: Maybe (SDL.Window),
	_ibSDLGLContext :: Maybe (SDL.GLContext),
	_ibSDLFont :: Maybe (SDL.Font.Font),
	_ibShader :: Maybe (TMVar ImmutaballShaderHandle),

	-- | Used, freed.
	_ibGLTextureNames      :: Maybe (TVar (S.Set GLuint, S.Set GLuint)),
	_ibGLTextTextures      :: Maybe (TVar (M.Map T.Text  (WidthHeightI, GLuint))),
	_ibGLMtrlTextures      :: Maybe (TVar (M.Map String ((WidthHeightI, GLuint), MtrlMeta))),
	-- | By tracking allocated textures, we can attach their lifetimes to the
	-- SDL Manager thread to automatically clean them up upon quit.
	-- 'createTexture' and 'freeTexture' use this interface.
	_ibGLAllocatedTextures :: Maybe (TVar (S.Set GLuint, S.Set GLuint))
}
makeLenses ''IBStateContext

initialStateCxt :: IBContext -> IBStateContext
initialStateCxt cxt = IBStateContext {
	_ibContext = cxt,

	_ibNeverballrc = cxt^.ibNeverballrc0,

	_ibSDLWindow           = Nothing,
	_ibSDLGLContext        = Nothing,
	_ibSDLFont             = Nothing,
	_ibShader              = Nothing,
	_ibGLTextureNames      = Nothing,
	_ibGLTextTextures      = Nothing,
	_ibGLMtrlTextures      = Nothing,
	_ibGLAllocatedTextures = Nothing
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
			let windowTitle = T.pack "Immutaball"
			let windowCfg = SDL.defaultWindow {
				SDL.windowMode = if' (cxt0^.ibNeverballrc.fullscreen) SDL.Fullscreen SDL.Windowed,
				SDL.windowGraphicsContext = SDL.OpenGLContext (SDL.defaultOpenGL {SDL.glProfile = SDL.Compatibility SDL.Normal 4 5}),
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

requireShader :: Wire ImmutaballM IBStateContext (ImmutaballShaderHandle, IBStateContext)
requireShader = proc cxt0 -> do
	case (cxt0^.ibShader) of
		Just mshader -> monadic *** id -< (liftIBIO $ Atomically (readTMVar mshader) id, cxt0)
		Nothing -> do
			mshader <- monadic -< liftIBIO $ sdlCreateImmutaballShader (cxt0^.ibContext.ibSDLManagerHandle)
			let cxt1 = cxt0 & (ibShader.~Just mshader)
			shader <- monadic -< liftIBIO $ Atomically (readTMVar mshader) id
			returnA -< (shader, cxt1)

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

requireGLMtrlTextures :: Wire ImmutaballM IBStateContext (TVar (M.Map String ((WidthHeightI, GLuint), MtrlMeta)), IBStateContext)
requireGLMtrlTextures = proc cxt0 -> do
	case (cxt0^.ibGLMtrlTextures) of
		Just glMtrlTextures -> returnA -< (glMtrlTextures, cxt0)
		Nothing -> do
			glMtrlTextures <- monadic -< liftIBIO $ Atomically (newTVar M.empty) id
			let cxt1 = cxt0 & (ibGLMtrlTextures.~Just glMtrlTextures)
			returnA -< (glMtrlTextures, cxt1)

-- | If you require this after you require the mtrl and text cache, you can
-- also free the text and mtrl cache on lifetime end.
requireGLAllocatedTextures :: Wire ImmutaballM IBStateContext (TVar (S.Set GLuint, S.Set GLuint), IBStateContext)
requireGLAllocatedTextures = proc cxt0 -> do
	case (cxt0^.ibGLAllocatedTextures) of
		Just glAllocatedTextures -> returnA -< (glAllocatedTextures, cxt0)
		Nothing -> do
			unusedTo_ <- monadic -< liftIBIO $ Atomically (newTMVar ()) id
			glAllocatedTextures <- monadic -< liftIBIO $ Atomically (newTVar (S.empty, S.empty)) id
			let cxt1 = cxt0 & (ibGLAllocatedTextures.~Just glAllocatedTextures)
			() <- monadic -< liftIBIO $ attachLifetime (cxt0^.ibContext.ibSDLManagerHandle) (pure ()) (\() -> void $ ibFreeAllTextures cxt1) unusedTo_ ()
			--returnA -< (glAllocatedTextures, cxt1)
			cxt2 <- checkPrecacheMtrls -< cxt1
			returnA -< (glAllocatedTextures, cxt2)

requireMisc :: Wire ImmutaballM IBStateContext IBStateContext
requireMisc =
	snd <$> requireShader <<< snd <$> requireGLAllocatedTextures <<<
	snd <$> requireGLMtrlTextures <<<
	snd <$> requireGLTextTextures <<< snd <$> requireGLTextureNames <<<
	snd <$> requireFont <<< id

-- | Also handles common set-up tasks like clearing the color for rendering.
requireBasics :: Wire ImmutaballM (IBStateContext, Request) IBStateContext
requireBasics = proc (cxt0, _request) -> do
	cxtn <- requireMisc <<< requireVideo -< cxt0
	let sdlGL1' = liftIBIO . sdlGL1 (cxtn^.ibContext.ibSDLManagerHandle)
	() <- monadic -< sdlGL1' $ do
		GLClearColor 0.1 0.1 0.9 1.0 ()
		GLClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT) ()
	returnA -< cxtn

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

-- * Utils

-- | This variant does NOT use ibGLAllocatedTextures, so lifetimes would need
-- to be manually managed.  (This is because I'm currently not using this
-- variant.)
--
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

-- | Generate a new texture name.
--
-- Also track its lifetime in ibGLAllocatedTextures, so that ibFreeTextures can
-- be attached to the SDL Manager thread to free all remaining on quit.
newTextureName :: Wire ImmutaballM IBStateContext (GLuint, IBStateContext)
newTextureName = proc cxtn -> do
	(glAllocatedTextures, cxtnp1) <- requireGLAllocatedTextures -< cxtn

	(sdlGL1' :: GLIOF me -> ImmutaballM me) <- returnA -< liftIBIO . sdlGL1 (cxtnp1^.ibContext.ibSDLManagerHandle)
	name <- monadic -< (unSingleton <$>) . sdlGL1' $ GLGenTextures 1 id
	-- Now track in ibGLAllocatedTextures.
	() <- monadic -< liftIBIO . flip Atomically id $ do
		let musedFree = glAllocatedTextures
		usedFree0 <- readTVar musedFree
		let (used0, free0) = usedFree0
		let (used1, free1) = (S.insert name used0, free0)
		let usedFree1 = (used1, free1)
		writeTVar musedFree usedFree1
	returnA -< (name, cxtnp1)
	where
		unSingleton [me] = me
		unSingleton _    = error "Internal error: newTextureName expected a single result from GLGenTextures."

freeTextureName :: Wire ImmutaballM (GLuint, IBStateContext) IBStateContext
freeTextureName = proc (name, cxtn) -> do
	(glAllocatedTextures, cxtnp1) <- requireGLAllocatedTextures -< cxtn

	(sdlGL1' :: GLIOF me -> ImmutaballM me) <- returnA -< liftIBIO . sdlGL1 (cxtnp1^.ibContext.ibSDLManagerHandle)
	() <- monadic -< sdlGL1' $ GLDeleteTextures [name] ()
	-- Now free in ibGLAllocatedTextures - just mark it as freed rather than
	-- used.
	() <- monadic -< liftIBIO . flip Atomically id $ do
		let musedFree = glAllocatedTextures
		usedFree0 <- readTVar musedFree
		let (used0, free0) = usedFree0
		let (used1, free1) = (S.delete name used0, if' keepTrackOfFreedTextureNames (S.insert name free0) free0)
		let usedFree1 = (used1, free1)
		writeTVar musedFree usedFree1
	returnA -< cxtnp1
	where
		keepTrackOfFreedTextureNames :: Bool
		keepTrackOfFreedTextureNames = False

-- | Tight RGBA.
createTexture :: Wire ImmutaballM ((WidthHeightI, BS.ByteString), IBStateContext) (GLuint, IBStateContext)
createTexture = proc (((w, h), image), cxtn) -> do
	(name, cxtnp1) <- newTextureName -< cxtn
	(sdlGL1' :: GLIOF me -> ImmutaballM me) <- returnA -< liftIBIO . sdlGL1 (cxtnp1^.ibContext.ibSDLManagerHandle)
	() <- monadic -< sdlGL1' $ do
		GLEnable GL_BLEND ()
		GLBlendEquationSeparate GL_FUNC_ADD GL_FUNC_ADD ()
		GLBlendFuncSeparate GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO ()

		GLEnable GL_TEXTURE_2D ()
		GLActiveTexture GL_TEXTURE0 ()
		GLClientActiveTexture GL_TEXTURE0 ()
		GLBindTexture GL_TEXTURE_2D name ()
		let glImage = reverseRowsImage ((w, h), image)
		GLTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral w) (fromIntegral h) 0 GL_RGBA GL_UNSIGNED_BYTE glImage ()
		-- TODO: re-enable after mipmap is fixed.
		--when (cxtnp1^.ibNeverballrc.mipmap) $ do
		--	GLGenerateMipmap name ()

		GLTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT ()
		GLTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT ()
		-- TODO: re-enable after mipmap is fixed.
		{-
		when (      cxtnp1^.ibNeverballrc.mipmap) $ do
			GLTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR ()
			GLTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR_MIPMAP_LINEAR ()
		when (not $ cxtnp1^.ibNeverballrc.mipmap) $ do
		-}
		when True $ do
			GLTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR ()
			GLTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR ()

		GLTexParameterfv GL_TEXTURE_2D GL_TEXTURE_BORDER_COLOR [0.0, 0.0, 0.0, 0.0] ()

	returnA -< (name, cxtnp1)

-- | This also frees the texture, not just the name.
freeTexture :: Wire ImmutaballM (GLuint, IBStateContext) IBStateContext
freeTexture = proc (name, cxtn) -> do
	cxtnp1 <- freeTextureName -< (name, cxtn)
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

-- | This only frees allocated textures; it does not update the text texture ID
-- or mtrl texture ID cache.  To clear everything use 'freeAllTextures'.
freeTextures :: Wire ImmutaballM IBStateContext IBStateContext
freeTextures = proc cxtn -> do
	(musedFree, cxtnp1) <- requireGLAllocatedTextures -< cxtn
	needsFreeing <- monadic -< liftIBIO . flip Atomically id $ do
		usedFree0 <- readTVar musedFree
		let (used0, free0) = usedFree0
		let needsFreeing = used0
		let free1 = used0 `S.union` free0
		let used1 = S.empty
		let usedFree1 = (used1, free1)
		writeTVar musedFree (if' weManageAllTextures usedFree1 usedFree0)
		return needsFreeing
	cxtnp2 <- foldrA freeTexture -< (cxtnp1, needsFreeing)
	returnA -< cxtnp2
	where
		weManageAllTextures :: Bool
		weManageAllTextures = False

-- | You can use this to attach a resource using this to the SDLManager thread to
-- free all textures on exit.  The state context can be old so long as it still
-- has the same STM references.
--
-- This only frees allocated textures; it does not update the text texture ID
-- or mtrl texture ID cache.  To clear everything use 'ibFreeAllTextures'.
ibFreeTextures :: IBStateContext -> ImmutaballIOF IBStateContext
ibFreeTextures cxt0 = (\w -> fst <$> stepImmutaballWire w cxt0) $ freeTextures

uncacheMtrl :: Wire ImmutaballM (String, IBStateContext) IBStateContext
uncacheMtrl = proc (mtrl, cxtn) -> do
	(mglMtrlTextures, cxtnp1) <- requireGLMtrlTextures -< cxtn
	mname <- monadic -< liftIBIO . flip Atomically id $ do
		glMtrlTextures <- readTVar mglMtrlTextures
		let glMtrlTextures2 = M.delete mtrl glMtrlTextures
		writeTVar mglMtrlTextures glMtrlTextures2
		return $ snd . fst <$> M.lookup mtrl glMtrlTextures
	case mname of
		Nothing -> returnA -< cxtnp1
		Just name -> do
			cxtnp2 <- freeTexture -< (name, cxtnp1)
			returnA -< cxtnp2

clearMtrlCache :: Wire ImmutaballM IBStateContext IBStateContext
clearMtrlCache = proc cxtn -> do
	(mglMtrlTextures, cxtnp1) <- requireGLMtrlTextures -< cxtn
	glMtrlTextures <- monadic -< liftIBIO $ Atomically (readTVar mglMtrlTextures) id
	let (mtrls :: [String]) = M.keys glMtrlTextures
	foldrA uncacheMtrl -< (cxtnp1, mtrls)

freeAllTextures :: Wire ImmutaballM IBStateContext IBStateContext
freeAllTextures = proc cxtn -> do
	cxtnp1 <- clearMtrlCache -< cxtn
	cxtnp2 <- clearTextCache -< cxtnp1
	cxtnp3 <- freeTextures   -< cxtnp2
	returnA -< cxtnp3

ibFreeAllTextures :: IBStateContext -> ImmutaballIOF IBStateContext
ibFreeAllTextures cxt0 = (\w -> fst <$> stepImmutaballWire w cxt0) $ freeAllTextures

-- | Give it a material path, like ‘mtrl/invisible’.
--
-- TODO: implement more full support for mtrl textures.  For now just read the
-- base image file for the texture, inside ‘data/textures/mtrl/’.
cachingRenderMtrl :: Wire ImmutaballM (String, IBStateContext) (((WidthHeightI, GLuint), MtrlMeta), IBStateContext)
cachingRenderMtrl = proc (mtrl, cxtn) -> do
	(mglMtrlTextures, cxtnp1) <- requireGLMtrlTextures -< cxtn
	glMtrlTextures <- monadic -< liftIBIO $ Atomically (readTVar mglMtrlTextures) id
	case M.lookup mtrl glMtrlTextures of
		Just (((w, h), name), meta) -> do
			returnA -< ((((w, h), name), meta), cxtnp1)
		Nothing -> do
			-- Find (((w, h), image), meta).
			let texturesDir = cxtnp1^.ibContext.ibDirs.ibStaticDataDir </> "textures"
			let baseMtrlPath = texturesDir </> mtrl
			let mtrlMetaPath = baseMtrlPath
			let mtrlImageTryPaths = [baseMtrlPath ++ ".png", baseMtrlPath ++ ".jpg"]

			ammtrlMetaContents <- monadic -< liftIBIO . BasicIBIOF $ ReadText mtrlMetaPath id
			ammtrlEncodedImages <- monadic -< liftIBIO . BasicIBIOF $ forM mtrlImageTryPaths $ \mtrlImageTryPath -> ReadBytes mtrlImageTryPath id
			mmtrlMetaContents <- monadic -< liftIBIO $ Wait ammtrlMetaContents id
			mmtrlEncodedImages <- monadic -< liftIBIO . forM ammtrlEncodedImages $ \ammtrlEncodedImage -> Wait ammtrlEncodedImage id

			mtrlMetaContents <- monadic -< liftIBIO . (ThrowIO ||| pure) $ mmtrlMetaContents
			let (mtrlEncodedImagesFailures, mtrlEncodedImages) = partitionEithers mmtrlEncodedImages
			mtrlEncodedImage <- monadic -< liftIBIO $ case (mtrlEncodedImagesFailures, mtrlEncodedImages) of
				(errs@(err:_), []) -> do
					BasicIBIOF $ PutStrLn (printf "Error: cachingRenderMtrl %s: failed to read mtrl texture image: %s" mtrl (intercalate "\n" (map show errs))) ()
					ThrowIO err
				([], []) -> ThrowIO $ userError ("Error: cachingRenderMtrl: no errors or successes when reading the mtrl " ++ mtrl)
				(_, [encodedImage]) -> return encodedImage
				(_, (_encodedImages@(encodedImage:_more))) -> do
					BasicIBIOF $ PutStrLn (printf "Warning: cachingRenderMtrl %s: found multiple mtrl images; using the first" mtrl) ()
					return encodedImage
			let mmtrlJpEncodedImage = JP.decodeImage . BL.toStrict $ mtrlEncodedImage
			mtrlJpEncodedImage <- monadic -< liftIBIO . (ThrowIO . (\e -> userError (printf "Error: cachingRenderMtrl %s: failed to decode texture image!: %s" mtrl e)) ||| pure) $ mmtrlJpEncodedImage
			let mtrlJpImage = juicyPixelsDynamicImageToJPImage mtrlJpEncodedImage
			let (mtrlW, mtrlH) = join (***) fromIntegral (JP.imageWidth mtrlJpImage, JP.imageHeight mtrlJpImage)
			let mtrlImage = juicyPixelsImageToImage mtrlJpImage
			let mtrlImageGL = reverseRowsImage ((mtrlW, mtrlH), mtrlImage)
			let mtrlMeta = MtrlMeta
			let _unused = [mtrlMetaContents]

			let (((w, h), image), meta) = (((mtrlW, mtrlH), mtrlImageGL), mtrlMeta)

			-- Make the texture with (((w, h), image), meta).
			(name, cxtnp3) <- createTexture -< (((w, h), image), cxtnp1)
			-- Also see if somebody already cached our text while we were
			-- creating the texture.
			raceAlreadyCached <- monadic -< liftIBIO . flip Atomically id $ do
				glMtrlTextures2 <- readTVar mglMtrlTextures
				case M.lookup mtrl glMtrlTextures2 of
					Nothing -> do
						let glMtrlTextures3 = M.insert mtrl (((w, h), name), meta) glMtrlTextures2
						writeTVar mglMtrlTextures glMtrlTextures3
						return Nothing
					Just (((w2, h2), name2), meta2) -> do
						return $ Just (((w2, h2), name2), meta2)
			case raceAlreadyCached of
				Nothing -> do returnA -< ((((w, h), name), meta), cxtnp3)
				Just (((w2, h2), name2), meta2) -> do
					cxtnp4 <- freeTexture -< (name, cxtnp3)
					returnA -< ((((w2, h2), name2), meta2), cxtnp4)

-- | Convert a JuicyPixels image to our own RGBA pixel format.
juicyPixelsDynamicImageToJPImage :: JP.DynamicImage -> JP.Image JP.PixelRGBA8
juicyPixelsDynamicImageToJPImage (JP.ImageY8     img) = JP.promoteImage img
juicyPixelsDynamicImageToJPImage (JP.ImageY16    img) = juicyPixelsDynamicImageToJPImage . JP.ImageRGBA16 $ JP.promoteImage img
juicyPixelsDynamicImageToJPImage (JP.ImageY32    img) = flip JP.pixelMap img $ \pixel -> (\c -> JP.PixelRGBA8 c c c 255) $ (round $ (fromIntegral pixel / (4294967295  :: Double))*255)
juicyPixelsDynamicImageToJPImage (JP.ImageYF     img) = flip JP.pixelMap img $ \pixel -> (\c -> JP.PixelRGBA8 c c c 255) $ (round $ pixel*255)
juicyPixelsDynamicImageToJPImage (JP.ImageYA8    img) = JP.promoteImage img
juicyPixelsDynamicImageToJPImage (JP.ImageYA16   img) = juicyPixelsDynamicImageToJPImage . JP.ImageRGBA16 $ JP.promoteImage img
juicyPixelsDynamicImageToJPImage (JP.ImageRGB8   img) = JP.promoteImage img
juicyPixelsDynamicImageToJPImage (JP.ImageRGB16  img) = flip JP.pixelMap img $ \(JP.PixelRGB16 r g b) -> JP.PixelRGBA8 (round $ ((fromIntegral r  :: Double) / 65535.0)*255) (round $ ((fromIntegral g  :: Double) / 65535.0)*255) (round $ ((fromIntegral b  :: Double) / 65535.0)*255) 255
juicyPixelsDynamicImageToJPImage (JP.ImageRGBF   img) = flip JP.pixelMap img $ \(JP.PixelRGBF r g b) -> JP.PixelRGBA8 (round $ r*255) (round $ g*255) (round $ b*255) (255)
juicyPixelsDynamicImageToJPImage (JP.ImageRGBA8  img) = img
juicyPixelsDynamicImageToJPImage (JP.ImageRGBA16 img) = flip JP.pixelMap img $ \(JP.PixelRGBA16 r g b a) -> JP.PixelRGBA8 (round $ ((fromIntegral r  :: Double) / 65535.0)*255) (round $ ((fromIntegral g  :: Double) / 65535.0)*255) (round $ ((fromIntegral b  :: Double) / 65535.0)*255) (round $ ((fromIntegral a  :: Double) / 65535.0)*255)
juicyPixelsDynamicImageToJPImage (JP.ImageYCbCr8 img) = juicyPixelsDynamicImageToJPImage . JP.ImageRGB8  $ JP.convertImage img
juicyPixelsDynamicImageToJPImage (JP.ImageCMYK8  img) = juicyPixelsDynamicImageToJPImage . JP.ImageRGB8  $ JP.convertImage img
juicyPixelsDynamicImageToJPImage (JP.ImageCMYK16 img) = juicyPixelsDynamicImageToJPImage . JP.ImageRGB16 $ JP.convertImage img

juicyPixelsImageToImage :: JP.Image JP.PixelRGBA8 -> BS.ByteString
juicyPixelsImageToImage jpImage@(JP.Image w h _) = BL.toStrict . BB.toLazyByteString $ JP.pixelFold (\builder _ _ (JP.PixelRGBA8 r g b a) -> builder <> BB.word8 r <> BB.word8 g <> BB.word8 b <> BB.word8 a) mempty jpImage
	where
		_w', _h' :: Integer
		(_w', _h') = join (***) fromIntegral (w, h)

-- | If the context enables precaching mtrls, spawn a thread to precache them.
checkPrecacheMtrls :: Wire ImmutaballM IBStateContext IBStateContext
checkPrecacheMtrls = proc cxtn -> do
	--cxtnp1 <- if' (cxtn^.ibContext.ibStaticConfig.x'cfgPrecacheMtrls) precacheMtrls returnA -<< cxtn
	cxtnp1 <- replaceNow $ (proc (_cxt, doPrecacheMtrls) -> do
		returnA -< if' (not doPrecacheMtrls) (arr fst) $ proc (cxt2, _doPrecacheMtrls) -> do
			precacheMtrls -< cxt2
		) -< (cxtn, (cxtn^.ibContext.ibStaticConfig.x'cfgPrecacheMtrls))
	returnA -< cxtnp1

-- | Spawn a thread to scan for mtrls and render them to the cache.
precacheMtrls :: Wire ImmutaballM IBStateContext IBStateContext
precacheMtrls = proc cxtn -> do
	cxtnp1      <- requireVideo               -< cxtn
	(_, cxtnp2) <- requireGLTextureNames      -< cxtnp1
	(_, cxtnp3) <- requireGLTextTextures      -< cxtnp2
	(_, cxtnp4) <- requireGLMtrlTextures      -< cxtnp3
	(_, cxtnp5) <- requireGLAllocatedTextures -< cxtnp4

	--() <- monadic -< liftIBIO . JoinIBIOF . BasicIBIOF $ ForkIO (void $ precacheMtrlsIB cxtnp5) (pure ())
	() <- monadic -< liftIBIO $ forkIBIOF (void $ precacheMtrlsIB cxtnp5) (pure ())

	returnA -< cxtnp5

-- | The mtrl precaching thread.
--
-- Ensure the STM resources have been allocated before calling.
precacheMtrlsIB :: IBStateContext -> ImmutaballIOF IBStateContext
precacheMtrlsIB cxt0 = ((\w -> fst <$> stepImmutaballWire w cxt0)) $ precacheMtrlsDirect

precacheMtrlsDirect :: Wire ImmutaballM IBStateContext IBStateContext
precacheMtrlsDirect = proc cxtn -> do
	let texturesDir = cxtn^.ibContext.ibDirs.ibStaticDataDir </> "textures"
	let mtrlsDir = texturesDir </> "mtrl"
	amtrlsDirContents <- monadic -< liftIBIO . BasicIBIOF $ GetDirectoryContents mtrlsDir id
	mtrlsDirContents <- monadic -< liftIBIO $ Wait amtrlsDirContents id
	let mtrlsBase_ = flip filter mtrlsDirContents $ \path -> not (path `elem` [".", ".."]) && not (".png" `isSuffixOf` path) && not (".jpg" `isSuffixOf` path)
	let mtrlsBase = S.toList . S.fromList $ mtrlsBase_
	let mtrls = map ("mtrl" </>) mtrlsBase
	foldrA (proc (mtrlBase, cxt) -> snd <$> cachingRenderMtrl -< (mtrlBase, cxt)) -< (cxtn, mtrls)
