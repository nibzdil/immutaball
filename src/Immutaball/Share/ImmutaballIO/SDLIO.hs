{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables, ExistentialQuantification #-}

module Immutaball.Share.ImmutaballIO.SDLIO
	(
		-- * SDLIO
		SDLIO,
		SDLIOF(..),
		runSDLIO,

		-- * mfix
		FixSDLIOException(..),
		fixSDLIOExceptionToException,
		fixSDLIOExceptionFromException,
		PrematureEvaluationFixSDLIOException(..),
		EmptyFixSDLIOException(..),
		fixSDLIOF,
		unsafeFixSDLIOFTo,

		-- * Runners
		runSDLIOIO,
		hsdlttfRender,

		-- * SDLIO aliases that apply the Fixed wrapper
		mkSDLWithInit,
		mkSDLWithTTFInit,
		mkSDLPollEvent,
		mkSDLPollEventSync,
		mkSDLWithWindow,
		mkSDLWithGLContext,
		mkSDLGLSwapWindow,
		mkSDLTTFLoad,
		mkSDLTTFRender,
		mkSDLTTFRenderSync
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Control.Concurrent.Async
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Graphics.GL.Core45
import qualified SDL.Event
import qualified SDL.Font
import qualified SDL.Init
import qualified SDL.Raw.Enum
import qualified SDL.Raw.Types
import qualified SDL.Vect
import qualified SDL.Video
import qualified SDL.Video.OpenGL
import qualified SDL.Video.Renderer

import Immutaball.Share.Utils

-- (mfix imports.)
import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import System.IO.Unsafe (unsafePerformIO)

-- * SDLIO

type SDLWidthHeight = (Integer, Integer)

type SDLIO = Fixed SDLIOF
data SDLIOF me =
	  SDLWithInit [SDL.Init.InitFlag] me
	| SDLWithTTFInit me
	-- | WARNING: do not call directly to avoid undefined behavior, but only
	-- with OS thread management compliant with the requirements of
	-- 'SDL.Event.pollEvent':
	-- > You can only call this function in the OS thread that set the video mode.
	-- SDLManager can handle this.
	| SDLPollEvent (Async (Maybe SDL.Event.Event) -> me)
	| SDLPollEventSync (Maybe SDL.Event.Event -> me)
	-- | Automatically handles destruction after lifetime.
	| SDLWithWindow T.Text SDL.Video.WindowConfig (SDL.Video.Window -> me)
	-- _| SDLDestroyWindow SDL.Video.Window  -- We can already manage the lifetime with WithCreate.
	-- | Automatically calls 'glMakeCurrent'.  NOTE: automatically calls
	-- 'glFinish' upon destruction.
	| SDLWithGLContext SDL.Video.Window (SDL.Video.OpenGL.GLContext -> me)
	-- | SDLGLMakeCurrent SDL.Video.Window SDL.Video.OpenGL.GLContext  -- We can automatically call this.
	-- -- _| See notes on 'glDeleleteContext' and 'glFinish' before using.
	-- _| SDLGLDeleteContext SDL.Video.OpenGL.GLContext
	| SDLGLSwapWindow SDL.Video.Window me
	| SDLTTFLoad FilePath SDL.Font.PointSize (SDL.Font.Font -> me)
	-- | Tight RGBA encoding.  Caching recommended.
	| SDLTTFRender SDL.Font.Font T.Text (Async (SDLWidthHeight, BL.ByteString) -> me)
	| SDLTTFRenderSync SDL.Font.Font T.Text ((SDLWidthHeight, BL.ByteString) -> me)
instance Functor SDLIOF where
	fmap :: (a -> b) -> (SDLIOF a -> SDLIOF b)
	fmap f (SDLWithInit subsystems sdlio)         = SDLWithInit subsystems (f sdlio)
	fmap f (SDLWithTTFInit sdlio)                 = SDLWithTTFInit (f sdlio)
	fmap f (SDLPollEvent withMEvent)              = SDLPollEvent (f . withMEvent)
	fmap f (SDLPollEventSync withMEvent)          = SDLPollEventSync (f . withMEvent)
	fmap f (SDLWithWindow title cfg withWindow  ) = SDLWithWindow title cfg (f . withWindow)
	fmap f (SDLWithGLContext window withCxt)      = SDLWithGLContext window (f . withCxt)
	fmap f (SDLGLSwapWindow window withUnit)      = SDLGLSwapWindow window (f withUnit)
	fmap f (SDLTTFLoad path size withFont)        = SDLTTFLoad path size (f . withFont)
	fmap f (SDLTTFRender font text withImage)     = SDLTTFRender font text (f . withImage)
	fmap f (SDLTTFRenderSync font text withImage) = SDLTTFRenderSync font text (f . withImage)

runSDLIO :: SDLIO -> IO ()
runSDLIO sdlio = cata runSDLIOIO sdlio

-- TODO: revisit:
{-
instance Foldable SDLIOF where
	foldr :: (a -> b -> b) -> b -> SDLIOF a -> b
	foldr reduce reduction0 (SDLWithInit _subsystems sdlio) = reduce sdlio reduction0
instance Traversable SDLIOF where
	traverse :: Applicative f => (a -> f b) -> SDLIOF a -> f (SDLIOF b)
	traverse traversal (SDLWithInit subsystems sdlio) = pure SDLWithInit <*> pure subsystems <*> traversal sdlio
-}

-- * mfix

data FixSDLIOException = forall e. Exception e => FixSDLIOException e
instance Show FixSDLIOException where
	show (FixSDLIOException e) = show e
instance Exception FixSDLIOException
fixSDLIOExceptionToException :: Exception e => e -> SomeException
fixSDLIOExceptionToException = toException . FixSDLIOException
fixSDLIOExceptionFromException :: Exception e => SomeException -> Maybe e
fixSDLIOExceptionFromException x = do
	FixSDLIOException a <- fromException x
	cast a

data PrematureEvaluationFixSDLIOException = PrematureEvaluationFixSDLIOException
	deriving (Show)
instance Exception PrematureEvaluationFixSDLIOException where
	toException = fixSDLIOExceptionToException
	fromException = fixSDLIOExceptionFromException

data EmptyFixSDLIOException = EmptyFixSDLIOException
	deriving (Show)
instance Exception EmptyFixSDLIOException where
	toException = fixSDLIOExceptionToException
	fromException = fixSDLIOExceptionFromException

--    mfix f = mfix f >>= f
-- => mfix f = join $ f <$> mfix f
-- Incorrect: runs f twice.
	--x -> f undefined >>= mfix f
{-
fixSDLIOF :: (me -> SDLIOF me) -> SDLIOF me
fixSDLIOF f = case f (error "Error: fixSDLIOF: premature evaluation of result before we could start it!") of
	x -> joinSDLIOF $ f <$> x
-}
-- Do it like fixIO and fixST (see also their notes; it's a little tricky).
-- Use a lazily read MVar.
fixSDLIOF :: (me -> SDLIOF me) -> SDLIOF me
fixSDLIOF f = unsafePerformIO $ do
	mme <- newEmptyMVar
	return $ unsafeFixSDLIOFTo mme f

-- | Helper for fixSDLIOF.
unsafeFixSDLIOFTo :: MVar me -> (me -> SDLIOF me) -> SDLIOF me
unsafeFixSDLIOFTo mme f = unsafePerformIO $ do
	me_ <- unsafeDupableInterleaveIO (readMVar mme `catch` \BlockedIndefinitelyOnMVar -> throwIO PrematureEvaluationFixSDLIOException)
	case f me_ of
		y@( SDLWithInit _subsystems me)           -> putMVar mme me >> return y
		y@( SDLWithTTFInit me)                    -> putMVar mme me >> return y
		_y@(SDLPollEvent withMEvent)              -> return $ SDLPollEvent               ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withMEvent)
		_y@(SDLPollEventSync withMEvent)          -> return $ SDLPollEventSync           ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withMEvent)
		_y@(SDLWithWindow title cfg withWindow)   -> return $ SDLWithWindow    title cfg ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withWindow)
		_y@(SDLWithGLContext window withCxt)      -> return $ SDLWithGLContext window    ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withCxt)
		y@( SDLGLSwapWindow _window me)           -> putMVar mme me >> return y
		_y@(SDLTTFLoad path size withFont)        -> return $ SDLTTFLoad       path size ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withFont)
		_y@(SDLTTFRender font text withImage)     -> return $ SDLTTFRender     font text ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withImage)
		_y@(SDLTTFRenderSync font text withImage) -> return $ SDLTTFRenderSync font text ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withImage)

-- * Runners

runSDLIOIO :: SDLIOF (IO ()) -> IO ()
runSDLIOIO (SDLWithInit subsystems sdlioio) = do
	SDL.Init.initialize subsystems
	sdlioio
	SDL.Init.quit
runSDLIOIO (SDLWithTTFInit sdlioio) = do
	SDL.Font.initialize
	sdlioio
	SDL.Font.quit
runSDLIOIO (SDLPollEvent withMEvent) = withAsync SDL.Event.pollEvent withMEvent
runSDLIOIO (SDLPollEventSync withMEvent) = SDL.Event.pollEvent >>= withMEvent
runSDLIOIO (SDLWithWindow title cfg withWindow) = do
	window <- SDL.Video.createWindow title cfg
	withWindow window
	SDL.Video.destroyWindow window
runSDLIOIO (SDLWithGLContext window withCxt) = do
	cxt <- SDL.Video.OpenGL.glCreateContext window
	withCxt cxt
	glFinish
	SDL.Video.OpenGL.glDeleteContext cxt
runSDLIOIO (SDLGLSwapWindow window withUnit) = do
	SDL.Video.OpenGL.glSwapWindow window
	withUnit
runSDLIOIO (SDLTTFLoad path size withFont) = do
	font <- SDL.Font.load path size
	withFont font
runSDLIOIO (SDLTTFRender font text withImage) = withAsync (hsdlttfRender font text) withImage
runSDLIOIO (SDLTTFRenderSync font text withImage) = hsdlttfRender font text >>= withImage

-- | Render to a tight RGBA image.
--
-- We need some low-level C-like processing to interface with the SDL
-- libraries.
hsdlttfRender :: SDL.Font.Font -> T.Text -> IO (SDLWidthHeight, BL.ByteString)
hsdlttfRender font text = do
	let maxColorComponent = 255  :: Word8
	let mcc = maxColorComponent
	fmt0Surface <- SDL.Font.blended font (SDL.Vect.V4 mcc mcc mcc mcc) text

	-- Get a couple fields from fmt0Surface's pixel format.
	(fmt0SurfaceFmtFmt, fmt0SurfaceFmtPalette) <- do
		let (SDL.Video.Renderer.Surface fmt0SurfaceSurfacePtr _) = fmt0Surface
		(SDL.Raw.Types.Surface fmt0SurfacePixelFormatPtr _w0' _h0' _pixels0' _userdata0 _cliprect0 _refcount0) <- peek fmt0SurfaceSurfacePtr
		(SDL.Raw.Types.PixelFormat {SDL.Raw.Types.pixelFormatFormat = fmt0SurfaceFmtFmt, SDL.Raw.Types.pixelFormatPalette = fmt0SurfaceFmtPalette}) <- peek fmt0SurfacePixelFormatPtr
		return (fmt0SurfaceFmtFmt, fmt0SurfaceFmtPalette)

	let
		tightRGBA :: SDL.Raw.Types.PixelFormat
		tightRGBA = SDL.Raw.Types.PixelFormat {
			-- The documentation doesn't say what this field is.  I'll just copy it.
			SDL.Raw.Types.pixelFormatFormat        = fmt0SurfaceFmtFmt :: Word32,
			-- I may as well copy this field too.
			SDL.Raw.Types.pixelFormatPalette       = fmt0SurfaceFmtPalette :: (Ptr SDL.Raw.Types.Palette),
			SDL.Raw.Types.pixelFormatBitsPerPixel  = 32  :: Word8,
			SDL.Raw.Types.pixelFormatBytesPerPixel = 4  :: Word8,
			SDL.Raw.Types.pixelFormatRMask         = if' (SDL.Raw.Enum.SDL_BYTEORDER == SDL.Raw.Enum.SDL_BIG_ENDIAN) 0xFF000000 0x000000FF  :: Word32,
			SDL.Raw.Types.pixelFormatGMask         = if' (SDL.Raw.Enum.SDL_BYTEORDER == SDL.Raw.Enum.SDL_BIG_ENDIAN) 0x00FF0000 0x0000FF00  :: Word32,
			SDL.Raw.Types.pixelFormatBMask         = if' (SDL.Raw.Enum.SDL_BYTEORDER == SDL.Raw.Enum.SDL_BIG_ENDIAN) 0x0000FF00 0x00FF0000  :: Word32,
			SDL.Raw.Types.pixelFormatAMask         = if' (SDL.Raw.Enum.SDL_BYTEORDER == SDL.Raw.Enum.SDL_BIG_ENDIAN) 0x000000FF 0xFF000000  :: Word32
		}
	tightRGBAFmtFPtr <- mallocForeignPtrArray 1
	fmt1Surface <- withForeignPtr tightRGBAFmtFPtr $ \tightRGBAFmtPtr -> do
		poke tightRGBAFmtPtr tightRGBA
		let
			tightRGBA' :: SDL.Video.Renderer.SurfacePixelFormat
			tightRGBA' = SDL.Video.Renderer.SurfacePixelFormat tightRGBAFmtPtr
		fmt1Surface <- SDL.Video.Renderer.convertSurface fmt0Surface tightRGBA'
		return fmt1Surface
	--SDL.Video.Renderer.freeSurface fmt0Surface  -- We copied a pointer from fmt0; hold on until we free fmt1 just in case.
	let (SDL.Video.Renderer.Surface rawSurfacePtr _) = fmt1Surface
	(SDL.Raw.Types.Surface _fmt w' h' pixels' _userdata _cliprect _refcount) <- peek rawSurfacePtr
	let (w, h) = (fromIntegral w', fromIntegral h')
	w `seq` h `seq` pixels' `seq` return ()
	pixelsBs <- BS.packCStringLen $ (castPtr pixels', fromIntegral $ 4*w*h)
	let pixelsBl = BL.fromStrict pixelsBs
	let pixels = pixelsBl `seq` pixelsBl
	let result = w `seq` h `seq` pixels `seq` ((w, h), pixels)
	result `seq` return ()
	SDL.Video.Renderer.freeSurface fmt0Surface
	SDL.Video.Renderer.freeSurface fmt1Surface
	return result

-- * SDLIO aliases that apply the Fixed wrapper

mkSDLWithInit :: [SDL.Init.InitFlag] -> SDLIO -> SDLIO
mkSDLWithInit subsystems sdlio = Fixed $ SDLWithInit subsystems sdlio

mkSDLWithTTFInit :: SDLIO -> SDLIO
mkSDLWithTTFInit sdlio = Fixed $ SDLWithTTFInit sdlio

mkSDLPollEvent :: (Async (Maybe SDL.Event.Event) -> SDLIO) -> SDLIO
mkSDLPollEvent withMEvent = Fixed $ SDLPollEvent withMEvent

mkSDLPollEventSync :: (Maybe SDL.Event.Event -> SDLIO) -> SDLIO
mkSDLPollEventSync withMEvent = Fixed $ SDLPollEventSync withMEvent

mkSDLWithWindow :: T.Text -> SDL.Video.WindowConfig -> (SDL.Video.Window -> SDLIO) -> SDLIO
mkSDLWithWindow title cfg withWindow = Fixed $ SDLWithWindow title cfg withWindow

mkSDLWithGLContext :: SDL.Video.Window -> (SDL.Video.OpenGL.GLContext -> SDLIO) -> SDLIO
mkSDLWithGLContext window withCxt = Fixed $ SDLWithGLContext window withCxt

mkSDLGLSwapWindow :: SDL.Video.Window -> SDLIO -> SDLIO
mkSDLGLSwapWindow window withUnit = Fixed $ SDLGLSwapWindow window withUnit

mkSDLTTFLoad :: FilePath -> SDL.Font.PointSize -> (SDL.Font.Font -> SDLIO) -> SDLIO
mkSDLTTFLoad path size withFont = Fixed $ SDLTTFLoad path size withFont

mkSDLTTFRender :: SDL.Font.Font -> T.Text -> (Async (SDLWidthHeight, BL.ByteString) -> SDLIO) -> SDLIO
mkSDLTTFRender font text withImage = Fixed $ SDLTTFRender font text withImage

mkSDLTTFRenderSync :: SDL.Font.Font -> T.Text -> ((SDLWidthHeight, BL.ByteString) -> SDLIO) -> SDLIO
mkSDLTTFRenderSync font text withImage = Fixed $ SDLTTFRenderSync font text withImage
