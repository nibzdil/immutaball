{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables, ExistentialQuantification #-}

module Immutaball.Share.ImmutaballIO.SDLIO
	(
		-- * DirectoryIO
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

		-- * SDLIO aliases that apply the Fixed wrapper
		mkSDLInit,
		mkSDLPollEvent,
		mkSDLPollEventSync,
		mkSDLWithWindow,
		mkSDLWithGLContext,
		mkSDLGLSwapWindow
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.Async
import qualified Data.Text as T
-- TODO: file bug report: glFinish is missing in the Core modules.
import qualified Graphics.GL.Internal.Shared (glFinish)
import qualified SDL.Event
import qualified SDL.Init
import qualified SDL.Video
import qualified SDL.Video.OpenGL

import Immutaball.Share.Utils

-- (mfix imports.)
import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import System.IO.Unsafe (unsafePerformIO)

-- * DirectoryIO

type SDLIO = Fixed SDLIOF
data SDLIOF me =
	  SDLWithInit [SDL.Init.InitFlag] me
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
instance Functor SDLIOF where
	fmap :: (a -> b) -> (SDLIOF a -> SDLIOF b)
	fmap f (SDLWithInit subsystems sdlio)       = SDLWithInit subsystems (f sdlio)
	fmap f (SDLPollEvent withMEvent)            = SDLPollEvent (f . withMEvent)
	fmap f (SDLPollEventSync withMEvent)        = SDLPollEventSync (f . withMEvent)
	fmap f (SDLWithWindow title cfg withWindow) = SDLWithWindow title cfg (f . withWindow)
	fmap f (SDLWithGLContext window withCxt)    = SDLWithGLContext window (f . withCxt)
	fmap f (SDLGLSwapWindow window withUnit)    = SDLGLSwapWindow window (f withUnit)

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
		y@( SDLWithInit _subsystems me)         -> putMVar mme me >> return y
		_y@(SDLPollEvent withMEvent)            -> return $ SDLPollEvent               ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withMEvent)
		_y@(SDLPollEventSync withMEvent)        -> return $ SDLPollEventSync           ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withMEvent)
		_y@(SDLWithWindow title cfg withWindow) -> return $ SDLWithWindow    title cfg ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withWindow)
		_y@(SDLWithGLContext window withCxt)    -> return $ SDLWithGLContext window    ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withCxt)
		y@( SDLGLSwapWindow _window me)         -> putMVar mme me >> return y

-- * Runners

runSDLIOIO :: SDLIOF (IO ()) -> IO ()
runSDLIOIO (SDLWithInit subsystems sdlioio) = do
	SDL.Init.initialize subsystems
	sdlioio
	SDL.Init.quit
runSDLIOIO (SDLPollEvent withMEvent) = withAsync SDL.Event.pollEvent withMEvent
runSDLIOIO (SDLPollEventSync withMEvent) = SDL.Event.pollEvent >>= withMEvent
runSDLIOIO (SDLWithWindow title cfg withWindow) = do
	window <- SDL.Video.createWindow title cfg
	withWindow window
	SDL.Video.destroyWindow window
runSDLIOIO (SDLWithGLContext window withCxt) = do
	cxt <- SDL.Video.OpenGL.glCreateContext window
	withCxt cxt
	Graphics.GL.Internal.Shared.glFinish
	SDL.Video.OpenGL.glDeleteContext cxt
runSDLIOIO (SDLGLSwapWindow window withUnit) = do
	SDL.Video.OpenGL.glSwapWindow window
	withUnit

-- * SDLIO aliases that apply the Fixed wrapper

mkSDLInit :: [SDL.Init.InitFlag] -> SDLIO -> SDLIO
mkSDLInit subsystems sdlio = Fixed $ SDLWithInit subsystems sdlio

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
