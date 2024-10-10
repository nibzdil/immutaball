{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

-- | Some SDL functions that some functions can only be called in the OS thread
-- that set up video.
--
-- So I created a module that video setup and input event callers can route
-- their requests through, to ensure they are called from the same OS thread to
-- meet this requirement mentioned in the documentation.
--
-- Example:
-- > pollEvent :: MonadIO m => m (Maybe Event)
-- >
-- > Poll for currently pending events. You can only call this function in the OS thread that set the video mode.
module Immutaball.Share.SDLManager
	(
		-- * High level
		withSDLManager,
		SDLManagerHandle(..), sdlmh_done, sdlmh_doneReceived, sdlmh_commands,
		SDLManagerCommand(..),
		issueSDLCommand,

		-- * Low level
		initSDLManager,
		quitSDLManager,
		sdlManagerThread
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM
import Data.Functor

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.SDLManager.Types

-- TODO: reomve this debugging.  Basically I've written a lot without being
-- able to see anything, and I just want to test our GUI before I write more advanced OpenGL.
import System.IO.Unsafe (unsafePerformIO)
import Graphics.GL.Internal.Shared as GL
import Immutaball.Share.Utils
import Data.Bits
import Immutaball.Share.ImmutaballIO.GLIO
import SDL.Video
import SDL.Video.OpenGL

-- * High level

withSDLManager :: (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
withSDLManager withHandle =
	initSDLManager $ \sdlManagerHandle ->
	withHandle sdlManagerHandle <>>
	quitSDLManager sdlManagerHandle

-- Moved to .Types to fix Template Haskell erors:
-- SDLManagerHandle
-- SDLManagerCommand

issueSDLCommand :: SDLManagerHandle -> SDLManagerCommand -> me -> ImmutaballIOF me
issueSDLCommand sdlMgr cmd withUnit = Atomically (writeTChan (sdlMgr^.sdlmh_commands) cmd) (\() -> withUnit)

-- * Low level

-- | Manually start the lifetime of the SDLManager OS thread; the caller will
-- need to manage the lifetime.
initSDLManager :: (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
initSDLManager withSdlMgr =
	mkAtomically (newTVar False) $ \done ->
	mkAtomically (newTVar False) $ \doneReceived ->
	mkAtomically newTChan $ \commands ->
	let sdlMgr = SDLManagerHandle {
		_sdlmh_done         = done,
		_sdlmh_doneReceived = doneReceived,
		_sdlmh_commands     = commands
	}
	--in mkBIO . ForkOS (sdlManagerThread sdlMgr) $ withSdlMgr sdlMgr
	in mkBIO . ForkOS (sdlManagerThread sdlMgr Nothing) $ withSdlMgr sdlMgr

-- | Manually close the SDLManager thread low-level.  High-level
-- 'withSDLManager' automatically manages the lifetime.
quitSDLManager :: SDLManagerHandle -> ImmutaballIO
quitSDLManager sdlMgr =
	mkAtomically (do
		writeTVar (sdlMgr^.sdlmh_done) True) (const mempty) <>
	mkAtomically (do
		writeTChan (sdlMgr^.sdlmh_commands) QuitSDLManager) (const mempty) <>>
	mkAtomically (do
		readTVar (sdlMgr^.sdlmh_doneReceived) >>= check) (const mempty)

-- TODO: remove debugging once GL commands work.
{-# NOINLINE fff #-}
fff :: a -> a
fff =
	unsafePerformIO (do
		GL.glColor4f 0.3 0.7 0.3 1.0
		GL.glBegin GL.GL_QUADS
		GL.glVertex2f (-9.0) (-9.0)
		GL.glVertex2f (-9.0) ( 9.0)
		GL.glVertex2f ( 9.0) ( 9.0)
		GL.glVertex2f ( 9.0) (-9.0)
		GL.glEnd
		return id
	)

--sdlManagerThread :: SDLManagerHandle -> ImmutaballIO
--sdlManagerThread sdlMgr =
sdlManagerThread :: SDLManagerHandle -> Maybe Window -> ImmutaballIO
sdlManagerThread sdlMgr mwindow =
	fff .
	mkBIO . GLIO . GLClearColor 0.1 0.7 0.2 1.0 .
	mkBIO . GLIO . GLClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT) .
	maybe id (\window -> mkBIO . SDLIO . SDLGLSwapWindow (window)) mwindow .
	mkAtomically (
		readTVar (sdlMgr^.sdlmh_done) >>= \done ->
		(check done $> Left done) `orElse` (Right <$> readTChan (sdlMgr^.sdlmh_commands))
	) $ \doneOrCmd ->
	case doneOrCmd of
		--Left done -> if not done then sdlManagerThread sdlMgr else quit
		Left done -> if not done then sdlManagerThread sdlMgr mwindow else quit
		Right cmd -> case cmd of
			QuitSDLManager -> quit
			--NopSDLManager -> sdlManagerThread sdlMgr
			NopSDLManager -> sdlManagerThread sdlMgr mwindow
			--PollEvent to_ -> (mkBIO . SDLIO . SDLPollEventSync $ \mevent -> mkAtomically (writeTMVar to_ mevent) (\() -> mempty)) <>> sdlManagerThread sdlMgr
			PollEvent to_ -> (mkBIO . SDLIO . SDLPollEventSync $ \mevent -> mkAtomically (writeTMVar to_ mevent) (\() -> mempty)) <>> sdlManagerThread sdlMgr mwindow
			--WithWindow title cfg to_ -> mkBIO . SDLIO . SDLWithWindow title cfg $ \window -> mkAtomically (writeTMVar to_ window) $ \() -> sdlManagerThread sdlMgr
			WithWindow title cfg to_ -> mkBIO . SDLIO . SDLWithWindow title cfg $ \window -> mkAtomically (writeTMVar to_ window) $ \() -> sdlManagerThread sdlMgr (Just window)
			--WithGLContext window to_ -> mkBIO . SDLIO . SDLWithGLContext window $ \cxt    -> mkAtomically (writeTMVar to_ cxt)    $ \() -> sdlManagerThread sdlMgr
			WithGLContext window to_ -> mkBIO . SDLIO . SDLWithGLContext window $ \cxt    -> mkAtomically (writeTMVar to_ cxt)    $ \() -> sdlManagerThread sdlMgr mwindow
	where
		quit :: ImmutaballIO
		quit = mkAtomically (writeTVar (sdlMgr^.sdlmh_doneReceived) True) mempty <>> mkAtomically (writeTVar (sdlMgr^.sdlmh_done) True) mempty <>> mempty
