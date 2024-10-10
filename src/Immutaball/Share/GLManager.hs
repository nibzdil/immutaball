{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

-- | A thread that can order GL commands in sequence.
--
-- When GL commands need to be in order to be correct, this thread can be
-- useful for this end.
module Immutaball.Share.GLManager
	(
		-- * High level
		withGLManager,
		GLManagerHandle(..), glmh_done, glmh_doneReceived, glmh_commands,
		GLManagerCommand(..),
		issueGLCommand,
		glQueueValueless,

		-- * Low level
		initGLManager,
		glForkIBIO,
		quitGLManager,
		glManagerThread
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM
import Data.Functor

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.GLManager.Types

-- * High level

withGLManager :: (GLManagerHandle -> ImmutaballIO) -> ImmutaballIO
withGLManager withHandle =
	initGLManager $ \glManagerHandle ->
	withHandle glManagerHandle <>>
	quitGLManager glManagerHandle

-- Moved to .Types to fix Template Haskell erors:
-- GLManagerHandle
-- GLManagerCommand

issueGLCommand :: GLManagerHandle -> GLManagerCommand -> me -> ImmutaballIOF me
issueGLCommand glMgr cmd withUnit = Atomically (writeTChan (glMgr^.glmh_commands) cmd) (\() -> withUnit)

glQueueValueless :: GLManagerHandle -> [GLIOF ()] -> me -> ImmutaballIOF me
glQueueValueless glMgr orderedGLCommands withUnit =
	issueGLCommand glMgr (GLQueueValueless orderedGLCommands) withUnit

-- * Low level

-- | Manually start the lifetime of the GLManager OS thread; the caller will
-- need to manage the lifetime.
initGLManager :: (GLManagerHandle -> ImmutaballIO) -> ImmutaballIO
initGLManager withGLMgr =
	mkAtomically (newTVar False) $ \done ->
	mkAtomically (newTVar False) $ \doneReceived ->
	mkAtomically newTChan $ \commands ->
	let glMgr = GLManagerHandle {
		_glmh_done         = done,
		_glmh_doneReceived = doneReceived,
		_glmh_commands     = commands
	}
	in glForkIBIO (glManagerThread glMgr) $ withGLMgr glMgr

-- | glForkIBIO: The return value is better associated with the callback rather than the thread, so flip.
glForkIBIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
glForkIBIO = flip mkAndIBIO

-- | Manually close the GLManager thread low-level.  High-level
-- 'withGLManager' automatically manages the lifetime.
quitGLManager :: GLManagerHandle -> ImmutaballIO
quitGLManager glMgr =
	mkAtomically (do
		writeTVar (glMgr^.glmh_done) True) (const mempty) <>
	mkAtomically (do
		writeTChan (glMgr^.glmh_commands) QuitGLManager) (const mempty) <>>
	mkAtomically (do
		readTVar (glMgr^.glmh_doneReceived) >>= check) (const mempty)

glManagerThread :: GLManagerHandle -> ImmutaballIO
glManagerThread glMgr =
	mkAtomically (
		readTVar (glMgr^.glmh_done) >>= \done ->
		(check done $> Left done) `orElse` (Right <$> readTChan (glMgr^.glmh_commands))
	) $ \doneOrCmd ->
	case doneOrCmd of
		Left done -> if not done then glManagerThread glMgr else quit
		Right cmd -> case cmd of
			QuitGLManager -> quit
			NopGLManager -> glManagerThread glMgr
			GLQueueValueless orderedGLCommands -> runOrderedGLValueless orderedGLCommands (glManagerThread glMgr)
	where
		quit :: ImmutaballIO
		quit = mkAtomically (writeTVar (glMgr^.glmh_doneReceived) True) mempty <>> mkAtomically (writeTVar (glMgr^.glmh_done) True) mempty <>> mempty

runOrderedGLValueless :: [GLIOF ()] -> ImmutaballIO -> ImmutaballIO
runOrderedGLValueless orderedGLCommands withUnit =
	case orderedGLCommands of
		[] -> withUnit
		(orderedGLCommand:then_) -> mkBIO . GLIO $ runOrderedGLValueless then_ withUnit <$ orderedGLCommand
