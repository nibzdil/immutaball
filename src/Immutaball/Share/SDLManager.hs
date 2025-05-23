{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
		withSDLManager',
		SDLManagerHandle(..), sdlmh_done, sdlmh_doneReceived, sdlmh_commands,
		SDLManagerCommand(..),
		issueSDLCommand,

		-- * Utils
		sdlGLSwapWindow,
		sdlGL,
		sdlGL1,
		sdlGLHomogeneous,
		sdlGL_,
		sdl,
		attachLifetime,
		sdlIBIO,

		-- * Low level
		initSDLManager,
		quitSDLManager,
		sdlManagerFreeResources,
		sdlManagerThread,
		sdlManagerThreadContinue
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Functor

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM
import qualified SDL.Init
import qualified SDL.Video

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.SDLManager.Types
import Immutaball.Share.Utils

-- * High level

-- | Create an SDL manager and manage its lifetime.
withSDLManager :: (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
withSDLManager = withSDLManager' False

-- | Adds a headless setting.
withSDLManager' :: Bool -> (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
withSDLManager' headless withHandle =
	initSDLManager headless $ \sdlManagerHandle ->
	withHandle sdlManagerHandle <>>
	quitSDLManager sdlManagerHandle

-- Moved to .Types to fix Template Haskell erors:
-- SDLManagerHandle
-- SDLManagerCommand

issueSDLCommand :: SDLManagerHandle -> SDLManagerCommand -> me -> ImmutaballIOF me
issueSDLCommand sdlMgr cmd withUnit = Atomically (writeTChan (sdlMgr^.sdlmh_commands) cmd) (\() -> withUnit)

-- * Utils

-- | Swap the window, but also wait for the swap to finish.
sdlGLSwapWindow :: SDLManagerHandle -> SDL.Video.Window -> me -> ImmutaballIOF me
sdlGLSwapWindow sdlMgr window withUnit =
	JoinIBIOF . JoinIBIOF .
	Atomically (newEmptyTMVar) $ \mdone ->
	issueSDLCommand sdlMgr (GLSwapWindow window mdone) $
	Atomically (takeTMVar mdone) $ \() ->
	withUnit

-- | Run an ordered GLIO sequence in the SDL manager thread.
--
-- This might be needed to avoid issues with multi-threaded SDL & OpenGL on
-- some platforms.
sdlGL :: SDLManagerHandle -> [GLIOFTo] -> ImmutaballIOF ()
sdlGL sdlMgr glioTos =
	JoinIBIOF . JoinIBIOF .
	Atomically (newEmptyTMVar) $ \hasDone ->
	issueSDLCommand sdlMgr (GLSequence glioTos hasDone) $
	Atomically (takeTMVar hasDone) id

-- | 'sdlGL' variant specialized to a single GLIO.
sdlGL1 :: SDLManagerHandle -> GLIOF me -> ImmutaballIOF me
sdlGL1 sdlMgr glio = unSingleton <$> sdlGLHomogeneous sdlMgr [glio]
	where
		unSingleton [me] = me
		unSingleton _    = error "Internal error: sdlGL1 expected a single result."

-- | Run an ordered GLIO sequence in the SDL manager thread.
--
-- This might be needed to avoid issues with multi-threaded SDL & OpenGL on
-- some platforms.
--
-- This version handles reading the results but only supports it when the types
-- are all the same.
sdlGLHomogeneous :: forall me. SDLManagerHandle -> [GLIOF me] -> ImmutaballIOF [me]
sdlGLHomogeneous sdlMgr glios = foldr reduce reduction0 glios []
	where
		reduce :: GLIOF me -> ([(GLIOF me, TMVar me)] -> ImmutaballIOF [me]) -> ([(GLIOF me, TMVar me)] -> ImmutaballIOF [me])
		reduce glio withGlioTos = \glioTos -> JoinIBIOF . Atomically (newEmptyTMVar) $ \to_ -> withGlioTos ((glio, to_):glioTos)
		reduction0 :: [(GLIOF me, TMVar me)] -> ImmutaballIOF [me]
		reduction0 reversedGlioTos =
			let glioTos  = reverse reversedGlioTos in
			let glioTos' = map GLIOFTo $ glioTos in
			let tos      = map snd glioTos in
			JoinIBIOF . JoinIBIOF .
			Atomically (newEmptyTMVar) $ \hasDone ->
			issueSDLCommand sdlMgr (GLSequence glioTos' hasDone) .
			JoinIBIOF .
			Atomically (takeTMVar hasDone) $ \() ->
			foldr readReduce readReduction0 $ tos

		readReduce :: TMVar me -> ImmutaballIOF [me] -> ImmutaballIOF [me]
		readReduce to_ then_ =
			JoinIBIOF . Atomically (takeTMVar to_) $ \me -> (me:) <$> then_
		readReduction0 :: ImmutaballIOF [me]
		readReduction0 = PureIBIOF []

-- | An 'sdlGL' variant that only handles valueless sequences.
--
-- Additionally it only waits for the signal that all are done.  (The caller
-- can fork off a GL call if desired while it does other things in the
-- background; only the SDL thread would be blocked here.)
sdlGL_ :: SDLManagerHandle -> [GLIOF ()] -> ImmutaballIOF ()
sdlGL_ sdlMgr glios =
	JoinIBIOF . JoinIBIOF .
	Atomically (newEmptyTMVar) $ \hasDone ->
	issueSDLCommand sdlMgr (GLSequenceValueless glios hasDone) $
	Atomically (takeTMVar hasDone) id

-- | Run an general SDL command in the SDL Manager thread.
sdl :: SDLManagerHandle -> SDLIOF me -> ImmutaballIOF me
sdl sdlMgr sdlio =
	JoinIBIOF . JoinIBIOF .
	Atomically (newEmptyTMVar) $ \to_ ->
	issueSDLCommand sdlMgr (GenSDL sdlio to_) $
	Atomically (takeTMVar to_) id

-- | Attach a resource to the lifetime of the SDL Manager thread.
attachLifetime :: SDLManagerHandle -> ImmutaballIOF resource -> (resource -> ImmutaballIOF ()) -> TMVar resource -> me -> ImmutaballIOF me
attachLifetime sdlMgr init_ free to_ withUnit = issueSDLCommand sdlMgr (AttachLifetime (ResourceAllocationTo ((init_, free), to_))) withUnit

-- | Run an general ImmutaballIOF command in the SDL Manager thread.
--
-- Note that the command blocks the SDL Manager thread when it handles it; you
-- can fork if this is not desired.
sdlIBIO :: SDLManagerHandle -> ImmutaballIOF me -> ImmutaballIOF me
sdlIBIO sdlMgr ibio =
	JoinIBIOF . JoinIBIOF .
	Atomically (newEmptyTMVar) $ \to_ ->
	issueSDLCommand sdlMgr (GenIBIO ibio to_) $
	Atomically (takeTMVar to_) id

-- * Low level

-- | Manually start the lifetime of the SDLManager OS thread; the caller will
-- need to manage the lifetime.
initSDLManager :: Bool -> (SDLManagerHandle -> ImmutaballIO) -> ImmutaballIO
initSDLManager headless withSdlMgr =
	mkAtomically (newTVar False) $ \done ->
	mkAtomically (newTVar False) $ \doneReceived ->
	mkAtomically newTChan $ \commands ->
	mkAtomically (newTVar []) $ \finalizers ->
	let sdlMgr = SDLManagerHandle {
		_sdlmh_done         = done,
		_sdlmh_doneReceived = doneReceived,
		_sdlmh_commands     = commands,
		_sdlmh_finalizers   = finalizers
	}
	in mkBIO . ForkOS (sdlManagerThread headless sdlMgr) $ withSdlMgr sdlMgr

-- | Manually close the SDLManager thread low-level.  High-level
-- 'withSDLManager' automatically manages the lifetime.
quitSDLManager :: SDLManagerHandle -> ImmutaballIO
quitSDLManager sdlMgr =
	sdlManagerFreeResources sdlMgr $
	mkAtomically (do
		writeTVar (sdlMgr^.sdlmh_done) True) (const mempty) <>
	mkAtomically (do
		writeTChan (sdlMgr^.sdlmh_commands) QuitSDLManager) (const mempty) <>>
	mkAtomically (do
		readTVar (sdlMgr^.sdlmh_doneReceived) >>= check) (const mempty)

sdlManagerFreeResources :: SDLManagerHandle -> ImmutaballIO -> ImmutaballIO
sdlManagerFreeResources sdlMgr then_ =
	-- Take the first finalizer.
	mkAtomically (do
		finalizers <- readTVar (sdlMgr^.sdlmh_finalizers)
		let (mfinalizer, finalizers') = case finalizers of
			[]               -> (Nothing, [])
			(finalizer:rest) -> (Just finalizer, rest)
		writeTVar (sdlMgr^.sdlmh_finalizers) finalizers'
		return mfinalizer
	) $ \mfinalizer ->
	-- Run it, if the resource is still there.
	case mfinalizer of
		Nothing        -> then_
		Just _finalizer@(ResourceAllocationTo ((_init, free), to_)) ->
			mkAtomically (tryTakeTMVar to_) $ \mresource ->
			case mresource of
				Nothing       -> then_
				Just resource ->
					Fixed $ free resource >>= \() -> getFixed $ sdlManagerFreeResources sdlMgr then_

-- | The thread with initialization.
sdlManagerThread :: Bool -> SDLManagerHandle -> ImmutaballIO
sdlManagerThread headless sdlMgr =
	let initFlags = if' headless [] [SDL.Init.InitVideo, SDL.Init.InitAudio] ++ [SDL.Init.InitJoystick] in
	mkBIO . SDLIO . SDLWithInit initFlags .
	mkBIO . SDLIO . SDLWithTTFInit $
	sdlManagerThreadContinue sdlMgr

-- | The thread after initialization.
sdlManagerThreadContinue :: SDLManagerHandle -> ImmutaballIO
sdlManagerThreadContinue sdlMgr =
	mkAtomically (
		readTVar (sdlMgr^.sdlmh_done) >>= \done ->
		(check done $> Left done) `orElse` (Right <$> readTChan (sdlMgr^.sdlmh_commands))
	) $ \doneOrCmd ->
	case doneOrCmd of
		Left done -> if not done then sdlManagerThreadContinue sdlMgr else quit
		Right cmd -> case cmd of
			QuitSDLManager -> quit
			NopSDLManager -> sdlManagerThreadContinue sdlMgr
			PollEvent to_ -> (mkBIO . SDLIO . SDLPollEventSync $ \mevent -> mkAtomically (writeTMVar to_ mevent) (\() -> mempty)) <>> sdlManagerThreadContinue sdlMgr
			PollEvents to_ -> (mkBIO . SDLIO . SDLPollEventsSync $ \events -> mkAtomically (writeTMVar to_ events) (\() -> mempty)) <>> sdlManagerThreadContinue sdlMgr
			WithWindow title cfg      to_ -> mkBIO . SDLIO . SDLWithWindow title cfg $   \window -> mkAtomically (writeTMVar to_ window) $ \() -> sdlManagerThreadContinue sdlMgr
			WithGLContext window      to_ -> mkBIO . SDLIO . SDLWithGLContext window $   \cxt    -> mkAtomically (writeTMVar to_ cxt)    $ \() -> sdlManagerThreadContinue sdlMgr
			GLSwapWindow window       to_ -> mkBIO . SDLIO . SDLGLSwapWindow window  $              mkAtomically (writeTMVar to_ ())     $ \() -> sdlManagerThreadContinue sdlMgr
			--GLSequence glios          tos -> Fixed $ (BasicIBIOF $ GLIO glio)        >>= \me     ->   Atomically (writeTMVar to_ me)     $ \() -> sdlManagerThreadContinue sdlMgr
			--GLSequence glios          tos -> Fixed $ foldr (\(glio, to_) then_ -> (BasicIBIOF . GLIO $ glio) >>= \me -> Atomically (writeTMVar to_ me) $ \() -> Fixed then_) (getFixed $ sdlManagerThreadContinue sdlMgr) (zip glios tos)
			GLSequence glioTos   hasDone_ -> Fixed $ foldr (\(GLIOFTo (glio, to_)) then_ -> (BasicIBIOF . GLIO $ glio) >>= \me -> Atomically (writeTMVar to_ me) $ \() -> Fixed then_) (Atomically (writeTMVar hasDone_ ()) $ \() -> sdlManagerThreadContinue sdlMgr) glioTos
			AttachLifetime a@(ResourceAllocationTo ((init_, _free), to_)) ->
				if waitForResourceInitializers
					then
						Fixed $ Atomically (modifyTVar (sdlMgr^.sdlmh_finalizers) (a:)) id >>= \() -> init_ >>= \resource -> Atomically (writeTMVar to_ resource) id >>= \() -> getFixed $ sdlManagerThreadContinue sdlMgr
					else
						Fixed $ Atomically (modifyTVar (sdlMgr^.sdlmh_finalizers) (a:)) id >>= \() -> forkBoundIBIOF (init_ >>= \resource -> Atomically (writeTMVar to_ resource) id >>= \() -> mempty) . getFixed $ sdlManagerThreadContinue sdlMgr
			GLSequenceValueless glios to_ -> Fixed $ foldr (\glio then_ -> (BasicIBIOF . GLIO $ glio) >>= \() -> then_) (Atomically (writeTMVar to_ ()) $ \() -> sdlManagerThreadContinue sdlMgr) glios
			GenSDL sdlio              to_ -> Fixed $ (BasicIBIOF . SDLIO $ sdlio) >>= \me -> Atomically (writeTMVar to_ me) $ \() -> sdlManagerThreadContinue sdlMgr
			GenIBIO ibio              to_ -> Fixed $ ibio >>= \me -> Atomically (writeTMVar to_ me) $ \() -> sdlManagerThreadContinue sdlMgr
	where
		quit :: ImmutaballIO
		quit = mkAtomically (writeTVar (sdlMgr^.sdlmh_doneReceived) True) mempty <>> mkAtomically (writeTVar (sdlMgr^.sdlmh_done) True) mempty <>> mempty
		waitForResourceInitializers :: Bool
		waitForResourceInitializers = False
