{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables #-}

module Immutaball.Share.ImmutaballIO.SDLIO
	(
		-- * DirectoryIO
		SDLIO,
		SDLIOF(..),
		runSDLIO,

		-- * Runners
		runSDLIOIO,

		-- * SDLIO aliases that apply the Fixed wrapper
		mkSDLInit,
		mkSDLPollEvent,
		mkSDLPollEventSync
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.Async
import qualified SDL.Event
import qualified SDL.Init

import Immutaball.Share.Utils

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
instance Functor SDLIOF where
	fmap :: (a -> b) -> (SDLIOF a -> SDLIOF b)
	fmap f (SDLWithInit subsystems sdlio) = SDLWithInit subsystems (f sdlio)
	fmap f (SDLPollEvent withMEvent)      = SDLPollEvent (f . withMEvent)
	fmap f (SDLPollEventSync withMEvent)  = SDLPollEventSync (f . withMEvent)

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

-- * Runners

runSDLIOIO :: SDLIOF (IO ()) -> IO ()
runSDLIOIO (SDLWithInit subsystems sdlioio) = do
	SDL.Init.initialize subsystems
	sdlioio
	SDL.Init.quit
runSDLIOIO (SDLPollEvent withMEvent) = withAsync SDL.Event.pollEvent withMEvent
runSDLIOIO (SDLPollEventSync withMEvent) = SDL.Event.pollEvent >>= withMEvent

-- * SDLIO aliases that apply the Fixed wrapper

mkSDLInit :: [SDL.Init.InitFlag] -> SDLIO -> SDLIO
mkSDLInit subsystems sdlio = Fixed $ SDLWithInit subsystems sdlio

mkSDLPollEvent :: (Async (Maybe SDL.Event.Event) -> SDLIO) -> SDLIO
mkSDLPollEvent withMEvent = Fixed $ SDLPollEvent withMEvent

mkSDLPollEventSync :: (Maybe SDL.Event.Event -> SDLIO) -> SDLIO
mkSDLPollEventSync withMEvent = Fixed $ SDLPollEventSync withMEvent
