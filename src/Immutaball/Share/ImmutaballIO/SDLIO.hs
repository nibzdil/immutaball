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

		-- * DirectoryIO aliases that apply the Fixed wrapper
		mkSDLInit,
	) where

import qualified SDL.Init

import Immutaball.Share.Utils

-- * DirectoryIO

type SDLIO = Fixed SDLIOF
data SDLIOF a =
	SDLInit [SDL.Init.InitFlag] a
	deriving (Eq, Ord, Show)
instance Functor SDLIOF where
	fmap :: (a -> b) -> (SDLIOF a -> SDLIOF b)
	fmap f (SDLInit subsystems sdlio) = SDLInit subsystems (f sdlio)

runSDLIO :: SDLIO -> IO ()
runSDLIO (Fixed (SDLInit subsystems sdlio)) = do
	SDL.Init.initialize subsystems
	runSDLIO sdlio
	SDL.Init.quit

instance Foldable SDLIOF where
	foldr :: (a -> b -> b) -> b -> SDLIOF a -> b
	foldr reduce reduction0 (SDLInit _subsystems sdlio) = reduce sdlio reduction0
instance Traversable SDLIOF where
	traverse :: Applicative f => (a -> f b) -> SDLIOF a -> f (SDLIOF b)
	traverse traversal (SDLInit subsystems sdlio) = pure SDLInit <*> pure subsystems <*> traversal sdlio

-- * Runners

runSDLIOIO :: SDLIOF (IO ()) -> IO ()
runSDLIOIO (SDLInit subsystems sdlioio) = do
	SDL.Init.initialize subsystems
	sdlioio
	SDL.Init.quit

-- * DirectoryIO aliases that apply the Fixed wrapper

mkSDLInit :: [SDL.Init.InitFlag] -> SDLIO -> SDLIO
mkSDLInit subsystems sdlio = Fixed $ SDLInit subsystems sdlio
