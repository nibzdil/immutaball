{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

-- | Moved into another module to avoid Template Haskell errors.
module Immutaball.Share.SDLManager.Types
	(
		SDLManagerHandle(..), sdlmh_done, sdlmh_doneReceived, sdlmh_commands,
		SDLManagerCommand(..),
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Lens
import qualified Data.Text as T
import SDL.Event
import SDL.Video

import Immutaball.Share.ImmutaballIO.GLIO

-- Moved from later to fix Template Haskell errors.
data SDLManagerCommand =
	-- | Close the manager; it's done.
	  QuitSDLManager
	| NopSDLManager
	-- | SDL.Event.pollEvent documents that it requires it be run in the OS
	-- thread which set the video mode, which is why SDLManager solely manages
	-- this.
	| PollEvent (TMVar (Maybe Event))
	| PollEvents (TMVar ([Event]))
	| WithWindow T.Text WindowConfig (TMVar Window)
	| WithGLContext Window (TMVar GLContext)
	| GLSwapWindow Window (TMVar ())
	| forall me. GLSequence (GLIOF me) (TMVar me)

-- | The inner fields and lenses are internal (low-level).
data SDLManagerHandle = SDLManagerHandle {
	_sdlmh_done :: TVar Bool,
	_sdlmh_doneReceived :: TVar Bool,
	_sdlmh_commands :: TChan SDLManagerCommand
}
	deriving (Eq)
makeLenses ''SDLManagerHandle  -- Error for future module reference.
