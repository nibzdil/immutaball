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
			sdlmh_finalizers,
		SDLManagerCommand(..),
		GLIOFTo(..),
		ResourceAllocation(..),
		ResourceAllocationTo(..)
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

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.ImmutaballIO.SDLIO

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
	| GLSequence [GLIOFTo] (TMVar ())
	| GLSequenceValueless [GLIOF ()] (TMVar ())
	-- | Attach a resource to the lifetime of the SDL Manager thread.
	| AttachLifetime ResourceAllocationTo
	| forall me. GenSDL (SDLIOF me) (TMVar me)

data GLIOFTo = forall me. GLIOFTo { _gliofTo :: (GLIOF me, TMVar me) }

data ResourceAllocation = forall me. ResourceAllocation { _resourceAllocation :: (ImmutaballIOF me, me -> ImmutaballIOF ()) }
data ResourceAllocationTo = forall me. ResourceAllocationTo { _resourceAllocationTo :: ((ImmutaballIOF me, me -> ImmutaballIOF ()), TMVar me) }

-- | The inner fields and lenses are internal (low-level).
data SDLManagerHandle = SDLManagerHandle {
	_sdlmh_done :: TVar Bool,
	_sdlmh_doneReceived :: TVar Bool,
	_sdlmh_commands :: TChan SDLManagerCommand,

	-- | Stored in reverse order, which is actually convenient for us, since we
	-- can then free in reverse order of init order.
	_sdlmh_finalizers :: TVar [ResourceAllocationTo]
}
	deriving (Eq)
makeLenses ''SDLManagerHandle  -- Error for future module reference.
