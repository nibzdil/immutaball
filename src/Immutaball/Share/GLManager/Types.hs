{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Moved into another module to avoid Template Haskell errors.
module Immutaball.Share.GLManager.Types
	(
		GLManagerHandle(..), glmh_done, glmh_doneReceived, glmh_commands,
		GLManagerCommand(..),
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Lens

import Immutaball.Share.ImmutaballIO.GLIO

-- Moved from later to fix Template Haskell errors.
data GLManagerCommand =
	-- | Close the manager; it's done.
	  QuitGLManager
	| NopGLManager
	-- | Running e.g. glBindTexture and then glTexImage2D concurrently can
	-- introduces race conditions without ordering management.  Ordered
	-- queueing can help with this.
	-- Note: individual valueless queue commands are not required to run in
	-- parallel.
	| GLQueueValueless [GLIOF ()]

-- | The inner fields and lenses are internal (low-level).
data GLManagerHandle = GLManagerHandle {
	_glmh_done :: TVar Bool,
	_glmh_doneReceived :: TVar Bool,
	_glmh_commands :: TChan GLManagerCommand
}
	deriving (Eq)
makeLenses ''GLManagerHandle  -- Error for future module reference.
