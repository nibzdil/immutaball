{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Base.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Level.Base
	(
		Sol(..),
		LevelIB
	) where

import Prelude ()
--import Immutaball.Prelude

import Control.Lens

-- | The level format: .sol.
data Sol = Sol {
}
makeLenses ''Sol

type LevelIB = Sol
