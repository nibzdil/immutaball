{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

-- | Optionally, we can add our own extra information about a level file.
module Immutaball.Share.Level.Analysis
	(
		SolAnalysis(..),
		mkSolAnalysis
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))

import Control.Lens

import Immutaball.Share.Level.Base

data SolAnalysis = SolAnalysis {
}
	deriving (Eq, Ord, Show)
makeLenses ''SolAnalysis

mkSolAnalysis :: Sol -> SolAnalysis
mkSolAnalysis _sol = fix $ \_sa -> SolAnalysis {
}
