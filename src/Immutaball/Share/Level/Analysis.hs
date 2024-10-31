{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

-- | Optionally, we can add our own extra information about a level file.
module Immutaball.Share.Level.Analysis
	(
		SolAnalysis(..), saRenderAnalysis, saPhysicsAnalysis,
		sar, sap,
		SolRenderAnalysis(..),
		SolPhysicsAnalysis(..),
		mkSolAnalysis,
		mkSolRenderAnalysis,
		mkSolPhysicsAnalysis
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))

import Control.Lens

import Immutaball.Share.Level.Base

data SolAnalysis = SolAnalysis {
	-- | Extra analysis of the sol useful for rendering.
	_saRenderAnalysis :: SolRenderAnalysis,

	-- | Extra analysis of the sol useful for physics.
	_saPhysicsAnalysis :: SolPhysicsAnalysis
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolAnalysis

-- sar, sap

-- | Extra data o the sol useful for rendering.
data SolRenderAnalysis = SolRenderAnalysis {
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolRenderAnalysis

-- | Extra data o the sol useful for rendering.
data SolPhysicsAnalysis = SolPhysicsAnalysis {
}
	deriving (Eq, Ord, Show)
makeLenses ''SolAnalysis
makeLenses ''SolRenderAnalysis
makeLenses ''SolPhysicsAnalysis

sar :: Lens' SolAnalysis SolRenderAnalysis
sar = saRenderAnalysis

sap :: Lens' SolAnalysis SolPhysicsAnalysis
sap = saPhysicsAnalysis

mkSolAnalysis :: Sol -> SolAnalysis
mkSolAnalysis sol = fix $ \_sa -> SolAnalysis {
	_saRenderAnalysis  = mkSolRenderAnalysis  sol,
	_saPhysicsAnalysis = mkSolPhysicsAnalysis sol
}

mkSolRenderAnalysis :: Sol -> SolRenderAnalysis
mkSolRenderAnalysis _sol = fix $ \_sar -> SolRenderAnalysis {
}

mkSolPhysicsAnalysis :: Sol -> SolPhysicsAnalysis
mkSolPhysicsAnalysis _sol = fix $ \_sap -> SolPhysicsAnalysis {
}
