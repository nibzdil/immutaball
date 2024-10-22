{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Utils.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Level.Utils
	(
		transformSol,
		restoreSolTransformation
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens

import Immutaball.Share.Level.Base
import Immutaball.Share.Math

-- TODO: implement.

-- | Apply a transformation to all positions in a Sol.
transformSol :: Mat4 Double -> Sol -> Sol
transformSol m sol = sol &
	(solVv %~ fmap (vertP %~ flip v3m4 m)) &
	(solPv %~ fmap (pathP %~ flip v3m4 m)) &  -- TODO: rotation?
	(solHv %~ fmap (itemP %~ flip v3m4 m)) &
	(solZv %~ fmap (goalP %~ flip v3m4 m)) &
	(solJv %~ fmap (jumpP %~ flip v3m4 m)) &
	(solJv %~ fmap (jumpQ %~ flip v3m4 m)) &
	(solXv %~ fmap (swchP %~ flip v3m4 m)) &
	(solUv %~ fmap (ballP %~ flip v3m4 m)) &
	(solWv %~ fmap (viewP %~ flip v3m4 m)) &
	(solWv %~ fmap (viewQ %~ flip v3m4 m)) &
	id

-- | Invert mapc's swapping of y and z axes and negation to in terms I like to
-- think of in more; glFrustrum can be combined with a transformation.
restoreSolTransformation :: Mat4 Double
restoreSolTransformation = error "TODO: unimplemented."
