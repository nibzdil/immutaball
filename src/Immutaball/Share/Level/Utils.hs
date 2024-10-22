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

import Immutaball.Share.Level.Base
import Immutaball.Share.Math

-- TODO: implement.

-- | Apply a transformation to all positions in a Sol.
transformSol :: Mat4 Double -> Sol -> Sol
transformSol = error "TODO: unimplemented."

-- | Invert mapc's swapping of y and z axes and negation to in terms I like to
-- think of in more; glFrustrum can be combined with a transformation.
restoreSolTransformation :: Mat4 Double
restoreSolTransformation = error "TODO: unimplemented."
