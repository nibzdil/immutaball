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
--
-- TODO: apply transformation to path E orientation, which is a quaternion.
transformSol :: Mat4 Double -> Sol -> Sol
transformSol m sol = sol &
	(solVv %~ fmap (vertP %~ m4v3 m)) &
	(solPv %~ fmap (pathP %~ m4v3 m)) &
	-- TODO:
	--(solPv %~ fmap (pathE %~ (\q -> (\v -> (qasv4.~v) & q) . TODO . mv4 m . rotate3 . qToSr . (^.qasv4) $ q))) &  -- Convert quaternion rotation to rotation matrix, mat mul, then convert back to quaternion.
	(solHv %~ fmap (itemP %~ m4v3 m)) &
	(solZv %~ fmap (goalP %~ m4v3 m)) &
	(solJv %~ fmap (jumpP %~ m4v3 m)) &
	(solJv %~ fmap (jumpQ %~ m4v3 m)) &
	(solXv %~ fmap (swchP %~ m4v3 m)) &
	(solUv %~ fmap (ballP %~ m4v3 m)) &
	(solWv %~ fmap (viewP %~ m4v3 m)) &
	(solWv %~ fmap (viewQ %~ m4v3 m)) &
	id

-- | Invert mapc's swapping of y and z axes and negation to in terms I like to
-- think of in more; glFrustrum can be combined with a transformation.
restoreSolTransformation :: Mat4 Double
restoreSolTransformation = error "TODO: unimplemented."
