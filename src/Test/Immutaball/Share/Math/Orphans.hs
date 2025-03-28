{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Immutaball.Share.Math.Orphans
	(
		--module Test.Immutaball.Share.Math.Core.Orphans
	) where

import Prelude ()
--import Immutaball.Prelude

import Test.Immutaball.Share.Math.Core.Orphans ()
