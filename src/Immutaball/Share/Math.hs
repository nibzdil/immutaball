{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

-- | Includes our Math modules.
module Immutaball.Share.Math
	(
		module Immutaball.Share.Math.Core
	) where

import Prelude ()
--import Immutaball.Prelude

import Immutaball.Share.Math.Core
