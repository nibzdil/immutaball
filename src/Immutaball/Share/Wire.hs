{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Expose primitives that IMO should be already non-internal, or at the very
-- least accessible through a non-internal interface.  Weirdly, I found no way
-- to represent a function equivalent to ‘Wire’ using only the non-internal
-- ‘wires’ API.
module Immutaball.Share.Wire
	(
		wire
	) where

import Control.Wire.Internal

wire :: (a -> m (b, Wire m a b)) -> Wire m a b
wire = Wire
