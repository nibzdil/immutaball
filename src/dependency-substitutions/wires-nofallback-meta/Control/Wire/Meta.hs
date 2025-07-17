{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Wire.hs.

{-# LANGUAGE Haskell2010 #-}

-- | Information about which Wire implementation is used.
module Control.Wire.Meta
	(
		isFallbackWire
	) where

-- | Is the fallback implementation used rather than the external ‘wires’
-- dependency?
isFallbackWire :: Bool
isFallbackWire = False
