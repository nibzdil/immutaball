{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Wire/Conttroller.hs.

{-# LANGUAGE Haskell2010 #-}

-- | A rewrite of wires, sufficient for our purposes.
module Control.Wire.Controller
	(
		module Control.Wire.Internal
	) where

import Control.Wire.Internal
