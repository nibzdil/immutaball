{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Utils
	(
		Fixed(..), fixed
	) where

import Control.Lens

-- | See F-algebras and catamorphisms for the idiom.
newtype Fixed f = Fixed {_fixed :: f (Fixed f)}
makeLenses ''Fixed
