{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}

module Immutaball.Share.Utils
	(
		Fixed(..), fixed,
		getFixed
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens

-- | See F-algebras and catamorphisms for the idiom.
newtype Fixed f = Fixed {_fixed :: f (Fixed f)}
makeLenses ''Fixed

instance (Eq (f (Fixed f))) => Eq (Fixed f) where
	(Fixed a) == (Fixed b) = a == b
instance (Ord (f (Fixed f))) => Ord (Fixed f) where
	(Fixed a) <= (Fixed b) = a <= b
instance (Show (f (Fixed f))) => Show (Fixed f) where
	show (Fixed a) = show a

getFixed :: Fixed f -> f (Fixed f)
getFixed = (^.fixed)
