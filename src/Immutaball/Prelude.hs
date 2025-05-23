{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Prelude
	(
		module Prelude,
		module Control.Category
	) where

-- Prelude imports
import Prelude hiding (id, (.))
--import qualified Prelude

-- base import
import Control.Category
