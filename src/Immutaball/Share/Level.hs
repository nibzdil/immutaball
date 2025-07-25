{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Level
	(
		module Immutaball.Share.Level.Base,
		module Immutaball.Share.Level.Parser,
		module Immutaball.Share.Level.Utils
	) where

import Prelude ()
--import Immutaball.Prelude

import Immutaball.Share.Level.Base
import Immutaball.Share.Level.Parser
import Immutaball.Share.Level.Utils
