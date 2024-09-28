{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Config
	(
		StaticConfig(..),
		Config(..)
	) where

import Control.Lens

data StaticConfig = StaticConfig {
}
	deriving (Eq, Ord)
makeLenses ''StaticConfig

data Config = Config {
}
	deriving (Eq, Ord)
makeLenses ''Config

-- TODO: print and parse.
