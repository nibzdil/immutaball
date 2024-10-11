{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.GLManager.Config
	(
		GLManagerStaticConfig(..),
		defaultGLManagerStaticConfig
	) where

import Prelude ()
--import Immutaball.Prelude

import Control.Lens

data GLManagerStaticConfig = GLManagerStaticConfig {
}
makeLenses ''GLManagerStaticConfig

-- | Default static config.
defaultGLManagerStaticConfig :: GLManagerStaticConfig
defaultGLManagerStaticConfig = GLManagerStaticConfig {
}
