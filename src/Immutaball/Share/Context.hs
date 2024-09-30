{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Context
	(
		IBDirs(..), ibStaticDataDir, ibUserDataDir, ibUserConfigDir,
		IBContext(..), ibDirs,
		withSDL
	) where

import Control.Lens

import Immutaball.Share.Context.Config
import Immutaball.Share.ImmutaballIO

data IBDirs = IBDirs {
	_ibStaticDataDir :: FilePath,
	_ibUserDataDir   :: FilePath,
	_ibUserConfigDir :: FilePath
}
	deriving (Eq, Ord, Show)
makeLenses ''IBDirs

-- | An Immutaball context instance.
data IBContext = IBContext {
	_ibDirs :: IBDirs
}
makeLenses ''IBContext

withSDL :: ContextConfig -> (IBContext -> ImmutaballIO) -> ImmutaballIO
withSDL _cxtCfg = error "Internal error: not yet implemented."
