{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables #-}

module Immutaball.Share.ImmutaballIO.DirectoryIO
	(
		-- * DirectoryIO
		DirectoryIO,
		DirectoryIOF(..),
		runDirectoryIO,

		-- * Runners
		runDirectoryAnyIO,

		-- * DirectoryIO aliases that apply the Fixed wrapper
		mkGetXdgDirectoryData,
		mkGetXdgDirectoryConfig,
		mkGetXdgDirectoryCache,
		mkGetXdgDirectoryState
	) where

import Data.Functor.Contravariant

import System.Directory

import Immutaball.Share.Utils

-- * DirectoryIO

type DirectoryIO = Fixed DirectoryIOF
data DirectoryIOF a =
	  GetXdgDirectoryData FilePath
		-- ^ Get ~/.local/share/path .
	| GetXdgDirectoryConfig FilePath
		-- ^ Get ~/.local/config/path .
	| GetXdgDirectoryCache FilePath
		-- ^ Get ~/.local/cache/path .
	| GetXdgDirectoryState FilePath
		-- ^ Get ~/.local/state/path .
	deriving (Eq, Ord, Show)
instance Functor DirectoryIOF where
	fmap :: (a -> b) -> (DirectoryIOF a -> DirectoryIOF b)
	fmap _f (GetXdgDirectoryData   path) = GetXdgDirectoryData   path
	fmap _f (GetXdgDirectoryConfig path) = GetXdgDirectoryConfig path
	fmap _f (GetXdgDirectoryCache  path) = GetXdgDirectoryCache  path
	fmap _f (GetXdgDirectoryState  path) = GetXdgDirectoryState  path
instance Contravariant DirectoryIOF where
	contramap :: (b -> a) -> (DirectoryIOF a -> DirectoryIOF b)
	contramap _f (GetXdgDirectoryData   path) = GetXdgDirectoryData   path
	contramap _f (GetXdgDirectoryConfig path) = GetXdgDirectoryConfig path
	contramap _f (GetXdgDirectoryCache  path) = GetXdgDirectoryCache  path
	contramap _f (GetXdgDirectoryState  path) = GetXdgDirectoryState  path

runDirectoryIO :: DirectoryIO -> IO FilePath
runDirectoryIO (Fixed (GetXdgDirectoryData   path)) = getXdgDirectory XdgData   path
runDirectoryIO (Fixed (GetXdgDirectoryConfig path)) = getXdgDirectory XdgConfig path
runDirectoryIO (Fixed (GetXdgDirectoryCache  path)) = getXdgDirectory XdgCache  path
runDirectoryIO (Fixed (GetXdgDirectoryState  path)) = getXdgDirectory XdgState  path

instance Foldable DirectoryIOF where
	foldr :: (a -> b -> b) -> b -> DirectoryIOF a -> b
	foldr _reduce reduction0 (GetXdgDirectoryData   _path) = reduction0
	foldr _reduce reduction0 (GetXdgDirectoryConfig _path) = reduction0
	foldr _reduce reduction0 (GetXdgDirectoryCache  _path) = reduction0
	foldr _reduce reduction0 (GetXdgDirectoryState  _path) = reduction0
instance Traversable DirectoryIOF where
	traverse :: Applicative f => (a -> f b) -> DirectoryIOF a -> f (DirectoryIOF b)
	traverse _traversal (GetXdgDirectoryData   path) = pure GetXdgDirectoryData   <*> pure path
	traverse _traversal (GetXdgDirectoryConfig path) = pure GetXdgDirectoryConfig <*> pure path
	traverse _traversal (GetXdgDirectoryCache  path) = pure GetXdgDirectoryCache  <*> pure path
	traverse _traversal (GetXdgDirectoryState  path) = pure GetXdgDirectoryState  <*> pure path

-- * Runners

runDirectoryAnyIO :: DirectoryIOF a -> DirectoryIO
runDirectoryAnyIO (GetXdgDirectoryData   path) = Fixed $ GetXdgDirectoryData   path
runDirectoryAnyIO (GetXdgDirectoryConfig path) = Fixed $ GetXdgDirectoryConfig path
runDirectoryAnyIO (GetXdgDirectoryCache  path) = Fixed $ GetXdgDirectoryCache  path
runDirectoryAnyIO (GetXdgDirectoryState  path) = Fixed $ GetXdgDirectoryState  path

-- * DirectoryIO aliases that apply the Fixed wrapper

mkGetXdgDirectoryData :: FilePath -> DirectoryIO
mkGetXdgDirectoryData path = Fixed $ GetXdgDirectoryData path

mkGetXdgDirectoryConfig :: FilePath -> DirectoryIO
mkGetXdgDirectoryConfig path = Fixed $ GetXdgDirectoryConfig path

mkGetXdgDirectoryCache :: FilePath -> DirectoryIO
mkGetXdgDirectoryCache path = Fixed $ GetXdgDirectoryCache path

mkGetXdgDirectoryState :: FilePath -> DirectoryIO
mkGetXdgDirectoryState path = Fixed $ GetXdgDirectoryState path
