{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia #-}

module Immutaball.Share.AutoPar
	(
		AutoPar,
		AutoParT(..), autoParT,
		runAutoParT,
		runAutoPar
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Parallel
import Data.Functor.Identity

import Control.Lens

type AutoPar = AutoParT Identity

-- Possible extension: an ADT with ‘NoAutoPar’ that does not apply extra ‘par’
-- instances.
newtype AutoParT m a = AutoParT { _autoParT :: m a }
	deriving (Eq, Ord, Show, Semigroup, Monoid, Enum, Read, Num, Fractional, Real, RealFrac, Bounded)
		via (m a)
makeLenses ''AutoParT

runAutoParT :: AutoParT m a -> m a
runAutoParT (AutoParT m) = m

runAutoPar :: AutoPar a -> a
runAutoPar (AutoParT (Identity a)) = a

instance (Functor m) => Functor (AutoParT m) where
	fmap f (AutoParT m) = AutoParT (fmap f m)
instance (Applicative m) => Applicative (AutoParT m) where
	pure a = AutoParT (pure a)
	(AutoParT f) <*> (AutoParT a) = f `par` a `par` AutoParT (f <*> a)
instance (Monad m) => Monad (AutoParT m) where
	return = pure
	(AutoParT ma) >>= fmb = AutoParT $ ma `par` fmb `par` (ma >>= _autoParT . fmb)
