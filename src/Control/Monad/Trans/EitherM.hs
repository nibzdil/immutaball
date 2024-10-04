{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, InstanceSigs #-}

module Control.Monad.Trans.EitherM
	(
		EitherMT(..), eitherMT,
		runEitherMT,
		EitherM
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Functor.Identity

import Control.Lens
import Control.Monad.Trans.Class
import qualified SDL.Init

newtype EitherMT m n a = EitherMT { _eitherMT :: Either (m a) (n a) }
	deriving (Eq, Ord, Show, Read)
instance (Monad m) => MonadTrans (EitherMT m) where
	lift :: (Monad m) => n a -> EitherMT m n a
	lift = EitherMT . Right
runEitherMT :: EitherMT m n a -> Either (m a) (n a)
runEitherMT = _eitherMT

type EitherM m a = EitherMT m Identity a

instance (Functor m, Functor n) => Functor (EitherMT m n) where
	fmap :: (a -> b) -> (EitherMT m n a -> EitherMT m n b)
	fmap f = EitherMT . either (Left . fmap f) (Right . fmap f) . runEitherMT

instance (Applicative m, Applicative n) => Applicative (EitherMT m n) where
	pure :: (Applicative m, Applicative n) => a -> EitherMT m n a
	pure = EitherMT . Right . pure
	(<*>) :: (Applicative m, Applicative n) => EitherMT m n (a -> b) -> EitherMT m n a -> EitherMT m n b
	f <*> a = oops

--instance MonadFix 
--mfix' :: (Monad m) => (a -> m a) -> m a
----mfix' f = let ma = ma >>= f in ma
--mfix' f = fix $ \me -> me >>= f
