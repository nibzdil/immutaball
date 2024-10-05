{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, InstanceSigs, ExplicitForAll, RankNTypes #-}

module Control.Monad.Trans.MaybeM
	(
		MaybeMT(..), maybeMT,
		runMaybeMT,
		MaybeM,
		NaturalTransformation,
		liftNaturalTransformation,
		liftNT,
		runMaybeM
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Applicative
import Data.Functor.Identity

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Zip

import Debug.Trace as D  ---------------------------- TODO--

newtype MaybeMT m a = MaybeMT { _maybeMT :: Either (m a) a }
	deriving (Eq, Ord, Show, Read)
makeLenses ''MaybeMT
instance MonadTrans MaybeMT where
	lift :: (Monad m) => m a -> MaybeMT m a
	lift = MaybeMT . Left
runMaybeMT :: MaybeMT m a -> Either (m a) a
runMaybeMT = _maybeMT

type MaybeM = MaybeMT Identity

instance (Functor m) => Functor (MaybeMT m) where
	fmap :: (a -> b) -> (MaybeMT m a -> MaybeMT m b)
	fmap f = MaybeMT . either (Left . fmap f) (Right . f) . runMaybeMT

instance (Applicative m) => Applicative (MaybeMT m) where
	pure :: (Applicative m) => a -> MaybeMT m a
	pure = MaybeMT . Right
	(<*>) :: (Applicative m) => MaybeMT m (a -> b) -> MaybeMT m a -> MaybeMT m b
	(MaybeMT (Right f))  <*> (MaybeMT (Right a))  = MaybeMT (Right (f a))
	(MaybeMT (Left  mf)) <*> (MaybeMT (Left  ma)) = MaybeMT (Left (mf <*> ma))
	(MaybeMT (Right f))  <*> (MaybeMT (Left  ma)) = MaybeMT (Left (pure f <*> ma))
	(MaybeMT (Left  mf)) <*> (MaybeMT (Right a))  = MaybeMT (Left (mf <*> pure a))

instance (Monad m) => Monad (MaybeMT m) where
	return :: (Monad m) => a -> MaybeMT m a
	return = pure
	(>>=) :: (Monad m) => MaybeMT m a -> (a -> MaybeMT m b) -> MaybeMT m b
	(MaybeMT (Right a)) >>= f = f a
	(MaybeMT (Left ma)) >>= f = MaybeMT . Left $ ma >>= \a -> either id return $ runMaybeMT (f a)

instance (Monad m, MonadFix m) => MonadFix (MaybeMT m) where
	mfix :: (Monad m) => (a -> MaybeMT m a) -> MaybeMT m a
	--mfix f = MaybeMT . Left . mfix $ \a -> either id return $ runMaybeMT (f a)
	mfix f = MaybeMT . Left . mfix $ \a -> D.trace "DEBUG: mfix MaybeMT: " . either id return $ runMaybeMT (f a)

instance (Alternative m) => Alternative (MaybeMT m) where
	empty :: (Alternative m) => MaybeMT m a
	empty = MaybeMT . Left $ empty
	(<|>) :: (Alternative m) => MaybeMT m a -> MaybeMT m a -> MaybeMT m a
	--x@( MaybeMT (Right _a)) <|> _ = x
	_x@(MaybeMT (Right a))  <|> y = MaybeMT . Left $ pure a <|> (either id pure . runMaybeMT $ y)
	_x@(MaybeMT (Left  ma)) <|> y = MaybeMT . Left $ ma     <|> (either id pure . runMaybeMT $ y)

instance (MonadPlus m) => MonadPlus (MaybeMT m) where
	mzero :: (MonadPlus m) => MaybeMT m a
	mzero = MaybeMT . Left $ mzero
	mplus :: (MonadPlus m) => MaybeMT m a -> MaybeMT m a -> MaybeMT m a
	--x@( MaybeMT (Right _a)) `mplus` _ = x
	_x@(MaybeMT (Right a))  `mplus` y = MaybeMT . Left $ pure a `mplus` (either id pure . runMaybeMT $ y)
	_x@(MaybeMT (Left  ma)) `mplus` y = MaybeMT . Left $ ma     `mplus` (either id pure . runMaybeMT $ y)

instance (Foldable m) => Foldable (MaybeMT m) where
	foldr :: (Foldable m) => (a -> b -> b) -> b -> MaybeMT m a -> b
	foldr reduce reduction0 (MaybeMT (Right a))  = reduce a reduction0
	foldr reduce reduction0 (MaybeMT (Left  ma)) = foldr reduce reduction0 ma

instance (Traversable m) => Traversable (MaybeMT m) where
	traverse :: (Traversable m, Applicative f) => (a -> f b) -> MaybeMT m a -> f (MaybeMT m b)
	traverse traversal_ (MaybeMT (Right a))  = pure (MaybeMT . Right) <*> traversal_ a
	traverse traversal_ (MaybeMT (Left  ma)) = pure (MaybeMT . Left)  <*> traverse traversal_ ma

instance (MonadZip m) => MonadZip (MaybeMT m) where
	mzip :: (MonadZip m) => MaybeMT m a -> MaybeMT m b -> MaybeMT m (a, b)
	mzip (MaybeMT (Right a))  (MaybeMT (Right b))  = MaybeMT (Right (a, b))
	mzip (MaybeMT (Left  ma)) (MaybeMT (Left  mb)) = MaybeMT (Left (mzip ma mb))
	mzip (MaybeMT (Right a))  (MaybeMT (Left  mb)) = MaybeMT (Left (mzip (pure a) mb))
	mzip (MaybeMT (Left  ma)) (MaybeMT (Right b))  = MaybeMT (Left (mzip ma (pure b)))

type NaturalTransformation m n = forall a. m a -> n a

-- | We _could_ require a natural transformation if we wanted, but since the
-- argument is to the _left_ of an arrow, not to the right, then rather than
-- outputting as specifically as we can, we want to input as generally as we
-- can.
--liftNaturalTransformation :: NaturalTransformation m n -> (MaybeMT m a -> MaybeMT n a)
liftNaturalTransformation :: (m a -> n a) -> (MaybeMT m a -> MaybeMT n a)
liftNaturalTransformation _nt (MaybeMT (Right a))  = MaybeMT (Right a)
liftNaturalTransformation  nt (MaybeMT (Left  ma)) = MaybeMT (Left (nt ma))

-- | Alias for 'liftNaturalTransformation'.
--liftNT :: NaturalTransformation m n -> (MaybeMT m a -> MaybeMT n a)
liftNT :: (m a -> n a) -> (MaybeMT m a -> MaybeMT n a)
liftNT = liftNaturalTransformation

-- | Simplify a MaybeMT if the possible monad is 'Identity'.
runMaybeM :: MaybeM a -> a
runMaybeM (MaybeMT (Right a))           = a
runMaybeM (MaybeMT (Left (Identity a))) = a
