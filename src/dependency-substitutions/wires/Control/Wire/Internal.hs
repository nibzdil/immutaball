{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Wire/Internal.hs.

{-# LANGUAGE Haskell2010 #-}
--{-# LANGUAGE TemplateHaskell, Arrows, DerivingVia, RankNTypes #-}

-- | A rewrite of wires, sufficient for our purposes.
module Control.Wire.Internal
	(
		Wire(..),
		stepWire
	) where

import Control.Arrow
import Control.Category as C
import Control.Monad.Fix

-- | A self-modifying function.
--
-- This is the central construct in the ‘wires’ FRP implementation.
newtype Wire m a b = Wire { _wireStep :: a -> m (b, Wire m a b) }

-- | Step a wire.  Obtain its result and the next version of itself.
stepWire :: Wire m a b -> a -> m (b, Wire m a b)
stepWire = _wireStep

instance (Functor m) => Functor (Wire m a) where
	fmap f ~(Wire w) = Wire $ \a -> (\ ~(b, w') -> (f b, fmap f w')) <$> w a

instance (Applicative m) => Applicative (Wire m i) where
	pure a = fix $ \me -> Wire $ \_i -> pure (a, me)
	~(Wire wf) <*> ~(Wire wa) = Wire $ \i -> pure (\ ~(f, wf') ~(a, wa') -> (f a, wf' <*> wa')) <*> wf i <*> wa i

instance (Monad m) => Category (Wire m) where
	id = fix $ \me -> Wire $ \a -> return (a, me)
	~(Wire wc) . ~(Wire wb) = Wire $ \a -> wb a >>= \ ~(b, wb') -> wc b >>= \ ~(c, wc') -> return (c, wc' C.. wb')

instance (Monad m) => Arrow (Wire m) where
	arr f = fix $ \me -> Wire $ \a -> pure (f a, me)
	first ~(Wire w) = Wire $ \ ~(a, d) -> (\ ~(b, w') -> ((b, d), first w')) <$> w a

instance (Monad m) => ArrowChoice (Wire m) where
	-- (Tricky: in the Left case, don't use ‘me’ which would discard ‘w'’.)
	left ~(Wire w) = fix $ \me -> Wire $ \bd -> either (\b -> (\ ~(c, w') -> (Left c, left w')) <$> w b) (\d -> pure (Right d, me)) bd

instance (MonadFix m) => ArrowLoop (Wire m) where
	-- w :: (b, d) -> m ((c, d), Wire m (b, d) (c, d))
	--  Make sure this is lazy enough, or it could hang!
	loop ~(Wire w) = Wire $ \b -> (\ ~(~(c, _d), w') -> (c, loop w')) <$> (mfix $ \ ~(~(_c, d), _w') -> w (b, d))
