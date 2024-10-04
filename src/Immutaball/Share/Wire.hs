{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows #-}

-- | Besides low-level wire construction,
-- ArrowLoop's ‘loop’ helps provide for local state, e.g. as in the ‘counter’
-- example in
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/arrows.html
-- ; with loop we can use ‘Arrows’ syntax with ‘rec’ with a delayed update.
module Immutaball.Share.Wire
	(
		-- * Primitives
		Wire,
		stepWire,
		wire,
		delay,
		loopWire,
		loopWireSimple,

		-- * Utilities
		mfix',
		integrate,
		derive
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad.Fix
--import Data.Function
import Data.Functor.Identity

import Control.Wire
import qualified Control.Wire.Controller
import qualified Control.Wire.Internal (Wire(Wire))

-- * Primitives

-- data Wire = …

-- | Run a 'Wire'.
stepWire :: Wire m a b -> a -> m (b, Wire m a b)
stepWire = Control.Wire.Controller.stepWire

-- | Expose primitives that IMO should be already non-internal, or at the very
-- least accessible through a non-internal interface.  Weirdly, I found no way
-- to represent a function equivalent to ‘Wire’ using only the non-internal
-- ‘wires’ API.
wire :: (a -> m (b, Wire m a b)) -> Wire m a b
wire = Control.Wire.Internal.Wire

-- | 'Wire' provides us with a delay, in contrast with '->'.
delay :: (Functor m) => b -> Wire m a b -> Wire m a b
delay y0 wire_ = wire $ \x -> (\(y, wire') -> (y0, delay y wire')) <$> stepWire wire_ x

-- | 'Wire' is an instance of 'ArrowLoop'.
loopWire :: (Monad m) => Wire m (b, d) (c, d) -> Wire m b c
loopWire w = wire $ \b -> (\((c, _d), w') -> (c, loopWire w')) <$> (mfix' $ \((_c, d), _w') -> stepWire w (b, d))

-- | Simple example with Identity.
loopWireSimple :: Wire Identity (b, d) (c, d) -> Wire Identity b c
--loopWireSimple w = wire $ \b -> let Identity ((c, d), w') = stepWire w (b, d) in Identity (c, loopWireSimple w')
loopWireSimple = fix $ \loopWireSimple_ w -> wire $ \b -> let Identity ((c, _d), w') = fix (\(Identity ((_c, d), _w')) -> stepWire w (b, d)) in Identity (c, loopWireSimple_ w')

-- * Utilities

mfix' :: (Monad m) => (a -> m a) -> m a
--mfix' f = let ma = ma >>= f in ma
mfix' f = fix $ \me -> me >>= f

integrate :: (Num a, Monad m, MonadFix m) => a -> Wire m a a
-- Low-level example for knowledge aid:
--integrate y0 = flip fix y0 $ \me y -> wire $ \x -> let result = y + x in return (result, me result)
integrate y0 = proc x -> do
	rec output <- delay y0 returnA -< output + x
	returnA -< output

derive :: (Num a, Monad m, MonadFix m) => Wire m a a
derive = proc x -> do
	rec output <- delay 0 returnA -< x - output
	returnA -< output
