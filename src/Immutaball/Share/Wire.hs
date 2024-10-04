{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, DerivingVia #-}

-- Warning: this module has an orphan instance.
-- TODO: maybe just use a newtype wrapper, and comment out a raw definition.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Besides low-level wire construction,
-- ArrowLoop's ‘loop’ helps provide for local state, e.g. as in the ‘counter’
-- example in
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/arrows.html
-- ; with loop we can use ‘Arrows’ syntax with ‘rec’ with a delayed update.
module Immutaball.Share.Wire
	(
		-- * Primitives
		--Wire,
		Wire(..),
		stepWire,
		wire,
		delay,
		loopWire,
		loopWireSimple,

		-- * Utilities
		withM,
		mfix',
		integrate,
		derive,
		hold,
		replace,
		switch,
		applyWire
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad.Fix
--import Data.Function
import Data.Functor.Identity

--import Control.Wire (Wire)
import qualified Control.Wire
import qualified Control.Wire.Controller
import qualified Control.Wire.Internal (Wire(Wire))

-- * Primitives

-- newtype Wire m a b = { _wireStep :: a -> m (b, Wire m a b) }
newtype Wire m a b = ReinstanceWire { _reinstanceWire :: Control.Wire.Wire m a b }
	deriving (Functor, Applicative)
		via (Control.Wire.Wire m a)
	deriving (Arrow, ArrowChoice, ArrowLoop, {-Choice, Strong, Costrong, Profunctor, -}Category)
		via (Control.Wire.Wire m)
-- Warning: orphan instance, without newtype wrapper.
instance (Monad m) => ArrowApply (Wire m) where
	app = applyWire

-- | Run a 'Wire'.
stepWire :: (Functor m) => Wire m a b -> a -> m (b, Wire m a b)
--stepWire = Control.Wire.Controller.stepWire
stepWire w a = second ReinstanceWire <$> Control.Wire.Controller.stepWire (_reinstanceWire w) a

-- | Expose primitives that IMO should be already non-internal, or at the very
-- least accessible through a non-internal interface.  Weirdly, I found no way
-- to represent a function equivalent to ‘Wire’ using only the non-internal
-- ‘wires’ API.
wire :: (Functor m) => (a -> m (b, Wire m a b)) -> Wire m a b
--wire = Control.Wire.Internal.Wire
wire wireStep_ = ReinstanceWire . Control.Wire.Internal.Wire $ \a -> second _reinstanceWire <$> wireStep_ a

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

withM :: (Monad m) => (s -> Wire m a b) -> (a -> m s) -> Wire m a b
withM initWire initState = wire $ \a -> initState a >>= \s -> stepWire (initWire s) a

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

-- | Output the last available input.
hold :: (Monad m, MonadFix m) => a -> Wire m (Maybe a) a
hold a0 = proc ma -> do
	rec lastJust <- delay a0 returnA -< maybe lastJust id ma
	returnA -< maybe lastJust id ma

-- TODO: test replace, switch, and applyWire.

replace :: (Monad m) => Wire m a (Wire m a b) -> Wire m a b
--replace w0 = wire $ \a -> stepWire w0 a >>= \(b, _w1) -> stepWire b a
-- Without instance ArrowApply:
-- ‘Could not deduce ‘ArrowApply (Wire m)’’:
-- {-
replace w0 = proc a -> do
	b <- w0 -< a
	b -<< a
-- -}

switch :: (Monad m) => Wire m a (Either (Wire m a b) b) -> Wire m a b
switch w0 = wire $ \a -> stepWire w0 a >>= \(eb, w1) -> either (\w1Override -> stepWire w1Override a) (\b -> return (b, switch w1)) eb

-- | Discards the wire; gives the result.
applyWire :: (Functor m) => Wire m (Wire m a b, a) b
--applyWire = wire $ \(w0, a) -> stepWire w0 a >>= \(b, w1) -> return (b, applyWire)
applyWire = wire $ \(w0, a) -> (\(b, _w1) -> (b, applyWire)) <$> stepWire w0 a
