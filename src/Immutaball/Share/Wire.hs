{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, DerivingVia, RankNTypes #-}

-- | Besides low-level wire construction,
-- ArrowLoop's ‘loop’ helps provide for local state, e.g. as in the ‘counter’
-- example in
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/arrows.html
-- ; with loop we can use ‘Arrows’ syntax with ‘rec’ with a delayed update.
module Immutaball.Share.Wire
	(
		-- * Primitives
		--Wire,
		Wire(..), reinstancedWire,
		stepWire,
		wire,
		delayWire,
		loopWire,
		loopWireSimple,
		monadic,

		-- * Utilities
		initial,
		withM,
		delay,
		delayWith,
		integrate,
		differentiate,
		hold,
		holdWith,
		replaceNow,
		replace,
		switchNow,
		switch,
		apply,
		queue,
		queueN,
		queueNI,
		delayN,
		delayNI,
		returnWire,
		constWire,
		multistep,
		foldrA,
		foldrListA,
		multistepFeedback,
		multistepFeedbackList,
		foldlA,
		foldlListA,
		constA,
		nopA,
		wave
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad.Fix
import Data.Foldable
--import Data.Function
import Data.Functor.Identity
import Data.List

import Control.Lens
--import Control.Wire (Wire)
import qualified Control.Wire
import qualified Control.Wire.Controller
import qualified Control.Wire.Internal (Wire(Wire))

import Control.Monad.Trans.MaybeM (NaturalTransformation)
import Immutaball.Share.Math
import Immutaball.Share.Utils

-- * Primitives

-- newtype Wire m a b = { _wireStep :: a -> m (b, Wire m a b) }
-- | A wire.  A function that changes itself each time it is called / stepped.
--
-- Note that -<< would discard wires with this, which might not be what you want.
-- Beware that the left side of '-<<' may ‘never advance’ in stepping!  (-<<
-- does have uses, e.g. see 'replaceNow' where ‘-<<’ is exactly what we want
-- there and correct.)
newtype Wire m a b = ReinstanceWire { _reinstancedWire :: Control.Wire.Wire m a b }
	deriving (Functor, Applicative)
		via (Control.Wire.Wire m a)
	deriving (Arrow, ArrowChoice, ArrowLoop, {-Choice, Strong, Costrong, Profunctor, -}Category)
		via (Control.Wire.Wire m)
makeLenses ''Wire
-- (Without newtype wrapper we get a warning: orphan instance.)
-- Note that -<< would discard wires with this, which might not be what you want.
-- Beware that the left side of '-<<' may ‘never advance’ in stepping!
instance (Monad m) => ArrowApply (Wire m) where
	app = apply

-- | Run a 'Wire'.
stepWire :: (Functor m) => Wire m a b -> a -> m (b, Wire m a b)
--stepWire = Control.Wire.Controller.stepWire
stepWire w a = second ReinstanceWire <$> Control.Wire.Controller.stepWire (_reinstancedWire w) a

-- | Expose primitives that IMO should be already non-internal, or at the very
-- least accessible through a non-internal interface.  Weirdly, I found no way
-- to represent a function equivalent to ‘Wire’ using only the non-internal
-- ‘wires’ API.
wire :: (Functor m) => (a -> m (b, Wire m a b)) -> Wire m a b
--wire = Control.Wire.Internal.Wire
wire wireStep_ = ReinstanceWire . Control.Wire.Internal.Wire $ \a -> second _reinstancedWire <$> wireStep_ a

-- | 'Wire' provides us with a delay, in contrast with '->'.
delayWire :: (Functor m) => b -> Wire m a b -> Wire m a b
--delayWire y0 wire_ = wire $ \x -> (\(y, wire') -> (y0, delayWire y wire')) <$> stepWire wire_ x
delayWire y0 wire_ = wire $ \x -> (\ ~(y, wire') -> (y0, delayWire y wire')) <$> stepWire wire_ x

-- | 'Wire' is an instance of 'ArrowLoop'.
loopWire :: (Monad m, MonadFix m) => Wire m (b, d) (c, d) -> Wire m b c
loopWire w = wire $ \b -> (\ ~( ~(c, _d), w') -> (c, loopWire w')) <$> (mfix $ \ ~( ~(_c, d), _w') -> stepWire w (b, d))

-- | Simple example with Identity.
loopWireSimple :: Wire Identity (b, d) (c, d) -> Wire Identity b c
--loopWireSimple w = wire $ \b -> let Identity ((c, d), w') = stepWire w (b, d) in Identity (c, loopWireSimple w')
loopWireSimple = fix $ \loopWireSimple_ w -> wire $ \b -> let Identity ((c, _d), w') = fix (\(Identity ((_c, d), _w')) -> stepWire w (b, d)) in Identity (c, loopWireSimple_ w')

-- | Run the given action in the monad.
monadic :: (Applicative m) => Wire m (m a) a
monadic = wire $ \m -> (\a -> (a, monadic)) <$> m

-- * Utilities

-- | For a vesion that always applies the input, see 'monadic'.
--
-- This version only initializes the monadic action once.
initial :: (Applicative m) => Wire m (m a) a
initial = wire $ \m -> (\a -> (a, pure a)) <$> m

-- | The monad is only used to initialize the wire; it is not repeated.
withM :: (Monad m) => (s -> Wire m a b) -> (a -> m s) -> Wire m a b
withM initWire initState = wire $ \a -> initState a >>= \s -> stepWire (initWire s) a

delay :: (Monad m) => a -> Wire m a a
delay y0 = delayWire y0 returnA

-- TODO: test delayWith.

-- | A 'delay' variant that takes the initial value through the wire.
--
-- The _second_ argument (not the first) is the initial value.
--
-- The initial argument is ignored after the first frame.
delayWith :: (Monad m) => Wire m (a, a) a
delayWith = wire $ \(y1, y0) -> pure (y0, delay y1 <<< arr fst)

integrate :: (Num a, Monad m, MonadFix m) => a -> Wire m a a
-- Low-level example for knowledge aid:
--integrate y0 = flip fix y0 $ \me y -> wire $ \x -> let result = y + x in return (result, me result)
integrate y0 = proc x -> do
	rec output <- delay y0 -< output + x
	returnA -< output

differentiate :: (Num a, Monad m, MonadFix m) => Wire m a a
differentiate = proc x -> do
	lastX <- delayWith -< (x, x)
	returnA -< x - lastX

-- | Output the last available input.
hold :: (Monad m, MonadFix m) => a -> Wire m (Maybe a) a
-- A shorter style:
{-
hold a0 = proc ma -> do
	rec lastJust <- delay a0 -< maybe lastJust id ma
	returnA -< maybe lastJust id ma
-}
-- Arrows counter-style:
-- {-
hold a0 = proc ma -> do
	rec
		output <- returnA -< maybe lastJust id ma
		lastJust <- delay a0 -< output
	returnA -< output
-- -}

-- TODO: test holdWith

-- | A 'hold' variant that takes the initial value through the wire.
--
-- The initial argument is ignored after being reset.
holdWith :: (Monad m, MonadFix m) => Wire m (Maybe a, a) a
holdWith = proc (set_, initial_) -> do
	rec
		output <- returnA -< maybe lastOutput id set_
		lastOutput <- delayWith -< (output, initial_)
	returnA -< output

-- TODO: test replace*, switch*, and apply.

-- | Feed the input back into the new wire.
replaceNow :: (Monad m) => Wire m a (Wire m a b) -> Wire m a b
-- This is the direct version, which we are keeping for reference:
-- Actually, we commented out the ArrowApply instance (keeping arrowDiscard),
-- although arrowDiscard is exactly what we want here and correct, so use the
-- direct version now rather than the proc syntax.
{-
replaceNow w0 = wire $ \a -> stepWire w0 a >>= \(b, _w1) -> stepWire b a
-}
-- Here is a higher level version, which we are using:
-- Without instance ArrowApply:
-- ‘Could not deduce ‘ArrowApply (Wire m)’’:
-- {-
replaceNow w0 = proc a -> do
	b <- w0 -< a
	b -<< a
-- -}

-- | Discards the outer new wire, replacing it with the output wire instead.
replace :: (Functor m) => Wire m a (b, (Wire m a b)) -> Wire m a b
--replace w0 = wire $ \a -> (\(b, w1) -> delayWire b w1) <$> stepWire w0 a
replace w0 = wire $ \a -> (\((b, w1), _w1Outer) -> (b, w1)) <$> stepWire w0 a

-- | Feed the input back into the new wire.
switchNow :: (Monad m) => Wire m a (Either (Wire m a b) b) -> Wire m a b
switchNow w0 = wire $ \a -> stepWire w0 a >>= \(eb, w1) -> either (\w1Override -> stepWire w1Override a) (\b -> return (b, switchNow w1)) eb

-- | Optionally discard the outer new wire, replacing it with the provided
-- output wire instead.
switch :: (Functor m) => Wire m a (b, Maybe (Wire m a b)) -> Wire m a b
switch w0 = wire $ \a -> (\((b, mw1), w1Outer) -> (b, maybe (switch w1Outer) id mw1)) <$> stepWire w0 a

-- | Discards the wire; gives the result.
--
-- Each wire produces a wire sequence for future wires in time, but at every
-- frame the entire sequence is discarded and replaced with the new wire that
-- is provided as input.
--
-- Note that -<< would discard wires with this, which might not be what you want.
-- Beware that the left side of '-<<' may ‘never advance’ in stepping!
apply :: (Functor m) => Wire m (Wire m a b, a) b
-- Here is a version that uses monads:
--apply = wire $ \(w0, a) -> stepWire w0 a >>= \(b, _w1) -> return (b, apply)
-- We only need fmap here:
apply = wire $ \(w0, a) -> (\(b, _w1) -> (b, apply)) <$> stepWire w0 a

-- | Process one input each frame.
queue :: (Applicative m, MonadFix m) => Wire m [a] (Maybe a)
--queue = flip fix [] $ \me queued -> wire $ \(as) -> let as' = queued ++ as in pure (safeHead as', me (drop 1 as'))
queue = proc ins -> do
	rec
		(output, queue) <- returnA -< let ins' = lastQueue ++ ins in (safeHead ins', drop 1 ins')
		lastQueue <- delay [] -< queue
	returnA -< output

-- TODO: test queueN.

-- | Send at most N chunks at once.
queueN :: (Integral i, Monad m, MonadFix m) => i -> Wire m [a] [a]
queueN n = queueNI (fromIntegral n)

-- | 'queueN' variant specialized to Integer.
queueNI :: (Monad m, MonadFix m) => Integer -> Wire m [a] [a]
queueNI n = proc ins -> do
	rec
		(chunk, queue) <- returnA -< let ins' = lastQueue ++ ins in (genericTake n ins', genericDrop n ins')
		lastQueue <- delay [] -< queue
	returnA -< chunk

-- | Delay a given number of frames >= 0, with the same output for all frames
-- in between.
delayN :: (Integral i, Applicative m) => i -> a -> Wire m a a
delayN n y0tn = delayNI (fromIntegral n) y0tn

-- | 'delayN' variant specialized to Integer.
delayNI :: (Applicative m) => Integer -> a -> Wire m a a
delayNI n y0tn
	| n < 0     = error $ "delayNI: negative argument: " ++ (show n)
	| 0 <- n    = returnWire
	| otherwise = delayWire y0tn (delayNI (n-1) y0tn)

-- | The identity wire.
returnWire :: (Applicative m) => Wire m a a
returnWire = wire $ \a -> pure (a, returnWire)

-- | An input ignoring wire.
constWire :: (Applicative m) => b -> Wire m a b
constWire b = wire $ \_ -> pure (b, constWire b)

-- TODO: add test multiStep.

-- | Step the wire multiple times, once for each input.
--
-- If we had dependent types in our language, we could probably represent
-- equality of lengths.
multistep :: (Monad m) => Wire m a b -> Wire m [a] [b]
multistep w0 = wire . flip fix w0 $ \me wn as -> case as of
	[]        -> pure ([], multistep wn)
	(an:rest) -> stepWire wn an >>= \(bn, wnp1) -> first (bn:) <$> me wnp1 rest

-- TODO: see if can maybe you can do something like class LeftArrow with inArr :: ((a -> b) -> c) -> Arr … ?
----inArrWire :: ((a -> b) -> c) -> Wire m a b -> Wire m () c
--inArrWire :: ((a -> b) -> (c -> d)) -> Wire m a b -> Wire m c d
--inArrWire f w = wire $ \c -> f $ \a -> ???

foldrA :: (Foldable t, Monad m, MonadFix m) => Wire m (a, b) b -> Wire m (b, t a) b
foldrA reduce = foldrListA reduce <<< second (arr toList)

foldrListA :: (Monad m, MonadFix m) => Wire m (a, b) b -> Wire m (b, [a]) b
foldrListA reduce = proc (reduction0, xs) -> do
	--foldr reduce reduction0 [] = reduction0
	--foldr reduce reduction0 (x:rest) = reduce x (foldr reduce reduction0 rest)
	case xs of
		[] -> returnA -< reduction0
		(x:rest) -> reduce <<< second (foldrListA reduce) -< (x, (reduction0, rest))

-- | 'multistep', with a value for the previous (or initial) result.
--
-- Step the wire multiple times in a single frame.  Add a state value between
-- steps.
multistepFeedback :: (Monad m, MonadFix m) => NaturalTransformation t [] -> NaturalTransformation [] t -> Wire m (a, b) (c, b) -> Wire m (t a, b) (t c, b)
multistepFeedback toList_ fromList_ step = first (arr fromList_) <<< multistepFeedbackList step <<< first (arr toList_)

-- | 'multistepFeedback' specialized to lists.
multistepFeedbackList :: (Monad m, MonadFix m) => Wire m (a, b) (c, b) -> Wire m ([a], b) ([c], b)
multistepFeedbackList w0 = wire . flip fix w0 $ \me wn (as, b) -> case as of
	[]      -> pure (([], b), multistepFeedbackList wn)
	(a:as') -> stepWire wn (a, b) >>= \((c, b'), wnp1) -> first (first (c:)) <$> me wnp1 (as', b')

foldlA :: (Foldable t, Monad m, MonadFix m) => Wire m (b, a) b -> Wire m (b, t a) b
foldlA reduce = foldlListA reduce <<< second (arr toList)

foldlListA :: (Monad m, MonadFix m) => Wire m (b, a) b -> Wire m (b, [a]) b
foldlListA reduce = proc (reduction0, xs) -> do
	--foldl reduce reduction0 [] = reduction0
	--foldl reduce reduction0 (x:rest) = foldl reduce (reduce reduction0 x) rest
	case xs of
		[] -> returnA -< reduction0
		(x:rest) -> foldlListA reduce <<< first reduce -< ((reduction0, x), rest)

constA :: (Arrow a) => c -> a b c
constA c = arr (const c)

nopA :: (Arrow a) => a () ()
nopA = arr $ \() -> ()

wave :: (Floating a, Monad m, MonadFix m) => a -> a -> a -> Wire m a a
wave amplitude period phase = proc dt -> do
	t <- integrate 0 -< dt
	returnA -< amplitude * (sin $ tau*(t + phase)/period)
