{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances, DerivingVia #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}  -- For 'FakeEOS'.

module Immutaball.Share.Utils
	(
		Fixed(..), fixed,
		getFixed,
		cata,
		RCompose(..), rcompose,
		getRCompose,
		if',
		deconsBool,
		voidA,
		safeHead,
		safeTail,
		mfix',
		joinMaybeResult,
		chunksOfI,
		chunksOf,
		closeFirstO,
		closeSecondO,
		closeFirstI,
		closeSecondI,
		openFirstO,
		openSecondO,
		openFirstI,
		openSecondI,
		openFirstIO,
		openSecondIO,
		closeFirstIO,
		closeSecondIO,
		withOpenFirstIO,
		withOpenSecondIO,
		concatFirst,
		swap,
		split,
		trueAsIntegralI,
		falseAsIntegralI,
		trueAsIntegral,
		falseAsIntegral,
		deconsMaybe,

		morElse,

		--AssumeEOS,

		setMapFilter,
		steppingMean,

		FakeEOS(..), fakeEOS,

		modfl,

		uncurry3,
		listOthers,

		runListT,
		liftList
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad.Fix
import Data.Functor.Compose
import Data.List
import Data.Maybe
import qualified Data.Set as S

import Control.Lens
import qualified Pipes as P
import qualified Pipes.Prelude as P

-- | See F-algebras and catamorphisms for the idiom.
newtype Fixed f = Fixed {_fixed :: f (Fixed f)}
makeLenses ''Fixed

instance (Eq (f (Fixed f))) => Eq (Fixed f) where
	(Fixed a) == (Fixed b) = a == b
instance (Ord (f (Fixed f))) => Ord (Fixed f) where
	(Fixed a) <= (Fixed b) = a <= b
instance (Show (f (Fixed f))) => Show (Fixed f) where
	show (Fixed a) = show a

getFixed :: Fixed f -> f (Fixed f)
getFixed = (^.fixed)

cata :: (Functor f) => (f a -> a) -> (Fixed f -> a)
cata fAlgebra = fAlgebra . fmap (cata fAlgebra) . getFixed

newtype RCompose f g a = RCompose {_rcompose :: g (f a) }
	deriving (Eq, Ord, Show, Semigroup, Monoid, Enum, Read, Num, Fractional, Real, RealFrac, Bounded)
		via (g (f a))
	deriving (Functor, Applicative, Foldable, {-Traversable, -}Contravariant)
		via (Compose g f)
makeLenses ''RCompose

getRCompose :: RCompose f g a -> g (f a)
getRCompose = (^.rcompose)

if' :: Bool -> a -> a -> a
if' True then_ _  = then_
if' False _ else_ = else_

deconsBool :: r -> r -> Bool -> r
deconsBool withTrue _  (True)  = withTrue
deconsBool _ withFalse (False) = withFalse

voidA :: (Arrow a) => a b c -> a b ()
voidA f = f >>> arr (const ())

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

mfix' :: (Monad m) => (a -> m a) -> m a
--mfix' f = let ma = ma >>= f in ma
mfix' f = fix $ \me -> me >>= f

joinMaybeResult :: Maybe (a -> Maybe b) -> (a -> Maybe b)
joinMaybeResult mf = \a -> do
	f <- mf
	f a

chunksOfI :: Integer -> [a] -> [[a]]
chunksOfI = chunksOf

chunksOf :: (Integral i) => i -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take' n xs : chunksOf n (drop' n xs)
	where (take', drop') = (genericTake, genericDrop)

closeFirstO :: (Arrow a) => a b ((), c) -> a b c
closeFirstO = (>>> arr (\((), c) -> c))

closeSecondO :: (Arrow a) => a b (c, ()) -> a b c
closeSecondO = (>>> arr (\(c, ()) -> c))

closeFirstI :: (Arrow a) => a ((), b) c -> a b c
closeFirstI = (arr ((,) ()) >>>)

closeSecondI :: (Arrow a) => a (b, ()) c -> a b c
closeSecondI = (arr (flip (,) ()) >>>)

openFirstO :: (Arrow a) => a b c -> a b ((), c)
openFirstO = (>>> arr ((,) ()))

openSecondO :: (Arrow a) => a b c -> a b (c, ())
openSecondO = (>>> arr (flip (,) ()))

openFirstI :: (Arrow a) => a b c -> a ((), b) c
openFirstI = (arr (\((), b) -> b) >>>)

openSecondI :: (Arrow a) => a b c -> a (b, ()) c
openSecondI = (arr (\(b, ()) -> b) >>>)

openFirstIO :: (Arrow a) => a b c -> a ((), b) ((), c)
openFirstIO = openFirstO . openFirstI

openSecondIO :: (Arrow a) => a b c -> a (b, ()) (c, ())
openSecondIO = openSecondO . openSecondI

closeFirstIO :: (Arrow a) => a ((), b) ((), c) -> a b c
closeFirstIO = closeFirstI . closeFirstO

closeSecondIO :: (Arrow a) => a (b, ()) (c, ()) -> a b c
closeSecondIO = closeSecondI . closeSecondO

withOpenFirstIO :: (Arrow a) => (a ((), b0) ((), c0) -> a ((), b1) ((), c1)) -> (a b0 c0 -> a b1 c1)
withOpenFirstIO f = closeFirstIO . f . openFirstIO

withOpenSecondIO :: (Arrow a) => (a (b0, ()) (c0, ()) -> a (b1, ()) (c1, ())) -> (a b0 c0 -> a b1 c1)
withOpenSecondIO f = closeSecondIO . f . openSecondIO

concatFirst :: [([a], b)] -> [(a, b)]
concatFirst = concat . map (\(ys, b) -> map (\y -> (y, b)) ys)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

split :: [(a, b)] -> ([a], [b])
split xs = (map fst xs, map snd xs)

trueAsIntegralI :: Integer
trueAsIntegralI = 1

falseAsIntegralI :: Integer
falseAsIntegralI = 0

trueAsIntegral :: (Integral i) => i
trueAsIntegral = fromIntegral $ trueAsIntegralI

falseAsIntegral :: (Integral i) => i
falseAsIntegral = fromIntegral $ falseAsIntegralI

deconsMaybe :: r -> (a -> r) -> Maybe a -> r
deconsMaybe withNothing _ (Nothing) = withNothing
deconsMaybe _ withJust    (Just a)  = withJust a

-- | A convenient way to deconstruct a maybe with a default value.
morElse :: Maybe a -> a -> a
morElse = flip fromMaybe

{-
-- | Empty Eq, Ord, Show instance.
--
-- Useful to hide a component from a record that otherwise implements these
-- instances, e.g. a utility function like ‘spaLumpGetVertexAdjacents’ not
-- essential to the record.  Alternatively, these utility functions could be
-- moved from the record, so that they take an additional input argument of the
-- record.
type AssumeEOS a = a
instance Eq (AssumeEOS a) where _ == _ = True
instance Ord (AssumeEOS a) where _ <= _ = True
instance Show (AssumeEOS a) where show _ = "(AssumeEOS)"
-}

setMapFilter :: (Ord a, Ord b) => (a -> Maybe b) -> S.Set a -> S.Set b
setMapFilter f s =
	S.map (\x -> case x of
		Nothing -> error "Internal error: setMapFilter found a Nothing after removing all Nothings."
		Just a -> a) .
	S.filter (\x -> case x of
		Nothing -> False
		Just _  -> True) .
	S.map f $
	s

-- | Find the mean value.
--
-- 	  (lastLen*lastMean + x)/(lastLen + 1)
-- 	= (lastLen/(lastLen + 1))*lastMean + x/(lastLen + 1)
steppingMean :: (Foldable t, Num a, Fractional a) => t a -> a
steppingMean = snd . foldr (\x (lastLen, lastMean) -> let lastLenP1 = lastLen + 1 in (lastLenP1, (lastLen/lastLenP1)*lastMean + x/lastLenP1)) (0, 0)

-- | Considers all functions equal.  Compiler record lookup obtains a different
-- record for Eq, Ord, Show instance values.
newtype FakeEOS a = FakeEOS { _fakeEOS :: a }
makeLenses ''FakeEOS
instance Eq (FakeEOS a) where _ == _ = True
instance Ord (FakeEOS a) where _ <= _ = True
instance Show (FakeEOS a) where show _ = "(FakeEOS)"

-- | 'mod' generalized to Double and floats.
modfl :: (RealFrac a) => a -> a -> a
modfl a b = a - b*(fromInteger . floor $ a/b)

-- | 3-ary 'uncurry'.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | Given e.g. a list [1,2,3], return [(1,[2,3]), (2,[1,3]), (3,[1,2])].
listOthers :: [a] -> [(a, [a])]
listOthers []     = []
listOthers (x:xs) = (x,xs) : fmap (second (x:)) (listOthers xs)

-- | Run a ListT monad.
--
-- ListT is provided by the ‘pipes’ package.
runListT :: (Monad m) => P.ListT m a -> m [a]
runListT (P.Select m) = P.toListM m

-- | Lift a plain list to a ListT monad.
--
-- ListT is provided by the ‘pipes’ package.
liftList :: (Functor m) => [a] -> P.ListT m a
liftList = P.Select . P.each
