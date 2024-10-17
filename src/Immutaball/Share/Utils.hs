{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances, DerivingVia #-}

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
		withOpenSecondIO
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad.Fix
import Data.Functor.Compose
import Data.List

import Control.Lens

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
