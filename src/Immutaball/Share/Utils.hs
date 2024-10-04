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
		safeTail
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Data.Functor.Compose

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
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs
