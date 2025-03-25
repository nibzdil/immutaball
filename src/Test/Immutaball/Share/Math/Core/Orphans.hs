{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Immutaball.Share.Math.Core.Orphans
	(
	) where

import Prelude ()
import Immutaball.Prelude

import Test.QuickCheck

import Immutaball.Share.Math

instance (Arbitrary a) => Arbitrary (Vec2 a) where
	arbitrary = Vec2 <$> arbitrary <*> arbitrary
	shrink (Vec2 x y) = [Vec2 x' y' | (x', y') <- shrink (x, y)]

instance (Arbitrary a) => Arbitrary (Vec3 a) where
	arbitrary = Vec3 <$> arbitrary <*> arbitrary <*> arbitrary
	shrink (Vec3 x y z) = [Vec3 x' y' z' | (x', y', z') <- shrink (x, y, z)]

instance (Arbitrary a) => Arbitrary (Vec4 a) where
	arbitrary = Vec4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
	shrink (Vec4 x y z w) = [Vec4 x' y' z' w' | (x', y', z', w') <- shrink (x, y, z, w)]

instance (Arbitrary a) => Arbitrary (Mat3 a) where
	arbitrary = Mat3 <$> arbitrary
	shrink (Mat3 rows) = Mat3 <$> shrink rows

instance (Arbitrary a) => Arbitrary (Mat4 a) where
	arbitrary = Mat4 <$> arbitrary
	shrink (Mat4 rows) = Mat4 <$> shrink rows
