{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia, ScopedTypeVariables #-}

-- | Dependent types might make for a funner linear algebra implementation, so
-- I just stick with what's most applicable for our uses and goals here.
module Immutaball.Share.Math.X3D
	(
		Plane3(..),
		abcdp3,
		abcp3,
		dp3,

		defaultOrientationPlane3,
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens

import Immutaball.Share.Math.Core

-- | Normal, distance.
--
-- > ax + by + cd = d
--
-- abc is the unit normal vector, and d is the distance of the plane from the
-- origin.  The dot product of a point onto the unit normal projects onto it
-- and must be equal to the plane's closest distance from the origin.
newtype Plane3 a = Plane3 { _unPlane3 :: Vec4 a }
	deriving (Eq, Ord, Show)
		via (Vec4 a)

-- | Unit-lengthedness of the normal is not verified.
abcdp3 :: forall a. Lens' (Plane3 a) (Vec4 a)
abcdp3 = lens getter (flip setter)
	where
		getter :: Plane3 a -> Vec4 a
		getter (Plane3 v) = v
		setter :: Vec4 a -> Plane3 a -> Plane3 a
		setter v _ = Plane3 v

-- | Plane normal lens.
abcp3 :: forall a. Lens' (Plane3 a) (Vec3 a)
abcp3 = abcdp3.xyz4

-- | Plane distance lens.
dp3 :: forall a. Lens' (Plane3 a) a
dp3 = abcdp3.w4

-- | Obtain a default ‘orientation’ (new axes in a transformation matrix, each
-- of which is orthogonal to each) for a plane by using a pattern that is like
-- a determinent (for that next axis) of increasing dimensionality for each
-- next axis for a hyperplane, copying from the initial axis for the remaining
-- components.  For a plane, we use the normal as the initial axis for the
-- derivation of the remaining axes.  (e.g. apply a determinant-like procedure
-- for a 2x2 matrix (copy coords 3 and later from initial), for the next axis,
-- then for the one after, 3x3 (copy coords 4 and later), then 4x4, etc.)  This
-- will also derive identity matrices.  It a generalization of cross product.
--
-- > a x x
-- > b x x
-- > c x x
--
-- > a -b  x
-- > b  a  x
-- > c  c  x  (Copy c, d, e, …, for the second axis.  -b and a come from the determinent-like procedure of increasing dimensionality.)
--
-- > a -b  ( bc - ac)
-- > b  a  (-ac - bc)
-- > c  c  (a^2 + b^2)
--
-- (Currently lacking a dependently typed proof, which would also help verify
-- correctness.)
--
-- The non-first columns can be used to project 2D points onto the plane, with
-- the _scaled_ normal (d * abc) in the translation column if homogenous
-- coordinatse are added.  This projection matrix would be 4x3, projecting 2D
-- points into 3D space on the plane.
defaultOrientationPlane3 :: forall a. (Num a) => Plane3 a -> Mat3 a
defaultOrientationPlane3 (Plane3 (Vec4 a b c _)) = Mat3 $ Vec3
	(Vec3 (a) (-b) ( b*c - a*c))
	(Vec3 (b) ( a) (-a*c - b*c))
	(Vec3 (c) ( c) ( a*a + b*b))
