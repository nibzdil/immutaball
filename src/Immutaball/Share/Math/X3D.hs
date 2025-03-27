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

		Line3(..), p0l3, p1l3,
		line3Points,
		line3Axes,
		ol3,
		a0l3,
		line3NormalizeDisplacement,
		line3DistanceFromOrigin,

		plane3PointDistance,
		plane3LineSegmentDistance
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

-- | Given a plane, how far is a point from it in absolute value?
--
-- Returns negative if the point is behind the plane (e.g. you need to multiply
-- a negative number by the plane's normal to get from a point on the plane to
-- that point).
--
-- This can be done by projecting the point onto the plane's unit normal
-- (normalness is assumed and not verified here - verification would be a lot
-- easier with dependent types), and then finding projection length minus plane
-- distance.
plane3PointDistance :: forall a. (Num a) => Plane3 a -> Vec3 a -> a
plane3PointDistance p v = projectionLength - (p^.dp3)
	where
		projectionLength = v `d3` (p^.abcp3)

-- | A line segment or infinite line in 3D space.
--
-- A line segment can be represented by its 2 endpoints, as arbitrary points in
-- 3D space each relevant to the origin.  In this representation, we use the
-- names ‘p0’ and ‘p1’.
--
-- An infinite line in 3D can also be represented by a scaled normal vector,
-- like a plane representation, paired with an axis for the line.  This
-- representation can also be used to construct a matrix with 2 columns, the
-- final column being the homogoneous displacement to an arbitrary point on the
-- line, to represent the line.  This representation furthermore can be
-- ‘normalized’ by adjusting the displacement second column so that it points
-- to the closest point on that line, ensuring that the columns are orthogonal.
-- This normalized offset+axis representation gives you the convenient ability
-- to find the distance of the infinite line to the origin, by simply taking
-- the r3 (vector length) of the normalized displacement vector (2nd col),
-- ignoring the final extra ‘1’ coord if using homogenous coords consistently.
-- Additionally this lets you represent points on that line with a single
-- Double, by projecting it onto that plane by letting multiplying the Double
-- with the axis, and then adding the displacement vector to get onto the line.
--
-- In the axes representation, we use the names ‘o’ and ‘a0’.  If ‘o’ is
-- normalized and orthogonal to ‘a0’ (and is thus the closest point of the line
-- to the origin), we may also use the name ‘n’.  (Note: it is a _scaled_
-- normal in this case, not necessarily of unit length.)
--
-- Finally, another alternative way to represent a line _segment_ is similar
-- to the infinite line projection representation just described, except the
-- displacement is taken to be p0, the first point, and the axis is taken to be
-- p1-p0: 1*axis gets you from p0 to p1.  In this case, normalization is not
-- expected.
data Line3 a = Line3 {
	_p0l3 :: Vec3 a,
	_p1l3 :: Vec3 a
}
	deriving (Eq, Ord, Show)
makeLenses ''Line3

-- | Construct a line from 2 points.
line3Points :: Vec3 a -> Vec3 a -> Line3 a
line3Points p0 p1 = Line3 p0 p1

-- | Construct a line from a displacement vector and axis vector.
--
-- The vector doesn't have to be orthogonal to the axis like a normal.
line3Axes :: forall a. (Num a) => Vec3 a -> Vec3 a -> Line3 a
line3Axes p0 a0 = Line3 p0 (p0 `pv3` a0)

-- | Access a line's displacement vector while preserving its axis.
ol3 :: forall a. (Num a) => Lens' (Line3 a) (Vec3 a)
ol3 = lens getter (flip setter)
	where
		getter :: Line3 a -> Vec3 a
		getter (Line3 p0 _p1) = p0
		setter :: Vec3 a -> Line3 a -> Line3 a
		setter n (Line3 p0 p1) = Line3 n (n `pv3` (p1 `minusv3` p0))

-- | The first axis after the normal/displacement vector for this line.
a0l3 :: forall a. (Num a) => Lens' (Line3 a) (Vec3 a)
a0l3 = lens getter (flip setter)
	where
		getter :: Line3 a -> Vec3 a
		getter (Line3 p0 p1) = p1 `minusv3` p0
		setter :: Vec3 a -> Line3 a -> Line3 a
		setter a0 (Line3 p0 _) = Line3 p0 $ p0 `pv3` a0

-- | Normalize a line's p0/n to be the closest point to the origin.
--
-- a0 is preserved; it is not also normalized by this.
--
-- This is done by the formula:
-- 	n = o - v*(o dot a0)/(|a0|^2)
-- Meaning original displacement vector minus its projection onto the unit ‘a0’
-- axis vector.
-- Note:
-- 	s = (-o dot v)/(|v|^2)
line3NormalizeDisplacement :: forall a. (Num a, Fractional a, RealFloat a) => Line3 a -> Line3 a
line3NormalizeDisplacement l = l & a0l3 .~ ((((l^.ol3) `d3` (l^.a0l3))/(sq (l^.a0l3.r3))) `sv3` (l^.a0l3))
	where sq x = x*x

-- | Find the distance from the origin to an infinite line.
line3DistanceFromOrigin :: forall a. (Num a, Fractional a, RealFloat a) => Line3 a -> a
line3DistanceFromOrigin l = (line3NormalizeDisplacement l)^.ol3.r3

-- | Given a line segment representation by its 2 endpoints, find its distance
-- to the plane in absolute value.  (Negative if closest is behind the plane.)
-- The result is 0 if it intersects the plane.
--
-- (An infinite line in 3D can also be represented by a scaled normal vector,
-- like a plane representation, paired with an axis for the line.  This
-- representation can also be used to construct a matrix with 2 columns, the
-- final column being the homogoneous displacement to an arbitrary point on the
-- line, to represent the line.  This representation furthermore can be
-- ‘normalized’ by adjusting the displacement second column so that it points
-- to the closest point on that line, ensuring that the columns are orthogonal.
-- This normalized offset+axis representation gives you the convenient ability
-- to find the distance of the infinite line to the origin, by simply taking
-- the r3 (vector length) of the normalized displacement vector (2nd col),
-- ignoring the final extra ‘1’ coord if using homogenous coords consistently.
-- Additionally this lets you represent points on that line with a single
-- Double, by projecting it onto that plane by letting multiplying the Double
-- with the axis, and then adding the displacement vector to get onto the line.)
--
-- (Finally, another alternative way to represent a line _segment_ is similar
-- to the infinite line projection representation just described, except the
-- displacement is taken to be p0, the first point, and the axis is taken to be
-- p1-p0: 1*axis gets you from p0 to p1.  In this case, normalization is not
-- expected.)
plane3LineSegmentDistance :: forall a. (Num a, Fractional a, RealFloat a) => Plane3 a -> Line3 a -> a
plane3LineSegmentDistance p l
	| p0d >= 0 && p1d >= 0 = min p0d p1d
	| p0d <= 0 && p1d <= 0 = max p0d p1d
	| otherwise            = 0
	where
		p0d = plane3PointDistance p (l^.p0l3)
		p1d = plane3PointDistance p (l^.p1l3)
