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
		normalPlane3,
		abcdp3,
		abcp3,
		dp3,

		defaultOrientationPlane3,
		plane3PointDistance,
		pointToPlane,
		plane3ReflectPoint,
		normalizePlane3,

		Line3(..), p0l3, p1l3,
		line3Points,
		line3Axes,
		ol3,
		a0l3,
		line3NormalizeDisplacement,
		line3DistanceFromOrigin,

		plane3LineSegmentDistance,
		line3AxisReflectPlane3,

		line3Lerp,
		line3CoordAtDistancePlane3,

		line3PointCoord,
		line3PointDistance
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

-- | Construct a plane from a unit normal (unit-lengthedness unverified) and a
-- distance from the origin.
normalPlane3 :: forall a. (Num a) => Vec3 a -> a -> Plane3 a
normalPlane3 abc d = Plane3 $ Vec4 (abc^.x3) (abc^.y3) (abc^.z3) d

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
--
-- This can also be used to detect whether a point is on front of, on, or
-- behind a plane, by checking its sign.
plane3PointDistance :: forall a. (Num a) => Plane3 a -> Vec3 a -> a
plane3PointDistance p v = projectionLength - (p^.dp3)
	where
		projectionLength = v `d3` (p^.abcp3)

-- | Get the point on the plane closest to the point in 3D space.
--
-- Project onto the unit normal (unit-lengthedness assumed and unverified),
-- find the change in distance from the plane to the point, and subtract a
-- scaled normal based on this from the original point.
pointToPlane :: forall a. (Num a) => Vec3 a -> Plane3 a -> Vec3 a
pointToPlane v p = v `minusv3` (plane3PointDistance p v `sv3` (p^.abcp3))

-- | Reflect a point about a plane.
plane3ReflectPoint :: forall a. (Num a) => Plane3 a -> Vec3 a -> Vec3 a
plane3ReflectPoint p v = v `pv3` (2 `sv3` (pointToPlane v p `minusv3` v))

-- | Construct a plane from an arbitrary point on that plane and a normal.
--
-- The unit-lengthedness of the provided normal is assumed and not verified.
normalizePlane3 :: forall a. (Num a) => Vec3 a -> Vec3 a -> Plane3 a
normalizePlane3 v abc = normalPlane3 abc d
	where
		d :: a
		d = v `d3` abc

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
line3NormalizeDisplacement l = l & a0l3 .~ ((((l^.ol3) `d3` (l^.a0l3))/(sq_ (l^.a0l3.r3))) `sv3` (l^.a0l3))
	where sq_ x = x*x

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

-- | Reflect a line's axis about the plane with the given normal and
-- intersecting ‘o’ (‘p0’).
--
-- This can be done by finding the vector from a0 to the closest point on the
-- plane, and adding double that vector.
line3AxisReflectPlane3 :: forall a. (Num a) => Line3 a -> Vec3 a -> Line3 a
line3AxisReflectPlane3 l abc = l & a0l3 %~ plane3ReflectPoint (normalizePlane3 (l^.ol3) abc)

-- | Project a 1D coord x onto a line, like a lerp.
--
-- The coord 0 corresponds to p0, and the coord 1 corresponds to p1.  0.5 is
-- half-way in between, and 2 is p0 + 2*(p1 - p0), i.e. p0 + 2*a0.
line3Lerp :: forall a. (Num a) => Line3 a -> a -> Vec3 a
line3Lerp l x = (l^.ol3) `pv3` (x `sv3` (l^.a0l3))

-- | Given a plane, find the 1D coord that projects onto the line, that finds
-- the point at the given distance from the plane.  i.e. for a line and plane,
-- find x such that p0 + p1*x gives a point equal to distance d from the plane
-- (like a lerp).  However return Nothing, if the line is parallel and
-- thus every point is the same distance.
--
-- Conveniently, the change in distance from the plane per change in x is
-- constant.
line3CoordAtDistancePlane3 :: forall a. (SmallNum a, Ord a, Num a, Fractional a) => Plane3 a -> Line3 a -> a -> Maybe a
line3CoordAtDistancePlane3 p l d
	| dd_dx `equivalentSmall` 0 = Nothing
	| otherwise                 = Just $ (d - p0d)/dd_dx
	where
		-- Going from p0 to p1 (i.e. for every change in x by +1), how much
		-- does the distance from the plane change?
		dd_dx :: a
		dd_dx = p1d - p0d

		p0d = plane3PointDistance p (l^.p0l3)
		p1d = plane3PointDistance p (l^.p1l3)

-- | Find the coord on the line to the closest point on that line to the given
-- point in 3D space.
--
-- You can use the result with 'line3Lerp' to obtain the point in 3D space.
--
-- By projecting the arbitrary point onto the line's unit axis, we find the
-- length along the axis to the closest point, and then divide by the length of
-- the line's axis to obtain the coord.
--
-- Non-zeroness of the line's axis is assumed and not verified.
line3PointCoord :: forall a. (Num a, Fractional a, RealFloat a) => Line3 a -> Vec3 a -> a
line3PointCoord l v = v' `d3` (nl^.a0l3) / sqx (nl^.a0l3.r3)
	where
		nl = line3NormalizeDisplacement l
		v' = v `minusv3` (nl^.ol3)

-- | Find the distance from the line (by the closest point on the line) to the
-- given point in space.
line3PointDistance :: forall a. (Num a, Fractional a, RealFloat a) => Line3 a -> Vec3 a -> a
line3PointDistance l v = (v - line3Lerp l (line3PointCoord l v))^.r3
