{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}  -- SmallishInfiniteLineThreshold

-- | Dependent types might make for a funner linear algebra implementation, so
-- I just stick with what's most applicable for our uses and goals here.
module Immutaball.Share.Math.X3D
	(
		Plane3(..), unPlane3,
		normalPlane3,
		abcdp3,
		abcp3,
		dp3,

		defaultOrientationPlane3,
		plane3PointDistance,
		pointToPlane,
		plane3ReflectPoint,
		plane3ReflectPointAmount,
		normalizePlane3,
		negatePlaneOrientation,

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
		smallishInfiniteLineThresholdd,
		smallishInfiniteLineThresholdf,
		SmallishInfiniteLineThreshold(..),
		line3CoordAtDistancePlane3,
		line3PlaneIntersection,

		line3PointCoord,
		line3PointDistance,
		line3DistanceCoordFromPoint,
		line3Line3ClosestCoords,
		line3Line3ClosestCoordsAny,
		line3Line3Distance,

		eqPlane3,
		eqLine3,
		nearPlane3,
		nearLine3,

		eqPlane3PointsOnly,
		eqLine3PointsOnly,
		nearPlane3PointsOnly,
		nearLine3PointsOnly,

		plane3Plane3,

		QCurve3(..), a2q3, a1q3, a0q3,
		qcurve3,
		qcurvePath3,
		pathq3,
		qcurve3Qerp,
		qcurve3Vel,
		qcurve3Accel
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens

import Immutaball.Share.Math.Core
import Immutaball.Share.Utils

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
makeLenses ''Plane3

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
plane3ReflectPoint p v = plane3ReflectPointAmount p v 1

-- | Reflect a point about a plane by a certain amount.
plane3ReflectPointAmount :: forall a. (Num a) => Plane3 a -> Vec3 a -> a -> Vec3 a
plane3ReflectPointAmount p v a = v `pv3` ((1 + a) `sv3` (pointToPlane v p `minusv3` v))

-- | Construct a plane from an arbitrary point on that plane and a normal.
--
-- The unit-lengthedness of the provided normal is assumed and not verified.
normalizePlane3 :: forall a. (Num a) => Vec3 a -> Vec3 a -> Plane3 a
normalizePlane3 v abc = normalPlane3 abc d
	where
		d :: a
		d = v `d3` abc

-- | Keep the plane representing the same points, but negate the
-- direction of the normal.
negatePlaneOrientation :: forall a. (Num a) => Plane3 a -> Plane3 a
negatePlaneOrientation =
	(abcp3 %~ negate) .
	(dp3   %~ negate) .
	id

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
--
-- ‘o’ could also be named ‘p0’.
line3Axes :: forall a. (Num a) => Vec3 a -> Vec3 a -> Line3 a
line3Axes o a0 = Line3 o (o `pv3` a0)

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
-- 	n = o - a0*(o dot a0)/(|a0|^2)
-- Meaning original displacement vector minus its projection onto the unit ‘a0’
-- axis vector.
-- Note:
-- 	s = (-o dot v)/(|v|^2)
line3NormalizeDisplacement :: forall a. (Num a, Fractional a, RealFloat a) => Line3 a -> Line3 a
line3NormalizeDisplacement l = l & ol3 %~ subtract ((((l^.ol3) `d3` (l^.a0l3))/(sq_ (l^.a0l3.r3))) `sv3` (l^.a0l3))
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
--line3AxisReflectPlane3 l abc = l & p1l3 %~ plane3ReflectPoint (normalizePlane3 (l^.ol3) abc)  -- (Alternative.)
line3AxisReflectPlane3 l abc = l & a0l3 %~ plane3ReflectPoint (normalizePlane3 zv3 abc)

-- | Project a 1D coord x onto a line, like a lerp.
--
-- The coord 0 corresponds to p0, and the coord 1 corresponds to p1.  0.5 is
-- half-way in between, and 2 is p0 + 2*(p1 - p0), i.e. p0 + 2*a0.
line3Lerp :: forall a. (Num a) => Line3 a -> a -> Vec3 a
line3Lerp l x = (l^.ol3) `pv3` (x `sv3` (l^.a0l3))

smallishInfiniteLineThresholdd :: Double
smallishInfiniteLineThresholdd = 0.001

smallishInfiniteLineThresholdf :: Float
smallishInfiniteLineThresholdf = 0.001

class SmallishInfiniteLineThreshold a where smallishInfiniteLineThreshold :: a
instance {-# OVERLAPPING  #-} SmallishInfiniteLineThreshold Double where smallishInfiniteLineThreshold = smallishInfiniteLineThresholdd
instance {-# OVERLAPPING  #-} SmallishInfiniteLineThreshold Float where smallishInfiniteLineThreshold = smallishInfiniteLineThresholdf
instance {-# OVERLAPPABLE #-} (Fractional a) => SmallishInfiniteLineThreshold a where smallishInfiniteLineThreshold = realToFrac $ smallishInfiniteLineThresholdf

-- | Given a plane, find the 1D coord that projects onto the line, that finds
-- the point at the given distance from the plane.  i.e. for a line and plane,
-- find x such that p0 + p1*x gives a point equal to distance d from the plane
-- (like a lerp).  However return Nothing, if the line is parallel and
-- thus every point is the same distance.
--
-- Conveniently, the change in distance from the plane per change in x is
-- constant.
--
-- Beware the sign of the distance.
--
-- This is better suited for lines with non-trivial length.  See 'SmallishInfiniteLineThreshold'.
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

-- | Find the point on the plane where an infinite line intersects.
--
-- This is better suited for lines with non-trivial length.  See 'SmallishInfiniteLineThreshold'.
line3PlaneIntersection :: forall a. (SmallNum a, Ord a, Num a, Fractional a) => Line3 a -> Plane3 a -> Maybe (Vec3 a)
line3PlaneIntersection l p = line3Lerp l <$> line3CoordAtDistancePlane3 p l 0

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
line3PointCoord l v = v' `d3` (l^.a0l3) / sqx (l^.a0l3.r3)
	where
		v' = v `minusv3` (l^.ol3)

-- | Find the distance from the line (by the closest point on the line) to the
-- given point in space.
line3PointDistance :: forall a. (Num a, Fractional a, RealFloat a) => Line3 a -> Vec3 a -> a
line3PointDistance l v = (v - line3Lerp l (line3PointCoord l v))^.r3

-- | Relative to the closest point on the line, find what coord on the line
-- represents the point on the line exactly 'distance' distance away from the
-- point.  You can negate the coord offset and still be at the same distance.
--
-- Use this to find where on a line you would be at a given distance from the
-- provided point.  Add it to 'line3PointCoord' to find the net coord.
--
-- It works by applying the Pythagorean theorem to the triangle with hypotenuse
-- distance from 'v' to the point on the line at a distance, and with sides
-- ‘distance of point to line’ and ‘distance along the line to the desired
-- point’, finally scaling according to the line axis length so the coord is
-- correct.
--
-- Note that if the point is further from the line than the given distance,
-- your Haskell implementation may return NaN as it tries to take the sqrt of a
-- negative number.
line3DistanceCoordFromPoint :: forall a. (Num a, Fractional a, RealFloat a) => Line3 a -> Vec3 a -> a -> a
line3DistanceCoordFromPoint l v distance = (sqrt $ sqx distance - sqx (line3PointDistance l v)) / (l^.a0l3.r3)

-- | Given lines la and lb, find coords ‘ax’ and ‘bx’ on la and on lb
-- representing the points at which the two infinite lines are closest to each
-- other.
--
-- (Paralleling the order, the first coord is on the first line, and the second
-- coord is on the second line.)
--
-- If they are parallel, return Nothing, indicating any pair of points on the
-- lines has an equal distance.
--
-- Find the unique pair of points on the lines such that the line segment
-- between them is orthogonal to both lines or else where the points are equal
-- (i.e. the lines intersect).
--
-- To see how this can be implemented, first we can find the direction of the
-- line segment with p0 as the closest point on the first line and p1 as the
-- closest point on the second line, after assuming for now the lines don't
-- intersect.  We can use the pattern in defaultOrientationPlane3 here, which
-- here is essentially the cross product of the 2 line axes.  This new axis
-- will be the direction of this ‘closest’ line segment ‘lc’, but we don't yet
-- know its scale (and could be negative), or the ‘ol3’ of it: where to
-- position its start.
--
-- If we pick an arbitrary point on line ‘la’, which may as well be ‘ol3’, and
-- then try applying the tentative ‘closest’ ‘lc’ line segment axis found by
-- cross product, we'll probably find it doesn't point to a point on ‘lb’, but
-- sticks out somewhere else.  We need to find the right ‘ol3’ to translate
-- this ‘lc’ axis (as well as the length of the axis).  Conveniently, the
-- closest distance from the end of this tentative ‘lc’ ‘stick’ to ‘lb’ changes
-- at a constant proportion as ‘ax’ changes.  So we just need to use this
-- change, 'ddist/dax', to find how much we need to adjust our initial guess at
-- ‘la^.ol3’ to get to connect on the second line.  This lets us find ‘ax’.
--
-- To help visualize finding ‘ddist/dax’, consider projecting onto a plane by
-- looking down the axis we found.  If we assume the length of the ‘lc’ is
-- correct, then we can look at the axes of the 2 lines, intersecting the
-- origin, in 2D space, in this projection.  As we move along ‘la’, we can look
-- at the distance from our location to (via the ‘stick’ which we can now
-- ignore) ‘lb’.  If ‘la’ is pointing right, and ‘lb’ is pointing upright at a
-- slant, then we can just advance on ‘la’ by 1, where ax=1 and the 2D coord is
-- (1, 0), and simply look at distance of our location from ‘lb’ (the closest
-- point).  Since the distance was 0 at ax=0, this distance at ax=1 will be
-- ‘ddist/dax’.  (Take the normal of ‘lb’, dot product projection on the unit
-- normal, and find distance from origin to get the distance, or just use
-- 'line3PointDistance'.)
--
-- Possibly there are more efficient ways to do this.
line3Line3ClosestCoords :: forall a. (Show a, SmallNum a, Fractional a, RealFloat a) => Line3 a -> Line3 a -> Maybe (a, a)
line3Line3ClosestCoords la lb
	| ddist_dax `equivalentSmall` 0 = Nothing
	| otherwise                     = Just (ax, bx)
	where
		-- | The axis of the tentative ‘stick’ coming from ‘la’ to likely not a
		-- point on ‘lb’, but still has the right direction (possibly negated)
		-- of the line going from the closest point on ‘la’ to the closest
		-- point on ‘lb’.
		lca0 :: Vec3 a
		lca0 = (la^.a0l3) `vx3` (lb^.a0l3)
		-- | The ‘stick’.
		_lc :: Line3 a
		_lc = line3Axes (la^.p0l3) lca0

		-- | Find vector orthogonal to ‘lc’ and ‘lb’'s axis.  This lets us
		-- project the ‘stick’'s end-point (p1) coming from ‘la’ to get the
		-- distance if the ‘stick’ had the right scale/magnitude, i.e. if ‘lc’
		-- had the right magnitude.  We can find the point on ‘lb’ closest to
		-- the the stick's end-point (lc^.p1), and project onto this
		-- lbProjectionNormal with the dot product to find the distance we're
		-- looking for.  We'll project the vector going from closest point to
		-- the stick's end-point, onto lbProjectionNormal, producing the
		-- distance.
		lbProjectionNormal = v3normalize $ lca0 `vx3` (lb^.a0l3)

		-- | For a given ax, find the distance from adding ‘lc’ at the correct
		-- scale to the closest point on ‘lb’, using lbProjectionNormal.
		distanceAtAx ax_ = theDistance
			where
				stickEndPoint = line3Lerp la ax_ + lca0
				closestLbPointToEndPoint = line3Lerp lb . line3PointCoord lb $ stickEndPoint  -- (stickendpoint is orthogonal, so both stickEndPoint and correctedStickEndPoint would yield equivalent results.)
				correctedStickEndPoint = correctedStickEndPointDistance `sv3` lbProjectionNormal
				correctedStickEndPointDistance = (stickEndPoint - closestLbPointToEndPoint) `d3` lbProjectionNormal

				theDistance = correctedStickEndPointDistance
				_theDistanceAlternative = line3PointDistance lb $ correctedStickEndPoint

		ddist_dax :: a
		ddist_dax = distanceAtAx 1 - distanceAtAx 0

		-- | f(0) = c && f'(x) = constant_of_f' ddist_dax => f(-f(0)/f'(0)) = 0
		ax = -distanceAtAx 0 / ddist_dax

		bx = line3PointCoord lb $ line3Lerp la ax

-- | Like 'line3Line3ClosestCoords', but if the lines are parallel, such that
-- there are infinitely many close points, pick as an arbitrary point la at
-- coord ax=0, and lb at the closest point.
line3Line3ClosestCoordsAny :: forall a. (Show a, SmallNum a, Fractional a, RealFloat a) => Line3 a -> Line3 a -> (a, a)
line3Line3ClosestCoordsAny la lb
	| Just (ax, bx) <- line3Line3ClosestCoords la lb
	{-
	, not (isNaN ax || isInfinite ax)
	, not (isNaN bx || isInfinite bx) =
	-} =
		(ax, bx)
	| otherwise =
		let ax = 0.0 in
		let bx = line3PointCoord lb $ line3Lerp la ax in
		(ax, bx)

-- | Find the (closest) distance between 2 infinite lines.
line3Line3Distance :: forall a. (Show a, SmallNum a, Fractional a, RealFloat a) => Line3 a -> Line3 a -> a
line3Line3Distance la lb = case line3Line3ClosestCoords la lb of
	Nothing          -> line3PointDistance la $ line3Lerp lb 0
	Just    (ax, bx) -> (line3Lerp lb bx - line3Lerp la ax)^.r3

-- The orientation is checked, for these functions.

eqPlane3 :: (SmallNum a, Ord a, Num a, RealFloat a) => Plane3 a -> Plane3 a -> Bool
eqPlane3 a b = (a^.unPlane3) `eq4` (b^.unPlane3)

eqLine3 :: (SmallNum a, Ord a, Num a, RealFloat a) => Line3 a -> Line3 a -> Bool
eqLine3 a b = (a^.p0l3) `eq3` (b^.p0l3) && (a^.p1l3) `eq3` (b^.p1l3)

nearPlane3 :: (SmallishNum a, Ord a, Num a, RealFloat a) => Plane3 a -> Plane3 a -> Bool
nearPlane3 a b = (a^.unPlane3) `near4` (b^.unPlane3)

nearLine3 :: (SmallishNum a, Ord a, Num a, RealFloat a) => Line3 a -> Line3 a -> Bool
nearLine3 a b = (a^.p0l3) `near3` (b^.p0l3) && (a^.p1l3) `near3` (b^.p1l3)

-- These equivalence variants check the points only; you can e.g. use
-- ‘negatePlaneOrientation’ and preserve equivalence.  (That is, flipping the
-- plane normal doesn't necessarily change which points are on the plane.)

eqPlane3PointsOnly :: (SmallNum a, Ord a, Num a, RealFloat a) => Plane3 a -> Plane3 a -> Bool
eqPlane3PointsOnly a b = let a' = negatePlaneOrientation a in (a^.unPlane3) `eq4` (b^.unPlane3) || (a'^.unPlane3) `eq4` (b^.unPlane3)

eqLine3PointsOnly :: (SmallNum a, Ord a, Num a, RealFloat a) => Line3 a -> Line3 a -> Bool
eqLine3PointsOnly a b = let na' = na & (a0l3 %~ negate) in ( (na^.p0l3) `eq3` (nb^.p0l3) && (na^.p1l3) `eq3` (nb^.p1l3) ) || ( (na'^.p0l3) `eq3` (nb^.p0l3) && (na'^.p1l3) `eq3` (nb^.p1l3) )
	where (na, nb) = let standardize = (a0l3 %~ v3normalize) . line3NormalizeDisplacement in (standardize a, standardize b)

nearPlane3PointsOnly :: (SmallishNum a, Ord a, Num a, RealFloat a) => Plane3 a -> Plane3 a -> Bool
nearPlane3PointsOnly a b = let a' = negatePlaneOrientation a in (a^.unPlane3) `near4` (b^.unPlane3) || (a'^.unPlane3) `near4` (b^.unPlane3)

nearLine3PointsOnly :: (SmallishNum a, Ord a, Num a, RealFloat a) => Line3 a -> Line3 a -> Bool
nearLine3PointsOnly a b = let na' = na & (a0l3 %~ negate) in ( (na^.p0l3) `near3` (nb^.p0l3) && (na^.p1l3) `near3` (nb^.p1l3) ) || ( (na'^.p0l3) `near3` (nb^.p0l3) && (na'^.p1l3) `near3` (nb^.p1l3) )
	where (na, nb) = let standardize = (a0l3 %~ v3normalize) . line3NormalizeDisplacement in (standardize a, standardize b)

-- | Find the the line where 2 planes intersect, if they are not parallel.
plane3Plane3 :: (Num a, RealFloat a, Floating a, Ord a, SmallNum a) => Plane3 a -> Plane3 a -> Maybe (Line3 a)
plane3Plane3 pa pb = do
	-- Get a0, the axis of the line, from the cross product of the 2 plane
	-- normals.
	let a0Dir = (pa^.abcp3) `vx3` (pb^.abcp3)
	let a0DirUnit = v3or $ v3normalize a0Dir
	a0 <- if' (min (a0Dir^.r3) (a0DirUnit^.r3) <= smallNum) Nothing $ Just a0DirUnit

	-- To find any point where the planes intersect, take a point on pa, make a
	-- line from it using pa's normal cross a0, and find where this line
	-- intersects pb.  Use this arbitrary origin as an arbitrary point on the
	-- line, and then normalize the displacement so that it's a common form.
	let pap = pointToPlane zv3 pa                   -- Closest point on pa to the origin.
	let lab = line3Axes pap ((pa^.abcp3) `vx3` a0)  -- Line from pap to some point on the line.
	lp0 <- line3PlaneIntersection lab pb            -- An arbitrary point where pa intersects pb.

	let lPrenormalized = line3Axes lp0 a0
	let l              = line3NormalizeDisplacement lPrenormalized

	return $ l

-- | The curve represented by p = 1/2 (x^2) a2 + x a1 + a0
-- (The 1/2 comes from integrating twice.)
data QCurve3 a = QCurve3 {
	_a2q3 :: Vec3 a,  -- ^ Acceleration.
	_a1q3 :: Vec3 a,  -- ^ Velocity (linear).
	_a0q3 :: Vec3 a   -- ^ Position (starting position at x=0).
}
	deriving (Eq, Ord, Show)
makeLenses ''QCurve3

-- | Make a quadratic curve by an acceleration vector, a velocity vector, and a
-- starting position.
qcurve3 :: Vec3 a -> Vec3 a -> Vec3 a -> QCurve3 a
qcurve3 a2 a1 a0 = QCurve3 a2 a1 a0

-- | Make a quadratic curve by an acceleration vector and a line.
qcurvePath3 :: forall a. (Num a) => Vec3 a -> Line3 a -> QCurve3 a
qcurvePath3 a2 path = QCurve3 a2 (path^.a0l3) (path^.ol3)

-- | A lens of QCurve3 for the path: the velocity and position as a path line,
-- ignoring the acceleration component.
pathq3 :: forall a. (Num a) => Lens' (QCurve3 a) (Line3 a)
pathq3 = lens getter (flip setter)
	where
		getter :: QCurve3 a -> Line3 a
		getter (QCurve3 _ a1 a0) = line3Axes a0 a1
		setter :: Line3 a -> QCurve3 a -> QCurve3 a
		setter path (QCurve3 a2 _ _) = QCurve3 a2 (path^.a0l3) (path^.ol3)

-- | Convert a coordinate on the curve to a point:
-- p = 1/2 (x^2) a2 + x a1 + a0
qcurve3Qerp :: forall a. (Num a, Fractional a) => QCurve3 a -> a -> Vec3 a
qcurve3Qerp q x = ((1/2) * (sqx x)) `sv3` (q^.a2q3) + x `sv3` (q^.a1q3) + (q^.a0q3)

-- | Find the velocity at a coord.
-- v = x a2 + a1
qcurve3Vel :: forall a. (Num a) => QCurve3 a -> a -> Vec3 a
qcurve3Vel q x = x `sv3` (q^.a2q3) + (q^.a1q3)

-- | Find the acceleration at a coord.
-- v = a2
qcurve3Accel :: forall a. (Num a) => QCurve3 a -> a -> Vec3 a
qcurve3Accel q _x = (q^.a2q3)
