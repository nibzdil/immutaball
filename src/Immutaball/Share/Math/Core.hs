{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, InstanceSigs, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}

-- | Dependent types might make for a funner linear algebra implementation, so
-- I just stick with what's most applicable for our uses and goals here.
module Immutaball.Share.Math.Core
	(
		Vec2(..), x2, y2,
		rv2,
		pv2,
		sv2,
		minusv2,
		d2,
		co2,
		fr2,
		cm2,
		r2,
		t2,
		Vec3(..), x3, y3, z3,
		rv3,
		pv3,
		sv3,
		minusv3,
		d3,
		co3,
		fr3,
		cm3,
		r3,
		Vec4(..), x4, y4, z4, w4,
		rv4,
		pv4,
		sv4,
		minusv4,
		d4,
		co4,
		fr4,
		cm4,
		r4,
		Mat3(..), getMat3,
		Mat4(..), getMat4,
		r0_3,
		r1_3,
		r2_3,
		r0_4,
		r1_4,
		r2_4,
		r3_4,
		transposeMat3,
		transposeMat4,
		e0_0_4, e0_1_4, e0_2_4, e0_3_4,
		e1_0_4, e1_1_4, e1_2_4, e1_3_4,
		e2_0_4, e2_1_4, e2_2_4, e2_3_4,
		e3_0_4, e3_1_4, e3_2_4, e3_3_4,
		e0_0_3, e0_1_3, e0_2_3,
		e1_0_3, e1_1_3, e1_2_3,
		e2_0_3, e2_1_3, e2_2_3,
		mm3,
		mm4,
		c0_3,
		c1_3,
		c2_3,
		c0_4,
		c1_4,
		c2_4,
		c3_4,
		v3normalize,
		v3magnitude,
		v4normalize,
		v4magnitude,
		identity3,
		identity4,
		Rect(..), rectp1, rectp2,
		rectLowerLeft,
		rectUpperRight,
		rectCenter,
		rectTop,
		rectRight,
		rectBottom,
		rectLeft,
		rectUpperLeft,
		rectLowerRight,
		rectWidthAboutCenter,
		rectHeightAboutCenter,
		rectAvgSideAboutCenter,
		rectNormalize,
		isInRect,
		isInRect',
		lerpWith,
		lerp,
		lerpV2,
		lerpV3,
		lerpV4,
		ilerpWith,
		ilerp,
		tau,

		WidthHeightI,

		v4to3,
		v3to4,
		mv3,
		m4v3,
		mv4,
		vm3,
		vm4,
		v3m4,
		v2or,
		v2orWith,
		v3or,
		v3orWith,
		v4or,
		v4orWith,
		flor,
		florWith,
		vx3,
		v2perp,
		v3perp,

		SimpleRotation(..), srCcwAngle, srOriginAxis,
		Quaternion(..), qReal, qVector,
		qasv4,
		sq,
		pq,
		mq,
		qmi,
		qq,
		v3q,
		qcv3,
		srToQ,
		qnormalize,
		qmagnitude,
		qToSr,
		srToVec3,
		v3ToSr,
		simpleRotate,
		m3to4,
		scale3,
		scale3Simple,
		scale4,
		translate3,
		tilt3z,
		tilt3zSimple,
		tilt3zReverse,
		tilt3zReverseSimple,
		tilt3y,
		tilt3ySimple,
		tilt3yReverse,
		tilt3yReverseSimple,
		rotate3,
		rotate3Simple,
		rotate3Simple_,
		identityTransformation3,
		identityTransformation3Simple,
		rotatexy,
		rotatexz,
		rotateyz,
		rotatexySimple,
		rotatexzSimple,
		rotateyzSimple,
		determinant4,
		determinant3,
		smalld,
		smallf,
		SmallNum(..),
		equivalentSmall,
		eq2,
		eq3,
		eq4,
		eqm3,
		eqm4,
		smallishd,
		smallishf,
		SmallishNum(..),
		near,
		near2,
		near3,
		near4,
		nearm3,
		nearm4,
		rankNonzerov4,
		rankNonzerov3,
		inversem4,
		inversem3,
		inversem4GaussianElimination,
		inversem3GaussianElimination,
		perspective,
		perspectivePure,
		fov,
		fovPure,

		zv2,
		zv3,
		zv4,
		rm3,
		rm4,
		zm3,
		zm4,

		MView,
		MViewd,
		MView'(..), mviewPos, mviewTarget, mviewFov,
		viewMat,
		viewMat',
		worldToGL,
		worldToGLSimple,
		rescaleDepth,

		-- * 3D vector aiming rotation utils in radians: horizontal and vertical aiming of a point relative to origin

		aimHoriz3DSimple,
		aimVert3DSimple,

		-- * More utils
		v2z,
		v2s,
		v2nzElse,
		v2nsElse,
		v3z,
		v3s,
		v3nzElse,
		v3nsElse,
		v4z,
		v4s,
		v4nzElse,
		v4nsElse,

		-- * Subvectors
		xy3,
		xz3,
		yz3,
		xy4,
		xz4,
		xw4,
		yz4,
		yw4,
		zw4,
		xyz4,
		xyw4,
		xzw4,
		yzw4,

		sqx,

		-- * Equivalence and sign utils
		thresholdSignnum,
		nearSignnum,
		thresholdSignnumI,
		nearSignnumI
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad
import Data.Maybe

import Control.Lens

import Immutaball.Share.Utils

data Vec2 a = Vec2 {
	_x2 :: a,
	_y2 :: a
}
	deriving (Eq, Ord, Show)
makeLenses ''Vec2

instance Functor Vec2 where
	fmap :: (a -> b) -> (Vec2 a -> Vec2 b)
	fmap f (Vec2 x y) = Vec2 (f x) (f y)

instance Field1 (Vec2 a) (Vec2 a) a a where _1 = x2
instance Field2 (Vec2 a) (Vec2 a) a a where _2 = y2

instance Applicative Vec2 where
	pure :: a -> Vec2 a
	pure = rv2
	(<*>) :: Vec2 (a -> b) -> Vec2 a -> Vec2 b
	(Vec2 fx fy) <*> (Vec2 x y) = Vec2 (fx x) (fy y)

-- | Component-wise multiplication and abs instance.
instance (Num a) => Num (Vec2 a) where
	(+) = pv2
	(-) = minusv2
	fromInteger = rv2 . fromInteger
	abs = fmap abs
	(*) = cm2
	signum = fmap signum
instance (Num a, Fractional a) => Fractional (Vec2 a) where
	a / b = a * (recip <$> b)
	fromRational = rv2 . fromRational

rv2 :: a -> Vec2 a
rv2 z = Vec2 z z

pv2 :: (Num a) => Vec2 a -> Vec2 a -> Vec2 a
pv2 (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax + bx) (ay + by)

sv2 :: (Num a) => a -> Vec2 a -> Vec2 a
sv2 s (Vec2 x y) = Vec2 (s*x) (s*y)

minusv2 :: (Num a) => Vec2 a -> Vec2 a -> Vec2 a
minusv2 (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax - bx) (ay - by)

-- | Polar coordinates.
r2 :: forall a. (RealFloat a) => Lens' (Vec2 a) a
r2 = lens getter (flip setter)
	where
		getter :: Vec2 a -> a
		getter (Vec2 x y) = sqrt (x*x + y*y)
		setter :: a -> Vec2 a -> Vec2 a
		setter r1 v@(Vec2 x y)
			| x == 0.0 && y == 0.0    = Vec2 0.0 0.0
			| r0 == 0.0               = Vec2 0.0 0.0
			| isNaN s || isInfinite s = Vec2 0.0 0.0
			| otherwise               = Vec2 (s*x) (s*y)
			where
				r0 = getter v
				s  = r1/r0

t2 :: forall a. (RealFloat a) => Lens' (Vec2 a) a
t2 = lens getter (flip setter)
	where
		getter :: Vec2 a -> a
		getter v@(Vec2 x y)
			| (v^.r2) == 0.0            = 0.0
			| isNaN v' || isInfinite v' = 0.0
			| otherwise                 = v'
			where
				v' = atan2 y x
		setter :: a -> Vec2 a -> Vec2 a
		setter t1 v@(Vec2 _x _y) =
			Vec2 (r*(cos t1)) (r*(sin t1))
			where
				r = v^.r2

d2 :: (Num a) => Vec2 a -> Vec2 a -> a
d2 (Vec2 ax ay) (Vec2 bx by) = ax*bx + ay*by

-- | Component-wise binary operation.
co2 :: (a -> a -> a) -> Vec2 a -> Vec2 a -> Vec2 a
co2 (+*) (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax +* bx) (ay +* by)

-- | Component-wise foldr.
fr2 :: (a -> acc -> acc) -> acc -> Vec2 a -> acc
fr2 f z (Vec2 x y) = foldr f z [x, y]

-- | Component-wise multiplication.
cm2 :: (Num a) => Vec2 a -> Vec2 a -> Vec2 a
cm2 = co2 (*)

data Vec3 a = Vec3 {
	_x3 :: a,
	_y3 :: a,
	_z3 :: a
}
	deriving (Eq, Ord, Show)
makeLenses ''Vec3

instance Functor Vec3 where
	fmap :: (a -> b) -> (Vec3 a -> Vec3 b)
	fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Field1 (Vec3 a) (Vec3 a) a a where _1 = x3
instance Field2 (Vec3 a) (Vec3 a) a a where _2 = y3
instance Field3 (Vec3 a) (Vec3 a) a a where _3 = z3

instance Applicative Vec3 where
	pure :: a -> Vec3 a
	pure = rv3
	(<*>) :: Vec3 (a -> b) -> Vec3 a -> Vec3 b
	(Vec3 fx fy fz) <*> (Vec3 x y z) = Vec3 (fx x) (fy y) (fz z)

-- | Component-wise multiplication and abs instance.
instance (Num a) => Num (Vec3 a) where
	(+) = pv3
	(-) = minusv3
	fromInteger = rv3 . fromInteger
	abs = fmap abs
	(*) = cm3
	signum = fmap signum
instance (Num a, Fractional a) => Fractional (Vec3 a) where
	a / b = a * (recip <$> b)
	fromRational = rv3 . fromRational

rv3 :: a -> Vec3 a
rv3 z = Vec3 z z z

pv3 :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
pv3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax + bx) (ay + by) (az + bz)

sv3 :: (Num a) => a -> Vec3 a -> Vec3 a
sv3 s (Vec3 x y z) = Vec3 (s*x) (s*y) (s*z)

minusv3 :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
minusv3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax - bx) (ay - by) (az - bz)

d3 :: (Num a) => Vec3 a -> Vec3 a -> a
d3 (Vec3 ax ay az) (Vec3 bx by bz) = ax*bx + ay*by + az*bz

-- | Component-wise binary operation.
co3 :: (a -> a -> a) -> Vec3 a -> Vec3 a -> Vec3 a
co3 (+*) (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax +* bx) (ay +* by) (az +* bz)

-- | Component-wise foldr.
fr3 :: (a -> acc -> acc) -> acc -> Vec3 a -> acc
fr3 f z0 (Vec3 x y z) = foldr f z0 [x, y, z]

-- | Component-wise multiplication.
cm3 :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
cm3 = co3 (*)

-- | Magnitude of a vector.
r3 :: forall a. (RealFloat a) => Lens' (Vec3 a) a
r3 = lens getter (flip setter)
	where
		getter :: Vec3 a -> a
		getter (Vec3 x y z) = sqrt (x*x + y*y + z*z)
		setter :: a -> Vec3 a -> Vec3 a
		setter r1 v@(Vec3 x y z)
			| (x, y, z) == (0.0, 0.0, 0.0) = Vec3 0.0 0.0 0.0
			| r0 == 0.0                    = Vec3 0.0 0.0 0.0
			| isNaN s || isInfinite s      = Vec3 0.0 0.0 0.0
			| otherwise                    = Vec3 (s*x) (s*y) (s*z)
			where
				r0 = getter v
				s  = r1/r0

data Vec4 a = Vec4 {
	_x4 :: a,
	_y4 :: a,
	_z4 :: a,
	_w4 :: a
}
	deriving (Eq, Ord, Show)
makeLenses ''Vec4

instance Functor Vec4 where
	fmap :: (a -> b) -> (Vec4 a -> Vec4 b)
	fmap f (Vec4 x y z w) = Vec4 (f x) (f y) (f z) (f w)

instance Field1 (Vec4 a) (Vec4 a) a a where _1 = x4
instance Field2 (Vec4 a) (Vec4 a) a a where _2 = y4
instance Field3 (Vec4 a) (Vec4 a) a a where _3 = z4
instance Field4 (Vec4 a) (Vec4 a) a a where _4 = w4

instance Applicative Vec4 where
	pure :: a -> Vec4 a
	pure = rv4
	(<*>) :: Vec4 (a -> b) -> Vec4 a -> Vec4 b
	(Vec4 fx fy fz fw) <*> (Vec4 x y z w) = Vec4 (fx x) (fy y) (fz z) (fw w)

-- | Component-wise multiplication and abs instance.
instance (Num a) => Num (Vec4 a) where
	(+) = pv4
	(-) = minusv4
	fromInteger = rv4 . fromInteger
	abs = fmap abs
	(*) = cm4
	signum = fmap signum
instance (Num a, Fractional a) => Fractional (Vec4 a) where
	a / b = a * (recip <$> b)
	fromRational = rv4 . fromRational

rv4 :: a -> Vec4 a
rv4 z = Vec4 z z z z

pv4 :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a
pv4 (Vec4 ax ay az aw) (Vec4 bx by bz bw) = Vec4 (ax + bx) (ay + by) (az + bz) (aw + bw)

sv4 :: (Num a) => a -> Vec4 a -> Vec4 a
sv4 s (Vec4 x y z w) = Vec4 (s*x) (s*y) (s*z) (s*w)

minusv4 :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a
minusv4 (Vec4 ax ay az aw) (Vec4 bx by bz bw) = Vec4 (ax - bx) (ay - by) (az - bz) (aw - bw)

d4 :: (Num a) => Vec4 a -> Vec4 a -> a
d4 (Vec4 ax ay az aw) (Vec4 bx by bz bw) = ax*bx + ay*by + az*bz + aw*bw

-- | Component-wise binary operation.
co4 :: (a -> a -> a) -> Vec4 a -> Vec4 a -> Vec4 a
co4 (+*) (Vec4 ax ay az aw) (Vec4 bx by bz bw) = Vec4 (ax +* bx) (ay +* by) (az +* bz) (aw +* bw)

-- | Component-wise foldr.
fr4 :: (a -> acc -> acc) -> acc -> Vec4 a -> acc
fr4 f z0 (Vec4 x y z w) = foldr f z0 [x, y, z, w]

-- | Component-wise multiplication.
cm4 :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a
cm4 = co4 (*)

-- | Magnitude of a vector.
r4 :: forall a. (RealFloat a) => Lens' (Vec4 a) a
r4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> a
		getter (Vec4 x y z w) = sqrt (x*x + y*y + z*z + w*w)
		setter :: a -> Vec4 a -> Vec4 a
		setter r1 v@(Vec4 x y z w)
			| (x, y, z, w) == (0.0, 0.0, 0.0, 0.0) = Vec4 0.0 0.0 0.0 0.0
			| r0 == 0.0                            = Vec4 0.0 0.0 0.0 0.0
			| isNaN s || isInfinite s              = Vec4 0.0 0.0 0.0 0.0
			| otherwise                            = Vec4 (s*x) (s*y) (s*z) (s*w)
			where
				r0 = getter v
				s  = r1/r0

-- | Row-major, like C.
newtype Mat3 a = Mat3 { _getMat3 :: Vec3 (Vec3 a) }
	deriving (Eq, Ord, Show)
makeLenses ''Mat3
-- | Row-major, like C.
newtype Mat4 a = Mat4 { _getMat4 :: Vec4 (Vec4 a) }
	deriving (Eq, Ord, Show)
makeLenses ''Mat4

instance Functor Mat3 where
	fmap :: (a -> b) -> (Mat3 a -> Mat3 b)
	fmap f (Mat3 (Vec3 r1_ r2_ r3_)) = Mat3 (Vec3 (f <$> r1_) (f <$> r2_) (f <$> r3_))
instance Functor Mat4 where
	fmap :: (a -> b) -> (Mat4 a -> Mat4 b)
	fmap f (Mat4 (Vec4 r1_ r2_ r3_ r4_)) = Mat4 (Vec4 (f <$> r1_) (f <$> r2_) (f <$> r3_) (f <$> r4_))

instance Field1 (Mat3 a) (Mat3 a) (Vec3 a) (Vec3 a) where _1 = r0_3
instance Field2 (Mat3 a) (Mat3 a) (Vec3 a) (Vec3 a) where _2 = r1_3
instance Field3 (Mat3 a) (Mat3 a) (Vec3 a) (Vec3 a) where _3 = r2_3

instance Field1 (Mat4 a) (Mat4 a) (Vec4 a) (Vec4 a) where _1 = r0_4
instance Field2 (Mat4 a) (Mat4 a) (Vec4 a) (Vec4 a) where _2 = r1_4
instance Field3 (Mat4 a) (Mat4 a) (Vec4 a) (Vec4 a) where _3 = r2_4
instance Field4 (Mat4 a) (Mat4 a) (Vec4 a) (Vec4 a) where _4 = r3_4

instance Applicative Mat3 where
	pure :: a -> Mat3 a
	pure z = let reps = rv3 z in Mat3 $ rv3 reps
	(<*>) :: Mat3 (a -> b) -> Mat3 a -> Mat3 b
	(Mat3 frows) <*> (Mat3 rows) = Mat3 $ ((<*>) <$> frows) <*> rows
instance Applicative Mat4 where
	pure :: a -> Mat4 a
	pure z = let reps = rv4 z in Mat4 $ rv4 reps
	(<*>) :: Mat4 (a -> b) -> Mat4 a -> Mat4 b
	(Mat4 frows) <*> (Mat4 rows) = Mat4 $ ((<*>) <$> frows) <*> rows

r0_3 :: Lens' (Mat3 a) (Vec3 a)
r0_3 = getMat3.x3

r1_3 :: Lens' (Mat3 a) (Vec3 a)
r1_3 = getMat3.y3

r2_3 :: Lens' (Mat3 a) (Vec3 a)
r2_3 = getMat3.z3

r0_4 :: Lens' (Mat4 a) (Vec4 a)
r0_4 = getMat4.x4

r1_4 :: Lens' (Mat4 a) (Vec4 a)
r1_4 = getMat4.y4

r2_4 :: Lens' (Mat4 a) (Vec4 a)
r2_4 = getMat4.z4

r3_4 :: Lens' (Mat4 a) (Vec4 a)
r3_4 = getMat4.w4

e0_0_4 :: Lens' (Mat4 a) a
e0_0_4 = r0_4.x4
e0_1_4 :: Lens' (Mat4 a) a
e0_1_4 = r0_4.y4
e0_2_4 :: Lens' (Mat4 a) a
e0_2_4 = r0_4.z4
e0_3_4 :: Lens' (Mat4 a) a
e0_3_4 = r0_4.w4

e1_0_4 :: Lens' (Mat4 a) a
e1_0_4 = r1_4.x4
e1_1_4 :: Lens' (Mat4 a) a
e1_1_4 = r1_4.y4
e1_2_4 :: Lens' (Mat4 a) a
e1_2_4 = r1_4.z4
e1_3_4 :: Lens' (Mat4 a) a
e1_3_4 = r1_4.w4

e2_0_4 :: Lens' (Mat4 a) a
e2_0_4 = r2_4.x4
e2_1_4 :: Lens' (Mat4 a) a
e2_1_4 = r2_4.y4
e2_2_4 :: Lens' (Mat4 a) a
e2_2_4 = r2_4.z4
e2_3_4 :: Lens' (Mat4 a) a
e2_3_4 = r2_4.w4

e3_0_4 :: Lens' (Mat4 a) a
e3_0_4 = r3_4.x4
e3_1_4 :: Lens' (Mat4 a) a
e3_1_4 = r3_4.y4
e3_2_4 :: Lens' (Mat4 a) a
e3_2_4 = r3_4.z4
e3_3_4 :: Lens' (Mat4 a) a
e3_3_4 = r3_4.w4

e0_0_3 :: Lens' (Mat3 a) a
e0_0_3 = r0_3.x3
e0_1_3 :: Lens' (Mat3 a) a
e0_1_3 = r0_3.y3
e0_2_3 :: Lens' (Mat3 a) a
e0_2_3 = r0_3.z3

e1_0_3 :: Lens' (Mat3 a) a
e1_0_3 = r1_3.x3
e1_1_3 :: Lens' (Mat3 a) a
e1_1_3 = r1_3.y3
e1_2_3 :: Lens' (Mat3 a) a
e1_2_3 = r1_3.z3

e2_0_3 :: Lens' (Mat3 a) a
e2_0_3 = r2_3.x3
e2_1_3 :: Lens' (Mat3 a) a
e2_1_3 = r2_3.y3
e2_2_3 :: Lens' (Mat3 a) a
e2_2_3 = r2_3.z3

transposeMat3 :: Mat3 a -> Mat3 a
transposeMat3 (Mat3 (Vec3
		(Vec3 v0_0 v0_1 v0_2)
		(Vec3 v1_0 v1_1 v1_2)
		(Vec3 v2_0 v2_1 v2_2)
	)) = Mat3 (Vec3
		(Vec3 v0_0 v1_0 v2_0)
		(Vec3 v0_1 v1_1 v2_1)
		(Vec3 v0_2 v1_2 v2_2)
	)

transposeMat4 :: Mat4 a -> Mat4 a
transposeMat4 (Mat4 (Vec4
		(Vec4 v0_0 v0_1 v0_2 v0_3)
		(Vec4 v1_0 v1_1 v1_2 v1_3)
		(Vec4 v2_0 v2_1 v2_2 v2_3)
		(Vec4 v3_0 v3_1 v3_2 v3_3)
	)) = Mat4 (Vec4
		(Vec4 v0_0 v1_0 v2_0 v3_0)
		(Vec4 v0_1 v1_1 v2_1 v3_1)
		(Vec4 v0_2 v1_2 v2_2 v3_2)
		(Vec4 v0_3 v1_3 v2_3 v3_3)
	)

c0_3 :: Lens' (Mat3 a) (Vec3 a)
c0_3 = transposeM3.r0_3

c1_3 :: Lens' (Mat3 a) (Vec3 a)
c1_3 = transposeM3.r1_3

c2_3 :: Lens' (Mat3 a) (Vec3 a)
c2_3 = transposeM3.r2_3

c0_4 :: Lens' (Mat4 a) (Vec4 a)
c0_4 = transposeM4.r0_4

c1_4 :: Lens' (Mat4 a) (Vec4 a)
c1_4 = transposeM4.r1_4

c2_4 :: Lens' (Mat4 a) (Vec4 a)
c2_4 = transposeM4.r2_4

c3_4 :: Lens' (Mat4 a) (Vec4 a)
c3_4 = transposeM4.r3_4

transposeM3 :: Lens' (Mat3 a) (Mat3 a)
transposeM3 = lens getter (flip setter)
	where
		getter :: Mat3 a -> Mat3 a
		getter m = transposeMat3 m
		setter :: Mat3 a -> Mat3 a -> Mat3 a
		setter m _ = transposeMat3 m

transposeM4 :: Lens' (Mat4 a) (Mat4 a)
transposeM4 = lens getter (flip setter)
	where
		getter :: Mat4 a -> Mat4 a
		getter m = transposeMat4 m
		setter :: Mat4 a -> Mat4 a -> Mat4 a
		setter m _ = transposeMat4 m

-- | Matrix multiplication.
--
-- This is not commutative.
mm3 :: (Num a) => Mat3 a -> Mat3 a -> Mat3 a
mm3 a b = (Mat3 (Vec3
		(Vec3 (d3 (a^.r0_3) (b^.c0_3))  (d3 (a^.r0_3) (b^.c1_3))  (d3 (a^.r0_3) (b^.c2_3)))
		(Vec3 (d3 (a^.r1_3) (b^.c0_3))  (d3 (a^.r1_3) (b^.c1_3))  (d3 (a^.r1_3) (b^.c2_3)))
		(Vec3 (d3 (a^.r2_3) (b^.c0_3))  (d3 (a^.r2_3) (b^.c1_3))  (d3 (a^.r2_3) (b^.c2_3)))
	))

-- | Matrix multiplication.
--
-- This is not commutative.
mm4 :: (Num a) => Mat4 a -> Mat4 a -> Mat4 a
mm4 a b = (Mat4 (Vec4
		(Vec4 (d4 (a^.r0_4) (b^.c0_4))  (d4 (a^.r0_4) (b^.c1_4))  (d4 (a^.r0_4) (b^.c2_4))  (d4 (a^.r0_4) (b^.c3_4)))
		(Vec4 (d4 (a^.r1_4) (b^.c0_4))  (d4 (a^.r1_4) (b^.c1_4))  (d4 (a^.r1_4) (b^.c2_4))  (d4 (a^.r1_4) (b^.c3_4)))
		(Vec4 (d4 (a^.r2_4) (b^.c0_4))  (d4 (a^.r2_4) (b^.c1_4))  (d4 (a^.r2_4) (b^.c2_4))  (d4 (a^.r2_4) (b^.c3_4)))
		(Vec4 (d4 (a^.r3_4) (b^.c0_4))  (d4 (a^.r3_4) (b^.c1_4))  (d4 (a^.r3_4) (b^.c2_4))  (d4 (a^.r3_4) (b^.c3_4)))
	))

v3magnitude :: (Num a, Floating a) => Vec3 a -> a
v3magnitude (Vec3 x y z) = sqrt $ x*x + y*y + z*z

v3normalize :: (Num a, Floating a) => Vec3 a -> Vec3 a
v3normalize v = (1/v3magnitude v) `sv3` v

v4magnitude :: (Num a, Floating a) => Vec4 a -> a
v4magnitude (Vec4 x y z w) = sqrt $ x*x + y*y + z*z + w*w

v4normalize :: (Num a, Floating a) => Vec4 a -> Vec4 a
v4normalize v = (1/v4magnitude v) `sv4` v

identity3 :: (Num a, Fractional a) => Mat3 a
identity3 = Mat3 $ Vec3
	(Vec3 1.0 0.0 0.0)
	(Vec3 0.0 1.0 0.0)
	(Vec3 0.0 0.0 1.0)

identity4 :: (Num a, Fractional a) => Mat4 a
identity4 = Mat4 $ Vec4
	(Vec4 1.0 0.0 0.0 0.0)
	(Vec4 0.0 1.0 0.0 0.0)
	(Vec4 0.0 0.0 1.0 0.0)
	(Vec4 0.0 0.0 0.0 1.0)

instance (Num a, Fractional a) => Semigroup (Mat3 a) where
	(<>) = mm3
instance (Num a, Fractional a) => Semigroup (Mat4 a) where
	(<>) = mm4
instance (Num a, Fractional a) => Monoid (Mat3 a) where
	mempty = identity3
instance (Num a, Fractional a) => Monoid (Mat4 a) where
	mempty = identity4

data Rect a = Rect {
	_rectp1 :: Vec2 a,
	_rectp2 :: Vec2 a
}
	deriving (Eq, Ord, Show)
makeLenses ''Rect

-- | Not a lens by laws definition; see 'rectBottom'.
rectLowerLeft :: forall a. (Ord a) => Lens' (Rect a) (Vec2 a)
rectLowerLeft = lens getter (flip setter)
	where
		getter :: Rect a -> Vec2 a
		getter (Rect (Vec2 ax ay) (Vec2 bx by)) = Vec2 (if' (ax <= bx) ax bx) (if' (ay <= by) ay by)
		setter :: Vec2 a -> Rect a -> Rect a
		setter (Vec2 x' y') (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 (if' (ax <= bx) x' ax) (if' (ay <= by) y' ay)) (Vec2 (if' (ax <= bx) bx x') (if' (ay <= by) by y'))

-- | Not a lens by laws definition; see 'rectBottom'.
rectUpperRight :: forall a. (Ord a) => Lens' (Rect a) (Vec2 a)
rectUpperRight = lens getter (flip setter)
	where
		getter :: Rect a -> Vec2 a
		getter (Rect (Vec2 ax ay) (Vec2 bx by)) = Vec2 (if' (ax <= bx) bx ax) (if' (ay <= by) by ay)
		setter :: Vec2 a -> Rect a -> Rect a
		setter (Vec2 x' y') (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 (if' (ax <= bx) ax x') (if' (ay <= by) ay y')) (Vec2 (if' (ax <= bx) x' bx) (if' (ay <= by) y' by))

-- | Not a lens by laws definition; see 'rectBottom'.
rectCenter :: forall a. (Num a, Fractional a) => Lens' (Rect a) (Vec2 a)
rectCenter = lens getter (flip setter)
	where
		getter :: Rect a -> Vec2 a
		getter (Rect p1 p2) = lerpV2 p1 p2 0.5
		setter :: (Vec2 a) -> Rect a -> Rect a
		setter c1 r@(Rect p1 p2) = Rect (p1 `pv2` offset) (p2 `pv2` offset)
			where
				offset = c1 `minusv2` c0
				c0 = getter r

-- | Not a lens by laws definition; see 'rectBottom'.
rectTop :: forall a. (Ord a) => Lens' (Rect a) a
rectTop = lens getter (flip setter)
	where
		getter :: Rect a -> a
		getter (Rect (Vec2 _ax ay) (Vec2 _bx by)) = if' (ay <= by) by ay
		setter :: a -> Rect a -> Rect a
		setter y' (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 ax (if' (ay <= by) ay y')) (Vec2 bx (if' (ay <= by) y' by))

-- | Not a lens by laws definition; see 'rectBottom'.
rectRight :: forall a. (Ord a) => Lens' (Rect a) a
rectRight = lens getter (flip setter)
	where
		getter :: Rect a -> a
		getter (Rect (Vec2 ax _ay) (Vec2 bx _by)) = if' (ax <= bx) bx ax
		setter :: a -> Rect a -> Rect a
		setter x' (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 (if' (ax <= bx) ax x') ay) (Vec2 (if' (ax <= bx) x' bx) by)

-- | This lens does not satisfy the 1st lens law, since you can set a new
-- bottom above the old top.  Same for the other 3 setters in this category, and also the corner setters.
rectBottom :: forall a. (Ord a) => Lens' (Rect a) a
rectBottom = lens getter (flip setter)
	where
		getter :: Rect a -> a
		getter (Rect (Vec2 _ax ay) (Vec2 _bx by)) = if' (ay <= by) ay by
		setter :: a -> Rect a -> Rect a
		setter y' (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 ax (if' (ay <= by) y' ay)) (Vec2 bx (if' (ay <= by) by y'))

-- | Not a lens by laws definition; see 'rectBottom'.
rectLeft :: forall a. (Ord a) => Lens' (Rect a) a
rectLeft = lens getter (flip setter)
	where
		getter :: Rect a -> a
		getter (Rect (Vec2 ax _ay) (Vec2 bx _by)) = if' (ax <= bx) ax bx
		setter :: a -> Rect a -> Rect a
		setter x' (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 (if' (ax <= bx) x' ax) ay) (Vec2 (if' (ax <= bx) bx x') by)

-- | Not a lens by laws definition; see 'rectBottom'.
rectUpperLeft :: forall a. (Ord a) => Lens' (Rect a) (Vec2 a)
rectUpperLeft = lens getter (flip setter)
	where
		getter :: Rect a -> Vec2 a
		getter (Rect (Vec2 ax ay) (Vec2 bx by)) = Vec2 (if' (ax <= bx) ax bx) (if' (ay <= by) by ay)
		setter :: Vec2 a -> Rect a -> Rect a
		setter (Vec2 x' y') (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 (if' (ax <= bx) x' ax) (if' (ay <= by) ay y')) (Vec2 (if' (ax <= bx) bx x') (if' (ay <= by) y' by))

-- | Not a lens by laws definition; see 'rectBottom'.
rectLowerRight :: forall a. (Ord a) => Lens' (Rect a) (Vec2 a)
rectLowerRight = lens getter (flip setter)
	where
		getter :: Rect a -> Vec2 a
		getter (Rect (Vec2 ax ay) (Vec2 bx by)) = Vec2 (if' (ax <= bx) bx ax) (if' (ay <= by) ay by)
		setter :: Vec2 a -> Rect a -> Rect a
		setter (Vec2 x' y') (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 (if' (ax <= bx) ax x') (if' (ay <= by) y' ay)) (Vec2 (if' (ax <= bx) x' bx) (if' (ay <= by) by y'))

rectWidthAboutCenter :: forall a. (Num a, Fractional a) => Lens' (Rect a) a
rectWidthAboutCenter = lens getter (flip setter)
	where
		getter :: Rect a -> a
		getter (Rect (Vec2 ax _ay) (Vec2 bx _by)) = abs $ bx - ax
		setter :: a -> Rect a -> Rect a
		setter w1 (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 (cx - wr) ay) (Vec2 (cx + wr) by)
			where
				cx = ax + (bx - ax)/2
				wr = w1/2

rectHeightAboutCenter :: forall a. (Num a, Fractional a) => Lens' (Rect a) a
rectHeightAboutCenter = lens getter (flip setter)
	where
		getter :: Rect a -> a
		getter (Rect (Vec2 _ax ay) (Vec2 _bx by)) = abs $ by - ay
		setter :: a -> Rect a -> Rect a
		setter h1 (Rect (Vec2 ax ay) (Vec2 bx by)) = Rect (Vec2 ax (cy - hr)) (Vec2 bx (cy + hr))
			where
				cy = ay + (by - ay)/2
				hr = h1/2

-- | Proportion preserving scaling.
rectAvgSideAboutCenter :: forall a. (Num a, Fractional a, RealFloat a) => Lens' (Rect a) a
rectAvgSideAboutCenter = lens getter (flip setter)
	where
		getter :: Rect a -> a
		getter r = lerp (r^.rectWidthAboutCenter) (r^.rectHeightAboutCenter) 0.5
		setter :: a -> Rect a -> Rect a
		setter avgSide r
			| isNaN s || isInfinite s = Rect z z
			| otherwise               = r & rectWidthAboutCenter %~ (s*) & rectHeightAboutCenter %~ (s*)
			where
				s = avgSide / getter r
				z = Vec2 (realToFrac (0.0 :: Double)) (realToFrac (0.0 :: Double))

rectNormalize :: (Ord a) => Rect a -> Rect a
rectNormalize r = Rect (r^.rectLowerLeft) (r^.rectUpperRight)

isInRect :: (Ord a) => Rect a -> Vec2 a -> Bool
isInRect = isInRect' False

isInRect' :: (Ord a) => Bool -> Rect a -> Vec2 a -> Bool
isInRect' exclusive r (Vec2 px py) = ll^.x2 <=- px && px <=- ur^.x2  &&  ll^.y2 <=- py && py <=- ur^.y2
	where
		ll = r^.rectLowerLeft
		ur = r^.rectUpperRight
		infixl 4 <=-
		(<=-) = if' (not exclusive) (<=) (<)

lerpWith :: (a -> a -> a) -> (a -> a -> a) -> (s -> a -> a) -> a -> a -> s -> a
lerpWith plus minus scale from_ to_ v = from_ `plus` (v`scale`(to_ `minus` from_))

lerp :: (Num a) => a -> a -> a -> a
--lerp from to v = from + v*(to - from)
lerp = lerpWith (+) (-) (*)

lerpV2 :: (Num a) => Vec2 a -> Vec2 a -> a -> Vec2 a
lerpV2 = lerpWith pv2 minusv2 sv2

lerpV3 :: (Num a) => Vec3 a -> Vec3 a -> a -> Vec3 a
lerpV3 = lerpWith pv3 minusv3 sv3

lerpV4 :: (Num a) => Vec4 a -> Vec4 a -> a -> Vec4 a
lerpV4 = lerpWith pv4 minusv4 sv4

ilerpWith :: (a -> s) -> (a -> a -> a) -> (s -> s -> s) -> a -> a -> a -> s
--ilerpWith from_ to_ on_ = (on_ - from_) / (to_ - from_)
ilerpWith flatten minus div_ from_ to_ on_ = (flatten $ on_ `minus` from_) `div_` (flatten $ to_ `minus` from_)

-- | Inverse lerp on x3.
--
-- Get the lerp3 that would provide input x3.  Find how close input x3 is to
-- destination x2 relative to source x1.
ilerp :: (Num a, Fractional a) => a -> a -> a -> a
ilerp = ilerpWith id (-) (/)

-- | pi is in prelude and yet tau isn't.
tau :: (Floating a) => a
tau = 2*pi

type WidthHeightI = (Integer, Integer)

-- | Homonogenous coordinates.
v4to3 :: (Fractional a) => Vec4 a -> Vec3 a
v4to3 (Vec4 x y z w) = Vec3 (x/w) (y/w) (z/w)

v3to4 :: (Fractional a) => Vec3 a -> Vec4 a
v3to4 (Vec3 x y z) = Vec4 x y z 1.0

-- | Apply a transformation matrix to a vector.
--
-- The vector is interpreted as a 4x1 matrix, like a column.
--
-- You can think of multiplication of a matrix with a vector like this:
--
--          x
--          x
--          x
--          x
--
-- x x x x  ?
-- x x x x  ?
-- x x x x  ?
-- x x x x  ?
--
-- In the middle is the result of ? we want to find.  Above it is the vector.
-- To the left of the result is the matrix.  This visual may aid in
-- understanding this basic linear algebra.  I like to think of the column on
-- top as like a falling block, with its top falling to the left, so that it
-- rotates 45 degrees counter-clockwise.  It first falls onto the top row, and
-- you take the dot product to get the first ‘?’ result.  Repeat for the second
-- ‘?’ on the second row and so on.  Note that if you had 4 vectors (columns)
-- above, you could do the same, taking that column as a brick-like block, and
-- letting it fall onto a row of the matrix of the left, and then you'd get
-- full matrix-matrix multiplication.
--
-- Each row of the transformation matrix tells you how to get a new coordinate,
-- i.e. what the result is of mapping a coordinate after the transformation back
-- into original coordinates.  The first row is like a weighted sum, taking
-- each input coordinate, and producing an ‘x’ coordinate.  The second row
-- likewise, producing a ‘y’ coordinate.  (e.g. for ‘y’ coordinate, a row of ‘0 1 0 0’
-- simply returns the ‘y’ coordinate unchanged, and ‘0.5 0.5 0.0 0.0’ would
-- provide the value half-way between the input x and y coordinates as the new y
-- coordinate.
--
-- The columns of the transformation matrix can be interpreted to mean the new
-- axes after your transformation.  e.g. if you scale the z axis by 2, making
-- it double in size, the new z axis is represented by the vector from the
-- origin to the point ‘0 0 2 0’.  This can also be useful for 3D rotation,
-- since it can be easier to do rotations in a simple, 2D plane, dealing with
-- just some sines, cosines, and angles.
--
-- We usually use the transformation on the matrix and the vector on the right.
-- You can do it in reverse order, but then the matrix would need to be
-- transposed, and the vector would be a row rather than a column.  For the
-- reversed order, see 'vm4'.
--
-- Finally, we usually use 4D vectors to deal with 3D space through what's
-- called homogonous coordinates, an extra coordinate, which lets us more
-- easily represent translations and perspectives.  The meaning of the last
-- coordinate with homogonous coordinates is that all the other coordinates are
-- divided by it.  Often the 4th coordinate can just be ‘1’.  This gives the
-- property that multiplying a scalar by the vector yields an equivalent point.
-- It also happens to be convenient for translations and perspectives.
mv4 :: (Num a) => Mat4 a -> Vec4 a -> Vec4 a
mv4 m v = Vec4 (d4 (m^.r0_4) v) (d4 (m^.r1_4) v) (d4 (m^.r2_4) v) (d4 (m^.r3_4) v)

-- | See 'mv4'; this is just for 3d vectors.
--
-- mv4 is 4d but we use 4d vectors to represent 3d vectors in order to more
-- easily represent translations and perspectives, through what's known as
-- homogonous coordinates, where the components except the last is divided by
-- the last component, and where a vector can be multiplied by a scalar and
-- getting an equivalent vector.
mv3 :: (Num a) => Mat3 a -> Vec3 a -> Vec3 a
mv3 m v = Vec3 (d3 (m^.r0_3) v) (d3 (m^.r1_3) v) (d3 (m^.r2_3) v)

m4v3 :: (Num a, Fractional a) => Mat4 a -> Vec3 a -> Vec3 a
m4v3 m v = v4to3 . mv4 m . v3to4 $ v

-- | (The vector is interpreted as a 1x4 matrix.)
--
-- Note the vector is on the left with a transposed matrix.  We personally use
-- vector on the right with mv3, where the columns, not the rows, are the new
-- axes, and points in terms of the new axes are mapped back to the original
-- coordinates with the transformation matrix.)
vm3 :: (Num a) => Vec3 a -> Mat3 a -> Vec3 a
vm3 v m = Vec3 (d3 v (m^.c0_3)) (d3 v (m^.c1_3)) (d3 v (m^.c2_3))

-- | Vector on the left with transposed transformation matrix.
--
-- See also 'vm3' and 'v4m'.
vm4 :: (Num a) => Vec4 a -> Mat4 a -> Vec4 a
vm4 v m = Vec4 (d4 v (m^.c0_4)) (d4 v (m^.c1_4)) (d4 v (m^.c2_4)) (d4 v (m^.c3_4))

v3m4 :: (Num a, Fractional a) => Vec3 a -> Mat4 a -> Vec3 a
v3m4 v m = v4to3 $ vm4 (v3to4 v) m

v2or :: (RealFloat a) => Vec2 a -> Vec2 a
v2or = v2orWith (Vec2 0.0 0.0)

v2orWith :: (RealFloat a) => Vec2 a -> Vec2 a -> Vec2 a
v2orWith else_ v@(Vec2 x y)
	| isNaN x || isInfinite x = else_
	| isNaN y || isInfinite y = else_
	| otherwise = v

v3or :: (RealFloat a) => Vec3 a -> Vec3 a
v3or = v3orWith (Vec3 0.0 0.0 0.0)

v3orWith :: (RealFloat a) => Vec3 a -> Vec3 a -> Vec3 a
v3orWith else_ v@(Vec3 x y z)
	| isNaN x || isInfinite x = else_
	| isNaN y || isInfinite y = else_
	| isNaN z || isInfinite z = else_
	| otherwise = v

v4or :: (RealFloat a) => Vec4 a -> Vec4 a
v4or = v4orWith (Vec4 0.0 0.0 0.0 0.0)

v4orWith :: (RealFloat a) => Vec4 a -> Vec4 a -> Vec4 a
v4orWith else_ v@(Vec4 x y z w)
	| isNaN x || isInfinite x = else_
	| isNaN y || isInfinite y = else_
	| isNaN z || isInfinite z = else_
	| isNaN w || isInfinite w = else_
	| otherwise = v

flor :: (RealFloat a) => a -> a
flor = florWith 0.0

florWith :: (RealFloat a) => a -> a -> a
florWith else_ x
	| isNaN x || isInfinite x = else_
	| otherwise = x

-- | Cross product.
--
-- Satisfies a x b = |a| * |b| * sin(t) * n, providing a vector perpendicular
-- to the plane containing both vectors (if linearly independent) with a magnitude
-- equal to the product of magnitudes at the sin of the angle between them.
vx3 :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
vx3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

-- | Find a vector perpendicular.
v2perp :: (Num a) => Vec2 a -> Vec2 a
v2perp (Vec2 x y) = Vec2 (-y) x

-- | Find a vector perpendicular to the normal, with v2perp on the largest
-- magnitude component (which is non-zero iff the vector is non-zero) and some
-- other component (the next one).
v3perp :: (Num a, Ord a) => Vec3 a -> Vec3 a
v3perp (Vec3 x y z)
	| abs y <= abs x && abs z <= abs x = Vec3 (-y) x    z
	| abs z <= abs y && abs x <= abs y = Vec3 x    (-z) y
	| otherwise                        = Vec3 z    y    (-x)

-- | A representation of a simple 3D rotation about an axis that intersects the
-- origin.
data SimpleRotation a = SimpleRotation {
	_srCcwAngle   :: a,
	_srOriginAxis :: Vec3 a
}
	deriving (Eq, Ord, Show)
makeLenses ''SimpleRotation

-- | A complex number with 3 imaginary components instead of 1.
--
-- It is sometimes used to represent 3D rotations as an alternative to transformation matrices.
--
-- ij = -ji = k; jk = -kj = i; ki = -ik = j; i^2 = j^2 = k^2 = -1.
--
-- Beyond this I haven't learned much about these things.
data Quaternion a = Quaternion {
	_qReal   :: a,
	_qVector :: Vec3 a
}
	deriving (Eq, Ord, Show)
makeLenses ''Quaternion

qasv4 :: Lens' (Quaternion a) (Vec4 a)
qasv4 = lens getter (flip setter)
	where
		getter :: Quaternion a -> Vec4 a
		getter (Quaternion a (Vec3 b c d)) = Vec4 a b c d
		setter :: Vec4 a -> Quaternion a -> Quaternion a
		setter (Vec4 a b c d) _ = Quaternion a (Vec3 b c d)

-- | Scale quaternion.
sq :: (Num a) => a -> Quaternion a -> Quaternion a
sq s (Quaternion a (Vec3 b c d)) = Quaternion (s*a) (Vec3 (s*b) (s*c) (s*d))

-- | Plus quaternion.
pq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
pq (Quaternion aa (Vec3 ab ac ad)) (Quaternion ba (Vec3 bb bc bd)) = Quaternion (aa + ba) (Vec3 (ab + bb) (ac + bc) (ad + bd))

-- | Minus quaternion.
mq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
mq (Quaternion aa (Vec3 ab ac ad)) (Quaternion ba (Vec3 bb bc bd)) = Quaternion (aa - ba) (Vec3 (ab - bb) (ac - bc) (ad - bd))

-- | quaternion ^ -1.
--
-- (Quaternion multiplicative inverse.)
qmi :: (Num a, Fractional a) => Quaternion a -> Quaternion a
qmi (Quaternion a (Vec3 b c d)) = sq (1/(a*a + b*b + c*c + d*d)) $ Quaternion a (Vec3 (-b) (-c) (-d))

-- | Quaternion multiplication.
--
-- ‘Hamilton product’: distribute bases.
qq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
qq (Quaternion aa (Vec3 ab ac ad)) (Quaternion ba (Vec3 bb bc bd)) =
	Quaternion
		(aa*ba - ab*bb - ac*bc - ad*bd) $ Vec3
		(aa*bb + ab*ba + ac*bd - ad*bc)
		(aa*bc - ab*bd + ac*ba + ad*bb)
		(aa*bd + ab*bc - ac*bb + ad*ba)

-- | Convert a vector to a quaternion representation.
--
-- This can be conjugated with a rotation quaternion to rotate a vector.
v3q :: (Fractional a) => Vec3 a -> Quaternion a
v3q v = Quaternion 0.0 v

-- | Conjugate a 3D vector by a quaternion.
--
-- If the quaternion represents a rotation, the vector will be rotated.
qcv3 :: (Num a, Fractional a) => Quaternion a -> Vec3 a -> Vec3 a
qcv3 q v = (^.qVector) $ q `qq` (v3q v) `qq` (qmi q)

-- | Convert a 'SimpleRotation' to a quaternion.
srToQ :: (Num a, Floating a) => SimpleRotation a -> Quaternion a
srToQ sr = Quaternion (cos ((sr^.srCcwAngle)/2)) $ (sin ((sr^.srCcwAngle)/2)) `sv3` (v3normalize $ sr^.srOriginAxis)

qnormalize :: (Num a, Fractional a, Floating a) => Quaternion a -> Quaternion a
qnormalize q = (1/qmagnitude q) `sq` q

qmagnitude :: (Num a, Floating a) => Quaternion a -> a
qmagnitude (Quaternion a (Vec3 b c d)) = sqrt $ a*a + b*b + c*c + d*d

-- | Convert a quaternion-encoded rotation to a SimpleRotation.
--
-- The magnitude of the vector is the sine of half the angle of rotation.
--
-- Get the sign of Real to get the quadrant.
qToSr :: (RealFloat a, Num a) => Quaternion a -> SimpleRotation a
qToSr (Quaternion a v) = SimpleRotation {
	_srCcwAngle   = atan2 (2 * asin (v3magnitude v)) (2 * acos a),
	_srOriginAxis = v3normalize v
}

-- | Encode the angle as the magnitude.
srToVec3 :: (Num a) => SimpleRotation a -> Vec3 a
srToVec3 sr = (sr^.srCcwAngle) `sv3` (sr^.srOriginAxis)

-- | Decode the angle as the magnitude.
v3ToSr :: (Floating a, RealFloat a) => Vec3 a -> SimpleRotation a
v3ToSr v = SimpleRotation {
	_srCcwAngle   = v3magnitude $ v,
	_srOriginAxis = v3or . v3normalize $ v
}

-- | Rotate x radians about the axis pointing in direction, which intersects the origin.
--
-- You can use 'v3to4' and 'v4to3' as needed to translate between homogenous
-- coordinates and non-homogenous coordinates.
simpleRotate :: (Num a, RealFloat a) => SimpleRotation a -> Vec3 a -> Vec3 a
simpleRotate sr v = rotate3Simple sr `mv3` v

m3to4 :: (Fractional a) => Mat3 a -> Mat4 a
m3to4 (Mat3 (Vec3
		(Vec3 v0_0 v0_1 v0_2)
		(Vec3 v1_0 v1_1 v1_2)
		(Vec3 v2_0 v2_1 v2_2)
	)) = Mat4 (Vec4
		(Vec4 v0_0 v0_1 v0_2 0.0)
		(Vec4 v1_0 v1_1 v1_2 0.0)
		(Vec4 v2_0 v2_1 v2_2 0.0)
		(Vec4 0.0  0.0  0.0  1.0)
	)

-- | Make a transformation that applies the given component-wise scale.
scale3 :: (Fractional a) => Vec3 a -> Mat4 a
scale3 = m3to4 . scale3Simple

-- | Make a transformation that applies the given component-wise scale.
scale3Simple :: (Fractional a) => Vec3 a -> Mat3 a
scale3Simple v = Mat3 $ Vec3
	(Vec3 (v^.x3) 0.0     0.0)
	(Vec3 0.0     (v^.y3) 0.0)
	(Vec3 0.0     0.0     (v^.z3))

-- | Make a transformation that applies the given component-wise scale.
scale4 :: (Fractional a) => Vec4 a -> Mat4 a
scale4 v = Mat4 $ Vec4
	(Vec4 (v^.x4) 0.0     0.0     0.0)
	(Vec4 0.0     (v^.y4) 0.0     0.0)
	(Vec4 0.0     0.0     (v^.z4) 0.0)
	(Vec4 0.0     0.0     0.0     (v^.w4))

-- | Make a transformation matrix that translates a 3D point in 4D homogeneous coordinates.
--
-- (Note this in the order we normally use, where you can multiply a vector to
-- the right of the matrix.  You can still transpose the matrix to multiply in the
-- reverse order.)
translate3 :: (Fractional a) => Vec3 a -> Mat4 a
translate3 v = Mat4 $ Vec4
	(Vec4 1.0 0.0 0.0 (v^.x3))
	(Vec4 0.0 1.0 0.0 (v^.y3))
	(Vec4 0.0 0.0 1.0 (v^.z3))
	(Vec4 0.0 0.0 0.0 1.0)

-- | Rotate about an axis in the xy plane so that the new z axis becomes as
-- specified.
--
-- Similarly to 'tilt3ySimple', multiply by ‘i’ to perform a right angle CCW
-- rotation in the corresponding plane with normalization, and for the second
-- rotation, if there is a roll (i.e. rotation in xz), treat the non-y
-- component as a projection of the length of z's xz.
tilt3z :: (Floating a, Num a, Fractional a, RealFloat a, SmallNum a) => Vec3 a -> Mat4 a
tilt3z = m3to4 . tilt3zSimple

-- | 'tilt3z' without homogeneous coordinates.
tilt3zSimple :: (Floating a, Num a, Fractional a, RealFloat a, SmallNum a) => Vec3 a -> Mat3 a
tilt3zSimple z_ = Mat3 $ Vec3
	-- new x axis           new y axis               new z axis
	( Vec3 ( z'^.z3 / zhr') (-(z'^.x3/zhr')*(z^.y3)) (z^.x3) )
	( Vec3 0.0              (zhr                   ) (z^.y3) )
	( Vec3 (-z'^.x3 / zhr') (-(z'^.z3/zhr')*(z^.y3)) (z^.z3) )
	where
		sq_ a = a * a
		z = v3normalize z_
		zhr = Vec2 (z^.x3) (z^.z3) ^. r2
		zhr' = Vec2 (z'^.x3) (z'^.z3) ^. r2  -- = sqrt $ sq_ z^.x3 + sq_ z^.y3  -- normalizes new x axis
		--zh = Vec2 (z'^.x3) (z'^.z3) -- The 2D vector mentioned above.
		--zhr = zh^. r2  -- = sqrt $ sq_ z^.x3 + sq_ z^.z3  -- normalizes new x axis

		-- z': this handles the special case of z==0,±1,0, where the new x axis should be 1,0,0, (and the new z axis should be 0,0,∓1).
		z' | sqrt (sq_ (z^.x3) + sq_ (z^.z3)) <= smallNum = Vec3 0.0 0.0 1.0 | otherwise = z

tilt3zReverse :: (Floating a, Num a, Fractional a, RealFloat a, SmallNum a) => Vec3 a -> Mat4 a
tilt3zReverse = m3to4 . tilt3zReverseSimple

tilt3zReverseSimple :: (Floating a, Num a, Fractional a, RealFloat a, SmallNum a) => Vec3 a -> Mat3 a
tilt3zReverseSimple (Vec3 x y z) = tilt3zSimple $ Vec3 (-x) (-y) z

-- | Rotate horizontally (xy plane around z axis), then rotate vertically by
-- treating x and y as a single number by magnitude and rotating with z.
tilt3y :: (Floating a, Num a, Fractional a, RealFloat a, SmallNum a) => Vec3 a -> Mat4 a
tilt3y = m3to4 . tilt3ySimple

-- | 'tilt3y' without homogeneous coordinates.
--
-- new x axis's z is 0.  Without horizontal tilt, new z axis's x is 0, but then
-- add horizontal rotate.
--
-- The new x axis is the 2D y vector without z rotated right 45 degrees (like
-- multiplying a complex number by i but in reverse), renormalized to a unit
-- vector.  However, if the 2D vector is 0, default it to 0,1 (the forward2
-- vector).
--
-- The new y axis is the argument provided.
--
-- The new z axis is Vec2 (Vec2 (y^.x3) (y^.y2)) (y^.z3) similarly rotated 90
-- degrees counter-clockwise, where the sub-vector has an ‘r2’ view (it scales
-- in number).  (This is like multiplying that vector by ‘i’ in complex number
-- representation.)
tilt3ySimple :: (Floating a, Num a, Fractional a, RealFloat a, SmallNum a) => Vec3 a -> Mat3 a
tilt3ySimple y_ = Mat3 $ Vec3
	-- new x axis           new y axis  new z axis
	( Vec3 ( y'^.y3 / yhr') (y^.x3)     (-(y'^.x3/yhr')*(y^.z3)) )
	( Vec3 (-y'^.x3 / yhr') (y^.y3)     (-(y'^.y3/yhr')*(y^.z3)) )
	( Vec3 0.0              (y^.z3)     (yhr                   ) )
	where
		sq_ a = a * a
		y = v3normalize y_
		yhr = Vec2 (y^.x3) (y^.y3) ^. r2
		yhr' = Vec2 (y'^.x3) (y'^.y3) ^. r2  -- = sqrt $ sq_ y^.x3 + sq_ y^.y3  -- normalizes new x axis
		--yh = Vec2 (y'^.x3) (y'^.y3) -- The 2D vector mentioned above.
		--yhr = yh^. r2  -- = sqrt $ sq_ y^.x3 + sq_ y^.y3  -- normalizes new x axis

		-- y': this handles the special case of y==0,0,±1, where the new x axis should be 1,0,0, (and the new z axis should be 0,∓1,0).
		y' | sqrt (sq_ (y^.x3) + sq_ (y^.y3)) <= smallNum = Vec3 0.0 1.0 0.0 | otherwise = y
		-- TODO: tilt3z probably needs fixes too

tilt3yReverse :: (Floating a, Num a, Fractional a, RealFloat a, SmallNum a) => Vec3 a -> Mat4 a
tilt3yReverse = m3to4 . tilt3yReverseSimple

tilt3yReverseSimple :: (Floating a, Num a, Fractional a, RealFloat a, SmallNum a) => Vec3 a -> Mat3 a
tilt3yReverseSimple (Vec3 x y z) = tilt3ySimple $ Vec3 (-x) y (-z)

-- | Rotate x radians about the axis pointing in direction, which intersects the origin.
--
-- One way to solve this is to compose tilting to the axis, then rotating the x
-- and y axes (w/ x axis cos t, sin t; y axis -sin t, cos t), and then reversing
-- the tilt, and then simplifying the result.
--
-- First, take the tilt to the axis, M0:
--
-- 	(Vec3 (sqrt$ 1 - sq (z^.x3)) 0.0                    (z^.x3))
-- 	(Vec3 0.0                    (sqrt$ 1 - sq (z^.y3)) (z^.y3))
-- 	(Vec3 (            -(z^.x3)) (            -(z^.y3)) (z^.z3))
--
-- Then take the xy rotate about theta, M1:
-- 	(Vec3 (cos t)  (-sin t)  0.0)
-- 	(Vec3 (sin t)  ( cos t)  0.0)
-- 	(Vec3 0.0      0.0       1.0)
--
-- Now take M1*M0:
-- 	(Vec3 ((sqrt$ 1 - sq (z^.x3)) * cos t) (-(sqrt$ 1 - sq (z^.y3)) * sin t) ((z^.x3) * cos t - (z^.y3) * sin t))
-- 	(Vec3 ((sqrt$ 1 - sq (z^.x3)) * sin t) ( (sqrt$ 1 - sq (z^.y3)) * cos t) ((z^.x3) * sin t + (z^.y3) * cos t))
-- 	(Vec3 (-(z^.x3))                       (-(z^.y3))                        (z^.z3)                            )
--
-- Now take M2, reverse tilt:
-- 	(Vec3 (sqrt$ 1 - sq (z^.x3)) 0.0                    (-(z^.x3)))
-- 	(Vec3 0.0                    (sqrt$ 1 - sq (z^.y3)) (-(z^.y3)))
-- 	(Vec3 (             (z^.x3)) (             (z^.y3)) (  z^.z3))
--
-- Now take M2*(M1*M0):
-- 	-- new x axis                                                                                                 new y axis                                                                                                    new z axis
-- 	(Vec3 ((sqrt$ 1 - sq (z^.x3)) * cos t * (sqrt$ 1 - sq (z^.x3))                             + (z^.x3)*(z^.x3)) ((-(sqrt$ 1 - sq (z^.y3)) * sin t) * (sqrt$ 1 - sq (z^.x3))                                + (z^.y3)*(z^.x3)) (((z^.x3) * cos t - (z^.y3) * sin t) * (sqrt$ 1 - sq (z^.x3))                                  - (z^.z3)*(z^.x3)))
-- 	(Vec3 ((sqrt$ 1 - sq (z^.x3)) * sin t * (sqrt$ 1 - sq (z^.y3))                             + (z^.x3)*(z^.y3)) (( (sqrt$ 1 - sq (z^.y3)) * cos t) * (sqrt$ 1 - sq (z^.y3))                                + (z^.y3)*(z^.y3)) (((z^.x3) * sin t + (z^.y3) * cos t) * (sqrt$ 1 - sq (z^.y3))                                  - (z^.z3)*(z^.y3)))
-- 	(Vec3 ((sqrt$ 1 - sq (z^.x3)) * cos t * (z^.x3) + (sqrt$ 1 - sq (z^.x3)) * sin t * (z^.y3) - (z^.x3)*(z^.z3)) ((-(sqrt$ 1 - sq (z^.y3)) * sin t) * (z^.x3) + ( (sqrt$ 1 - sq (z^.y3)) * cos t) * (z^.y3) - (z^.y3)*(z^.z3)) (((z^.x3) * cos t - (z^.y3) * sin t) * (z^.x3) + ((z^.x3) * sin t + (z^.y3) * cos t) * (z^.y3) + (z^.z3)*(z^.z3)))
--
-- Now simplify/rearrange M2*M1*M0:
-- 	-- new x axis                                                                          new y axis                                                                          new z axis
-- 	(Vec3 ((      1 - sq (z^.x3)) * cos t                               + (z^.x3)*(z^.x3)) ((-(sqrt$ 1 - sq (z^.y3)) * sin t) * (sqrt$ 1 - sq (z^.x3))      + (z^.y3)*(z^.x3)) (((z^.x3) * cos t - (z^.y3) * sin t) * (sqrt$ 1 - sq (z^.x3))                                  - (z^.z3)*(z^.x3)))
-- 	(Vec3 ((sqrt$ 1 - sq (z^.x3)) * sin t * (sqrt$ 1 - sq (z^.y3))      + (z^.x3)*(z^.y3)) (  (      1 - sq (z^.y3)) * cos t                                + (z^.y3)*(z^.y3)) (((z^.x3) * sin t + (z^.y3) * cos t) * (sqrt$ 1 - sq (z^.y3))                                  - (z^.z3)*(z^.y3)))
-- 	(Vec3 ((sqrt$ 1 - sq (z^.x3)) * (cos t * (z^.x3) + sin t * (z^.y3)) - (z^.x3)*(z^.z3)) (  (sqrt$ 1 - sq (z^.y3)) * (-sin t * (z^.x3) + cos t * (z^.y3)) - (z^.y3)*(z^.z3)) (((z^.x3) * cos t - (z^.y3) * sin t) * (z^.x3) + ((z^.x3) * sin t + (z^.y3) * cos t) * (z^.y3) + (z^.z3)*(z^.z3)))
--
-- This is our rotation matrix.
--
-- I hope I didn't make a mistake somewhere.
rotate3 :: (Fractional a, RealFloat a) => SimpleRotation a -> Mat4 a
rotate3 = m3to4 . rotate3Simple

-- | Rotate x radians about the axis pointing in direction, which intersects the origin.
rotate3Simple :: (RealFloat a, Floating a) => SimpleRotation a -> Mat3 a
rotate3Simple sr = rotate3Simple_ $ sr & (srOriginAxis %~ v3orWith (Vec3 0.0 0.0 1.0) . v3normalize)

-- | Skip normalizing the axis and assume it's already normal.
rotate3Simple_ :: (Num a, Floating a) => SimpleRotation a -> Mat3 a
rotate3Simple_ sr = Mat3 $ Vec3
	-- new x axis                       new y axis                      new z axis
	(Vec3 (x'_ * c               + x*x) (-y'  * s * x'           + y*x) ((x * c - y * s) * x'                      - z*x))
	(Vec3 (x'  * s * y'          + x*y) ( y'_ * c                + y*y) ((x * s + y * c) * y'                      - z*y))
	(Vec3 (x'  * (c * x + s * y) - x*z) ( y'  * (-s * x + c * y) - y*z) ((x * c - y * s) * x + (x * s + y * c) * y + z*z))
	where
		sq_ a = a*a
		x = zaxis^.x3
		y = zaxis^.y3
		z = zaxis^.z3
		zaxis = sr^.srOriginAxis
		t = sr^.srCcwAngle
		c = cos t
		s = sin t
		x'_ = 1 - sq_ x
		y'_ = 1 - sq_ y
		x' = sqrt $ x'_
		y' = sqrt $ y'_

-- | Uses homogeneous coordinates on top of 3 dimensions.
identityTransformation3 :: (Fractional a) => Mat4 a
identityTransformation3 = Mat4 $ Vec4
	(Vec4 1.0 0.0 0.0 0.0)
	(Vec4 0.0 1.0 0.0 0.0)
	(Vec4 0.0 0.0 1.0 0.0)
	(Vec4 0.0 0.0 0.0 1.0)

-- | Does not use homogeneous coordinates on top of 3 dimensions.
identityTransformation3Simple :: (Fractional a) => Mat3 a
identityTransformation3Simple = Mat3 $ Vec3
	(Vec3 1.0 0.0 0.0)
	(Vec3 0.0 1.0 0.0)
	(Vec3 0.0 0.0 1.0)

rotatexy :: (Floating a) => a -> Mat4 a
rotatexy = m3to4 . rotatexySimple

rotatexz :: (Floating a) => a -> Mat4 a
rotatexz = m3to4 . rotatexzSimple

rotateyz :: (Floating a) => a -> Mat4 a
rotateyz = m3to4 . rotateyzSimple

-- | Aim left: spin (CW) to the right.
--
-- (When the world is spinning clockwise, it looks like you are aiming left.)
rotatexySimple :: (Floating a) => a -> Mat3 a
rotatexySimple t = Mat3 $ Vec3
	(Vec3 c    s   0.0)
	(Vec3 (-s) c   0.0)
	(Vec3 0.0  0.0 1.0)
	where (c, s) = (cos t, sin t)

-- | Tilt the axes right.
rotatexzSimple :: (Floating a) => a -> Mat3 a
rotatexzSimple t = Mat3 $ Vec3
	(Vec3 c    0.0 s  )
	(Vec3 0.0  1.0 0.0)
	(Vec3 (-s) 0.0 c  )
	where (c, s) = (cos t, sin t)

-- | Aim up: move e.g. y axis down.
--
-- (When the world is being rotated downward, it looks like you are aiming up.)
rotateyzSimple :: (Floating a) => a -> Mat3 a
rotateyzSimple t = Mat3 $ Vec3
	(Vec3 1.0 0.0  0.0)
	(Vec3 0.0 c    s  )
	(Vec3 0.0 (-s) c  )
	where (c, s) = (cos t, sin t)

-- | For each on the first row, multiply the element by the determinent of the
-- submatrix modulo width, to the base case of a 2x2 matrix.  Sum.
-- If you ignored sign then 1x1 could be the base case.
--
-- TODO: double check and probably fix this; implementation might be incorrect.
determinant4 :: (Num a) => Mat4 a -> a
determinant4 (Mat4 (Vec4
		(Vec4 v0_0 v0_1 v0_2 v0_3)
		(Vec4 v1_0 v1_1 v1_2 v1_3)
		(Vec4 v2_0 v2_1 v2_2 v2_3)
		(Vec4 v3_0 v3_1 v3_2 v3_3)
	)) =
		v0_0*(v1_1*(v2_2*v3_3 - v2_3*v3_2) + v1_2*(v2_3*v3_1 - v2_1*v3_3) + v1_3*(v2_1*v3_2 - v2_2*v3_1)) +
		v0_1*(v1_2*(v2_3*v3_4 - v2_4*v3_3) + v1_3*(v2_4*v3_2 - v2_2*v3_4) + v1_4*(v2_2*v3_3 - v2_3*v3_2)) +
		v0_2*(v1_3*(v2_4*v3_5 - v2_5*v3_4) + v1_4*(v2_5*v3_3 - v2_3*v3_5) + v1_5*(v2_3*v3_4 - v2_4*v3_3)) +
		v0_3*(v1_4*(v2_5*v3_6 - v2_6*v3_5) + v1_5*(v2_6*v3_4 - v2_4*v3_6) + v1_6*(v2_4*v3_5 - v2_5*v3_4))
	where
		(Vec4 _v0_4 _v0_5 _v0_6 _v0_7) = (Vec4 v0_0 v0_1 v0_2 v0_3)
		(Vec4  v1_4  v1_5  v1_6 _v1_7) = (Vec4 v1_0 v1_1 v1_2 v1_3)
		(Vec4  v2_4  v2_5  v2_6 _v2_7) = (Vec4 v2_0 v2_1 v2_2 v2_3)
		(Vec4  v3_4  v3_5  v3_6 _v3_7) = (Vec4 v3_0 v3_1 v3_2 v3_3)

-- | TODO: document.
--
-- TODO: double check and probably fix this; implementation might be incorrect.
determinant3 :: (Num a) => Mat3 a -> a
determinant3 (Mat3 (Vec3
		(Vec3 v0_0 v0_1 v0_2)
		(Vec3 v1_0 v1_1 v1_2)
		(Vec3 v2_0 v2_1 v2_2)
	)) = v0_0*(v1_1*v2_2 - v1_2*v2_1) + v0_1*(v1_2*v2_0 - v1_0*v2_2) + v0_2*(v1_0*v2_1 - v1_1*v2_0)

smalld :: Double
smalld = 0.001**64

smallf :: Float
smallf = 0.001**12

class SmallNum a where smallNum :: a
instance {-# OVERLAPPING  #-} SmallNum Double where smallNum = smalld
instance {-# OVERLAPPING  #-} SmallNum Float where smallNum = smallf
instance {-# OVERLAPPABLE #-} (Fractional a) => SmallNum a where smallNum = realToFrac $ smallf

equivalentSmall :: (SmallNum a, Ord a, Num a) => a -> a -> Bool
equivalentSmall x y = abs (y - x) <= smallNum

eq2 :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec2 a -> Vec2 a -> Bool
eq2 a b = (b - a)^.r2 <= smallNum

eq3 :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec3 a -> Vec3 a -> Bool
eq3 a b = (b - a)^.r3 <= smallNum

eq4 :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec4 a -> Vec4 a -> Bool
eq4 a b = (b - a)^.r4 <= smallNum

eqm3 :: (SmallNum a, Ord a, Num a, RealFloat a) => Mat3 a -> Mat3 a -> Bool
eqm3 a b = let (Mat3 rows) = abs <$> ((-) <$> b <*> a) in ((^.r3) <$> rows)^.r3 <= smallNum

eqm4 :: (SmallNum a, Ord a, Num a, RealFloat a) => Mat4 a -> Mat4 a -> Bool
eqm4 a b = let (Mat4 rows) = abs <$> ((-) <$> b <*> a) in ((^.r4) <$> rows)^.r4 <= smallNum

smallishd :: Double
smallishd = 0.1**05

smallishf :: Float
smallishf = 0.1**05

class SmallishNum a where smallishNum :: a
instance {-# OVERLAPPING  #-} SmallishNum Double where smallishNum = smallishd
instance {-# OVERLAPPING  #-} SmallishNum Float where smallishNum = smallishf
instance {-# OVERLAPPABLE #-} (Fractional a) => SmallishNum a where smallishNum = realToFrac $ smallishf

near :: (SmallishNum a, Ord a, Num a) => a -> a -> Bool
near x y = abs (y - x) <= smallishNum

near2 :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec2 a -> Vec2 a -> Bool
near2 a b = (b - a)^.r2 <= smallishNum

near3 :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec3 a -> Vec3 a -> Bool
near3 a b = (b - a)^.r3 <= smallishNum

near4 :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec4 a -> Vec4 a -> Bool
near4 a b = (b - a)^.r4 <= smallishNum

nearm3 :: (SmallishNum a, Ord a, Num a, RealFloat a) => Mat3 a -> Mat3 a -> Bool
nearm3 a b = let (Mat3 rows) = abs <$> ((-) <$> b <*> a) in ((^.r3) <$> rows)^.r3 <= smallishNum

nearm4 :: (SmallishNum a, Ord a, Num a, RealFloat a) => Mat4 a -> Mat4 a -> Bool
nearm4 a b = let (Mat4 rows) = abs <$> ((-) <$> b <*> a) in ((^.r4) <$> rows)^.r4 <= smallishNum

rankNonzerov4 :: (SmallNum a, Ord a, Num a) => Vec4 a -> Integer
rankNonzerov4 (Vec4 x y z w)
	| abs x > smallNum = 4
	| abs y > smallNum = 3
	| abs z > smallNum = 2
	| abs w > smallNum = 1
	| otherwise        = 0

rankNonzerov3 :: (SmallNum a, Ord a, Num a) => Vec3 a -> Integer
rankNonzerov3 (Vec3 x y z)
	| abs x > smallNum = 3
	| abs y > smallNum = 2
	| abs z > smallNum = 1
	| otherwise        = 0

sortRowsRankNonzerom4 :: (SmallNum a, Ord a, Num a) => Mat4 a -> Mat4 a
sortRowsRankNonzerom4 (Mat4 (Vec4 r0 r1 r2_ r3_)) =
	-- Merge sort.
	if' (rn0 <= rn1) (
		if' (rn2 <= rn3) (
			-- Now merge (max 3 comparisons).
			if' (rn0 <= rn2) (
				if' (rn1 <= rn2) (
					m4 r0 r1 r2_ r3_
				) (
					if' (rn1 <= rn3) (m4 r0 r2_ r1 r3_) (m4 r0 r2_ r3_ r1)
				)
			) (
				if' (rn0 <= rn3) (
					if' (rn1 <= rn3) (m4 r2_ r0 r1 r3_) (m4 r2_ r0 r3_ r1)
				) (
					m4 r2_ r3_ r0 r1
				)
			)
		) (
			-- Copy and swap r3_ and r2_.
			if' (rn0 <= rn3) (
				if' (rn1 <= rn3) (
					m4 r0 r1 r3_ r2_
				) (
					if' (rn1 <= rn2) (m4 r0 r3_ r1 r2_) (m4 r0 r3_ r2_ r1)
				)
			) (
				if' (rn0 <= rn2) (
					if' (rn1 <= rn2) (m4 r3_ r0 r1 r2_) (m4 r3_ r0 r2_ r1)
				) (
					m4 r3_ r2_ r0 r1
				)
			)
		)
	) (
		-- Copy and swap r0 and r1.
		if' (rn2 <= rn3) (
			if' (rn1 <= rn2) (
				if' (rn0 <= rn2) (
					m4 r0 r1 r2_ r3_
				) (
					if' (rn0 <= rn3) (m4 r0 r2_ r1 r3_) (m4 r0 r2_ r3_ r1)
				)
			) (
				if' (rn1 <= rn3) (
					if' (rn0 <= rn3) (m4 r2_ r0 r1 r3_) (m4 r2_ r0 r3_ r1)
				) (
					m4 r2_ r3_ r0 r1
				)
			)
		) (
			if' (rn1 <= rn3) (
				if' (rn0 <= rn3) (
					m4 r0 r1 r3_ r2_
				) (
					if' (rn0 <= rn2) (m4 r0 r3_ r1 r2_) (m4 r0 r3_ r2_ r1)
				)
			) (
				if' (rn1 <= rn2) (
					if' (rn0 <= rn2) (m4 r3_ r0 r1 r2_) (m4 r3_ r0 r2_ r1)
				) (
					m4 r3_ r2_ r0 r1
				)
			)
		)
	)
	where
		rn0 = -rankNonzerov4 r0
		rn1 = -rankNonzerov4 r1
		rn2 = -rankNonzerov4 r2_
		rn3 = -rankNonzerov4 r3_
		m4 r0_ r1_ r2_2 r3__ = Mat4 $ Vec4 r0_ r1_ r2_2 r3__

sortRowsRankNonzerom3 :: (SmallNum a, Ord a, Num a) => Mat3 a -> Mat3 a
sortRowsRankNonzerom3 (Mat3 (Vec3 r0 r1 r2_)) =
	if' (rn0 <= rn1) (
		if' (rn1 <= rn2) (
			m3 r0 r1 r2_
		) (
			if' (rn0 <= rn2) (m3 r0 r2_ r1) (m3 r2_ r1 r0)
		)
	) (
		if' (rn0 <= rn2) (
			m3 r1 r0 r2_
		) (
			if' (rn1 <= rn2) (m3 r1 r2_ r0) (m3 r2_ r0 r1)
		)
	)
	where
		rn0 = -rankNonzerov3 r0
		rn1 = -rankNonzerov3 r1
		rn2 = -rankNonzerov3 r2_
		m3 r0_ r1_ r2_2 = Mat3 $ Vec3 r0_ r1_ r2_2

inversem4 :: (SmallNum a, Ord a, Num a, Fractional a) => Mat4 a -> Mat4 a
inversem4 = inversem4GaussianElimination

inversem3 :: (SmallNum a, Ord a, Num a, Fractional a) => Mat3 a -> Mat3 a
inversem3 = inversem3GaussianElimination

-- | Inverse.  Method by Gaussian elimination.  Non-zero determinant will avoid
-- divide by zero in the matrix, since then the vectors are linearly
-- independent without a vector to ‘collapse’ the hypervolume by multiplying by 0.
--
-- 	(Vec4 v0_0 v0_1 v0_2 v0_3) (Vec4 1.0 0.0 0.0 0.0)
-- 	(Vec4 v1_0 v1_1 v1_2 v1_3) (Vec4 0.0 1.0 0.0 0.0)
-- 	(Vec4 v2_0 v2_1 v2_2 v2_3) (Vec4 0.0 0.0 1.0 0.0)
-- 	(Vec4 v3_0 v3_1 v3_2 v3_3) (Vec4 0.0 0.0 0.0 1.0)
--
-- 	(Vec4 v0_0 v0_1 v0_2 v0_3)
-- 	(Vec4 0.0  v1_1 v1_2 v1_3)  - (v1_0/v0_0)*r0
-- 	(Vec4 0.0  v2_1 v2_2 v2_3)  - (v2_0/v0_0)*r0
-- 	(Vec4 0.0  v3_1 v3_2 v3_3)  - (v3_0/v0_0)*r0
--
-- 	(Vec4 v0_0 v0_1 v0_2 v0_3)
-- 	(Vec4 0.0  v1_1 v1_2 v1_3)
-- 	(Vec4 0.0  0.0  v2_2 v2_3)  - (v2_1/v1_1)*r1
-- 	(Vec4 0.0  0.0  v3_2 v3_3)  - (v3_1/v1_1)*r1
--
-- 	(Vec4 v0_0 v0_1 v0_2 v0_3)
-- 	(Vec4 0.0  v1_1 v1_2 v1_3)
-- 	(Vec4 0.0  0.0  v2_2 v2_3)
-- 	(Vec4 0.0  0.0  0.0  v3_3)  - (v3_2/v2_2)*r2
--
-- 	(Vec4 v0_0 v0_1 v0_2 0.0 )  - (v0_3/v3_3)*r3
-- 	(Vec4 0.0  v1_1 v1_2 0.0 )  - (v1_3/v3_3)*r3
-- 	(Vec4 0.0  0.0  v2_2 0.0 )  - (v2_3/v3_3)*r3
-- 	(Vec4 0.0  0.0  0.0  v3_3)
--
-- 	(Vec4 v0_0 v0_1 0.0  0.0 )  - (v0_2/v2_2)*r2
-- 	(Vec4 0.0  v1_1 0.0  0.0 )  - (v1_2/v2_2)*r2
-- 	(Vec4 0.0  0.0  v2_2 0.0 )
-- 	(Vec4 0.0  0.0  0.0  v3_3)
--
-- 	(Vec4 v0_0 0.0  0.0  0.0 )  - (v0_1/v1_1)*r1
-- 	(Vec4 0.0  v1_1 0.0  0.0 )
-- 	(Vec4 0.0  0.0  v2_2 0.0 )
-- 	(Vec4 0.0  0.0  0.0  v3_3)
--
-- 	(Vec4 1.0 0.0 0.0 0.0)  - / v0_0
-- 	(Vec4 0.0 1.0 0.0 0.0)  - / v1_1
-- 	(Vec4 0.0 0.0 1.0 0.0)  - / v2_2
-- 	(Vec4 0.0 0.0 0.0 1.0)  - / v3_3
--
-- Between each 0 operation there is a sort.
--
-- Now apply these operations to the identity matrix to get the inverse.
inversem4GaussianElimination :: (SmallNum a, Ord a, Num a, Fractional a) => Mat4 a -> Mat4 a
inversem4GaussianElimination m =
	b sortRowsRankNonzerom4 >>> (\(mn, mi) -> (
		b (r1_4 %~ (`minusv4` (((mn^.e1_0_4)/(mn^.e0_0_4)) `sv4` (mn^.r0_4)))) >>>
		b (r2_4 %~ (`minusv4` (((mn^.e2_0_4)/(mn^.e0_0_4)) `sv4` (mn^.r0_4)))) >>>
		b (r3_4 %~ (`minusv4` (((mn^.e3_0_4)/(mn^.e0_0_4)) `sv4` (mn^.r0_4))))
		) $ (mn, mi)) >>>
	b sortRowsRankNonzerom4 >>> (\(mn, mi) -> (
		b (r2_4 %~ (`minusv4` (((mn^.e2_1_4)/(mn^.e1_1_4)) `sv4` (mn^.r1_4)))) >>>
		b (r3_4 %~ (`minusv4` (((mn^.e3_1_4)/(mn^.e1_1_4)) `sv4` (mn^.r1_4))))
		) $ (mn, mi)) >>>
	b sortRowsRankNonzerom4 >>> (\(mn, mi) -> (
		b (r3_4 %~ (`minusv4` (((mn^.e3_2_4)/(mn^.e2_2_4)) `sv4` (mn^.r2_4))))
		) $ (mn, mi)) >>>

	(\(mn, mi) -> (
		b (r0_4 %~ (`minusv4` (((mn^.e0_3_4)/(mn^.e3_3_4)) `sv4` (mn^.r3_4)))) >>>
		b (r1_4 %~ (`minusv4` (((mn^.e1_3_4)/(mn^.e3_3_4)) `sv4` (mn^.r3_4)))) >>>
		b (r2_4 %~ (`minusv4` (((mn^.e2_3_4)/(mn^.e3_3_4)) `sv4` (mn^.r3_4))))
		) $ (mn, mi)) >>>
	(\(mn, mi) -> (
		b (r0_4 %~ (`minusv4` (((mn^.e0_2_4)/(mn^.e2_2_4)) `sv4` (mn^.r2_4)))) >>>
		b (r1_4 %~ (`minusv4` (((mn^.e1_2_4)/(mn^.e2_2_4)) `sv4` (mn^.r2_4))))
		) $ (mn, mi)) >>>
	(\(mn, mi) -> (
		b (r0_4 %~ (`minusv4` (((mn^.e0_1_4)/(mn^.e1_1_4)) `sv4` (mn^.r1_4))))
		) $ (mn, mi)) >>>

	(\(mn, mi) -> (
		b (r0_4 %~ ((1/(mn^.e0_0_4)) `sv4`)) >>>
		b (r1_4 %~ ((1/(mn^.e1_1_4)) `sv4`)) >>>
		b (r2_4 %~ ((1/(mn^.e2_2_4)) `sv4`)) >>>
		b (r3_4 %~ ((1/(mn^.e3_3_4)) `sv4`))
		) $ (mn, mi)) >>>

	snd $ (m, (
		Mat4 $ Vec4
			(Vec4 1.0 0.0 0.0 0.0)
			(Vec4 0.0 1.0 0.0 0.0)
			(Vec4 0.0 0.0 1.0 0.0)
			(Vec4 0.0 0.0 0.0 1.0)
	))
	where b = join (***)

inversem3GaussianElimination :: (SmallNum a, Ord a, Num a, Fractional a) => Mat3 a -> Mat3 a
inversem3GaussianElimination m =
	b sortRowsRankNonzerom3 >>> (\(mn, mi) -> (
		b (r1_3 %~ (`minusv3` (((mn^.e1_0_3)/(mn^.e0_0_3)) `sv3` (mn^.r0_3)))) >>>
		b (r2_3 %~ (`minusv3` (((mn^.e2_0_3)/(mn^.e0_0_3)) `sv3` (mn^.r0_3))))
		) $ (mn, mi)) >>>
	b sortRowsRankNonzerom3 >>> (\(mn, mi) -> (
		b (r2_3 %~ (`minusv3` (((mn^.e2_1_3)/(mn^.e1_1_3)) `sv3` (mn^.r1_3))))
		) $ (mn, mi)) >>>

	(\(mn, mi) -> (
		b (r0_3 %~ (`minusv3` (((mn^.e0_2_3)/(mn^.e2_2_3)) `sv3` (mn^.r2_3)))) >>>
		b (r1_3 %~ (`minusv3` (((mn^.e1_2_3)/(mn^.e2_2_3)) `sv3` (mn^.r2_3))))
		) $ (mn, mi)) >>>
	(\(mn, mi) -> (
		b (r0_3 %~ (`minusv3` (((mn^.e0_1_3)/(mn^.e1_1_3)) `sv3` (mn^.r1_3))))
		) $ (mn, mi)) >>>

	(\(mn, mi) -> (
		b (r0_3 %~ ((1/(mn^.e0_0_3)) `sv3`)) >>>
		b (r1_3 %~ ((1/(mn^.e1_1_3)) `sv3`)) >>>
		b (r2_3 %~ ((1/(mn^.e2_2_3)) `sv3`))
		) $ (mn, mi)) >>>

	snd $ (m, (
		Mat3 $ Vec3
			(Vec3 1.0 0.0 0.0)
			(Vec3 0.0 1.0 0.0)
			(Vec3 0.0 0.0 1.0)
	))
	where b = join (***)

-- | perspective.
--
-- e.g. for a simple right-angled fov with a (0,0,1) perspective vector,
-- a point (2,4,1,1) corresponds to (2,4,2,2), which is (1,2,1).  At further
-- distance from the viewer, points should seem smaller (closer to 0 x and y
-- coordinates), so where at z=0 the x and y coordinates effectively remain
-- unchanged, at z=1 they are halved.  And a point (2,4,3,1) is twice as far
-- away as the first point relative to the viewer z=-1 (z=0 is the inner
-- plane), and it corresponds / maps back to coordinates in the original
-- coordinate system (2,4,6,4), which is (0.5, 1, 1.5) after applying homogenous
-- coordinates (divide coords by last coordinate).  (3/2=1.5)  (2z/(z+1))
--
-- A third example is a point (2,4,5,1) mapping back to original axes as
-- (2,4,10,6), which is equivalent to (1/3, 2/3, 5/3 = 1.66…).  It is three
-- times as far away as the first point relative to the viewer z=-1.
perspective :: (Num a, Fractional a) => Vec3 a -> Mat4 a
perspective v = Mat4 $ Vec4
	(Vec4 (1+v^.x3) 0.0       0.0       0.0)
	(Vec4 0.0       (1+v^.y3) 0.0       0.0)
	(Vec4 0.0       0.0       (1+v^.z3) 0.0)
	(Vec4 (v^.x3)   (v^.y3)   (v^.z3)   1.0)

-- | preserves z and does not shift by 1 but is not invertible.
--
-- With linear non-independence, it collapses into 3 dimensions without a nonzero determinant.
perspectivePure :: (Num a, Fractional a) => Vec3 a -> Mat4 a
perspectivePure v = Mat4 $ Vec4
	(Vec4 (1+v^.x3) 0.0       0.0       0.0)
	(Vec4 0.0       (1+v^.y3) 0.0       0.0)
	(Vec4 0.0       0.0       (1+v^.z3) 0.0)
	(Vec4 (v^.x3)   (v^.y3)   (v^.z3)   0.0)

-- | fov.
--
-- For a 2D space with the y coordinate determining distance, with doubling
-- distance halving the other coordinates (x), you can find the the length of
-- the left side of the following triangle knowing angle theta and the length
-- of the top side, which is x, which is 1, where we want to know, for each
-- increase by 1 of the z or y or distance coordinate, what the increase in the
-- homogoneous coordinate w is, where the result of the homogeneous coordinate is
-- (x/w, y/w, z/w).
--
--  ^
--  |--.
--  |  /
--  | /
--  |θ/
--  |/
-- -+----->
--  |
--
-- Then we can generalize to 3 dimensions.
--
-- To preserve the original distance coordinate, .
--
-- Also preserving a nonzero determinant, the perspective is with z shifted
-- back by 1 unit, so that z=0 is not the eye origin, but the inner plane, for
-- a unit w or homogenous coordinate.
--
-- e.g. a right angled fov makes for a z component of 1, for a 1:1 ratio.
fov :: (Fractional a, Floating a) => a -> Mat4 a
fov t = perspective $ Vec3 0.0 (1 / tan (t/2)) 0.0

fovPure :: (Fractional a, Floating a) => a -> Mat4 a
fovPure t = perspectivePure $ Vec3 0.0 (1 / tan (t/2)) 0.0

zv2 :: (Num a) => Vec2 a
zv2 = rv2 0

zv3 :: (Num a) => Vec3 a
zv3 = rv3 0

zv4 :: (Num a) => Vec4 a
zv4 = rv4 0

rm3 :: a -> Mat3 a
rm3 z = Mat3 $ Vec3
	(rv3 z)
	(rv3 z)
	(rv3 z)

rm4 :: a -> Mat4 a
rm4 z = Mat4 $ Vec4
	(rv4 z)
	(rv4 z)
	(rv4 z)
	(rv4 z)

zm3 :: (Fractional a) => Mat3 a
zm3 = rm3 0.0

zm4 :: (Fractional a) => Mat4 a
zm4 = rm4 0.0

type MView = MViewd

type MViewd = MView' Double
data MView' a = MView {
	_mviewPos    :: Vec3 a,
	_mviewTarget :: Vec3 a,
	-- | Whole fov (not half).
	_mviewFov    :: a
}
	deriving (Eq, Ord, Show)
makeLenses ''MView'

-- | Translate, then rotate, then fov.
viewMat :: (Num a, Fractional a, Floating a, RealFloat a, SmallNum a) => MView' a -> Mat4 a
viewMat = viewMat' False

-- | Translate, then rotate, then fov or fovPure.
viewMat' :: (Num a, Fractional a, Floating a, RealFloat a, SmallNum a) => Bool -> MView' a -> Mat4 a
viewMat' viewCollapse v =
	fov'          (v^.mviewFov) <>
	tilt3yReverse ((v^.mviewTarget) `minusv3` (v^.mviewPos)) <>
	translate3    (-v^.mviewPos)
	where
		fov' = if' viewCollapse fovPure fov

-- | Swap y and z axes.
--
-- OpenGL expects z to be ‘forward’ with depth, and y to be vertically, whereas
-- world positioning expects y to be ‘forward’ towards the camera target, and z to
-- be up and down vertically.
worldToGL :: (Num a, Fractional a) => Mat4 a
worldToGL = Mat4 $ Vec4
	(Vec4 1.0 0.0 0.0 0.0)
	(Vec4 0.0 0.0 1.0 0.0)
	(Vec4 0.0 1.0 0.0 0.0)
	(Vec4 0.0 0.0 0.0 1.0)

-- | 'worldToGL' in a 3x3 matrix.
worldToGLSimple :: (Num a, Fractional a) => Mat3 a
worldToGLSimple = Mat3 $ Vec3
	(Vec3 1.0 0.0 0.0)
	(Vec3 0.0 0.0 1.0)
	(Vec3 0.0 1.0 0.0)

-- | Shift and scale the depth value to approximate disabling OpenGL clipping
-- depth values outside a range, so that we can see the whole scene, not only
-- the part of the scene within a certain range of distance from the viewer.
rescaleDepth :: (Num a, Fractional a) => a -> a -> Mat4 a
rescaleDepth depthTranslate depthScale = Mat4 $ Vec4
	(Vec4 1.0 0.0 0.0 0.0)
	(Vec4 0.0 ds  0.0 do_)
	(Vec4 0.0 0.0 1.0 0.0)
	(Vec4 0.0 0.0 0.0 1.0)
	where
		(do_, ds) = (depthTranslate, depthScale)

-- * 3D vector aiming rotation utils in radians: horizontal and vertical aiming of a point relative to origin

-- | The vector represents where the camera at the origin is pointing.  Rotate
-- aim right by ‘radiansRight’ radians.
aimHoriz3DSimple :: (Num a, Floating a) => a -> Vec3 a -> Vec3 a
aimHoriz3DSimple radiansRight target =
	rotatexySimple radiansRight `mv3` target

-- | The vector represents where the camera at the origin is pointing.
-- Rotate aim up by ‘radiansUp’ radians.
-- Optionally cap absolute result by ‘mmaxRadius’ radians from level where z=0.
aimVert3DSimple :: (Num a, Floating a, RealFloat a, SmallNum a) => Maybe a -> a -> Vec3 a -> Vec3 a
aimVert3DSimple mmaxRadius radiansUp target@(Vec3 tx ty _tz) = (`mv3` target) $
	rotateHorizToTarget <>
	rotateVertically <>
	unrotateHorizToTarget

	where
		-- First horizontally unrotate the point back to 0,1,z so that tx becomes 0 (the point becomes in the yz plane).
		unrotateHorizToTarget = rotatexySimple (-horizCWAngle)
		-- Third rotate horizontally back to restore tx,ty's angle.
		rotateHorizToTarget   = rotatexySimple   horizCWAngle
		-- Second rotate in plane yz around x axis.
		rotateVertically = rotateyzSimple (-radiansUp')

		-- Angle when aiming right.  CW relative to 0,1 in radians.
		-- (If you take a forward vector 0,1,0 and aim right by this angle,
		-- then the result will point in the same direction as 'target'
		-- ignoring the z coordinate.)
		horizCWAngle
			| (Vec2 tx ty ^. r2) `equivalentSmall` 0.0 = 0.0  -- Technically redundant, since rotating in this case would still give you 0,0 for x,y.
			| otherwise                                =
				-((Vec2 tx ty ^. t2) - (Vec2 0.0 1.0 ^. t2))
		---- Negate angle.
		--horizCCWAngle = -horizCWAngle

		-- In order to perform clamping, get a copy of a partially transformed target (only 1st transformation).
		yz = let (Vec3 _x y z) = (`mv3` target) $ unrotateHorizToTarget in Vec2 y z

		maxRadiansUp = fromMaybe (tau/2)  (           mmaxRadius) - (yz^.t2)
		minRadiansUp = fromMaybe (-tau/2) (negate <$> mmaxRadius) - (yz^.t2)

		radiansUp' = case mmaxRadius of
			Nothing -> radiansUp
			Just _ -> min maxRadiansUp . max minRadiansUp $ radiansUp

-- * More utils

-- | Is the vector zero?
v2z :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec2 a -> Bool
v2z v = equivalentSmall (v^.r2) 0

-- | Is the vector near zero?
v2s :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec2 a -> Bool
v2s v = near (v^.r2) 0

-- | Return the vector, unless it's zero, in which case return the default.
v2nzElse :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec2 a -> Vec2 a -> Vec2 a
v2nzElse v else_ = if' (not $ v2z v) v else_

-- | Return the vector, unless it's near zero, in which case return the default.
v2nsElse :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec2 a -> Vec2 a -> Vec2 a
v2nsElse v else_ = if' (not $ v2s v) v else_

-- | Is the vector zero?
v3z :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec3 a -> Bool
v3z v = equivalentSmall (v^.r3) 0

-- | Is the vector near zero?
v3s :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec3 a -> Bool
v3s v = near (v^.r3) 0

-- | Return the vector, unless it's zero, in which case return the default.
v3nzElse :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec3 a -> Vec3 a -> Vec3 a
v3nzElse v else_ = if' (not $ v3z v) v else_

-- | Return the vector, unless it's near zero, in which case return the default.
v3nsElse :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec3 a -> Vec3 a -> Vec3 a
v3nsElse v else_ = if' (not $ v3s v) v else_

-- | Is the vector zero?
v4z :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec4 a -> Bool
v4z v = equivalentSmall (v^.r4) 0

-- | Is the vector near zero?
v4s :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec4 a -> Bool
v4s v = near (v^.r4) 0

-- | Return the vector, unless it's zero, in which case return the default.
v4nzElse :: (SmallNum a, Ord a, Num a, RealFloat a) => Vec4 a -> Vec4 a -> Vec4 a
v4nzElse v else_ = if' (not $ v4z v) v else_

-- | Return the vector, unless it's near zero, in which case return the default.
v4nsElse :: (SmallishNum a, Ord a, Num a, RealFloat a) => Vec4 a -> Vec4 a -> Vec4 a
v4nsElse v else_ = if' (not $ v4s v) v else_

-- * Subvectors

-- | Handle just the ‘xy’ vector in a Vec3.
xy3 :: forall a. Lens' (Vec3 a) (Vec2 a)
xy3 = lens getter (flip setter)
	where
		getter :: Vec3 a -> Vec2 a
		getter (Vec3 x y _) = Vec2 x y
		setter :: Vec2 a -> Vec3 a -> Vec3 a
		setter (Vec2 x y) (Vec3 _ _ z) = Vec3 x y z

-- | Handle just the ‘xz’ vector in a Vec3.
xz3 :: forall a. Lens' (Vec3 a) (Vec2 a)
xz3 = lens getter (flip setter)
	where
		getter :: Vec3 a -> Vec2 a
		getter (Vec3 x _ z) = Vec2 x z
		setter :: Vec2 a -> Vec3 a -> Vec3 a
		setter (Vec2 x z) (Vec3 _ y _) = Vec3 x y z

-- | Handle just the ‘yz’ vector in a Vec3.
yz3 :: forall a. Lens' (Vec3 a) (Vec2 a)
yz3 = lens getter (flip setter)
	where
		getter :: Vec3 a -> Vec2 a
		getter (Vec3 _ y z) = Vec2 y z
		setter :: Vec2 a -> Vec3 a -> Vec3 a
		setter (Vec2 y z) (Vec3 x _ _) = Vec3 x y z

-- | Handle just the ‘xy’ vector in a Vec4.
xy4 :: forall a. Lens' (Vec4 a) (Vec2 a)
xy4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec2 a
		getter (Vec4 x y _ _) = Vec2 x y
		setter :: Vec2 a -> Vec4 a -> Vec4 a
		setter (Vec2 x y) (Vec4 _ _ z w) = Vec4 x y z w

-- | Handle just the ‘xz’ vector in a Vec4.
xz4 :: forall a. Lens' (Vec4 a) (Vec2 a)
xz4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec2 a
		getter (Vec4 x _ z _) = Vec2 x z
		setter :: Vec2 a -> Vec4 a -> Vec4 a
		setter (Vec2 x z) (Vec4 _ y _ w) = Vec4 x y z w

-- | Handle just the ‘xw’ vector in a Vec4.
xw4 :: forall a. Lens' (Vec4 a) (Vec2 a)
xw4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec2 a
		getter (Vec4 x _ _ w) = Vec2 x w
		setter :: Vec2 a -> Vec4 a -> Vec4 a
		setter (Vec2 x w) (Vec4 _ y z _) = Vec4 x y z w

-- | Handle just the ‘yz’ vector in a Vec4.
yz4 :: forall a. Lens' (Vec4 a) (Vec2 a)
yz4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec2 a
		getter (Vec4 _ y z _) = Vec2 y z
		setter :: Vec2 a -> Vec4 a -> Vec4 a
		setter (Vec2 y z) (Vec4 x _ _ w) = Vec4 x y z w

-- | Handle just the ‘yw’ vector in a Vec4.
yw4 :: forall a. Lens' (Vec4 a) (Vec2 a)
yw4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec2 a
		getter (Vec4 _ y _ w) = Vec2 y w
		setter :: Vec2 a -> Vec4 a -> Vec4 a
		setter (Vec2 y w) (Vec4 x _ z _) = Vec4 x y z w

-- | Handle just the ‘zw’ vector in a Vec4.
zw4 :: forall a. Lens' (Vec4 a) (Vec2 a)
zw4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec2 a
		getter (Vec4 _ _ z w) = Vec2 z w
		setter :: Vec2 a -> Vec4 a -> Vec4 a
		setter (Vec2 z w) (Vec4 x y _ _) = Vec4 x y z w

-- | Handle just the ‘xyz’ vector in a Vec4.
xyz4 :: forall a. Lens' (Vec4 a) (Vec3 a)
xyz4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec3 a
		getter (Vec4 x y z _) = Vec3 x y z
		setter :: Vec3 a -> Vec4 a -> Vec4 a
		setter (Vec3 x y z) (Vec4 _ _ _ w) = Vec4 x y z w

-- | Handle just the ‘xyw’ vector in a Vec4.
xyw4 :: forall a. Lens' (Vec4 a) (Vec3 a)
xyw4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec3 a
		getter (Vec4 x y _ w) = Vec3 x y w
		setter :: Vec3 a -> Vec4 a -> Vec4 a
		setter (Vec3 x y w) (Vec4 _ _ z _) = Vec4 x y z w

-- | Handle just the ‘xzw’ vector in a Vec4.
xzw4 :: forall a. Lens' (Vec4 a) (Vec3 a)
xzw4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec3 a
		getter (Vec4 x _ z w) = Vec3 x z w
		setter :: Vec3 a -> Vec4 a -> Vec4 a
		setter (Vec3 x z w) (Vec4 _ y _ _) = Vec4 x y z w

-- | Handle just the ‘yzw’ vector in a Vec4.
yzw4 :: forall a. Lens' (Vec4 a) (Vec3 a)
yzw4 = lens getter (flip setter)
	where
		getter :: Vec4 a -> Vec3 a
		getter (Vec4 _ y z w) = Vec3 y z w
		setter :: Vec3 a -> Vec4 a -> Vec4 a
		setter (Vec3 y z w) (Vec4 x _ _ _) = Vec4 x y z w

-- | Convenient utility to square a number.
sqx :: forall a. (Num a) => a -> a
sqx x = x*x

-- * Equivalence and sign utils

-- | Determine if the number is zero, negative, or positive.
thresholdSignnum :: forall a i. (SmallNum a, Num a, Ord a, Num i) => a -> i
thresholdSignnum x
	| x `equivalentSmall` 0 =  0
	| x <=                0 = -1
	| otherwise             =  1

-- | 'nearSignum' specialized to Integer.
nearSignnum :: forall a i. (SmallishNum a, Num a, Ord a, Num i) => a -> i
nearSignnum x
	| x `near`  0 =  0
	| x <=      0 = -1
	| otherwise   =  1

-- | 'thresholdSignum' specialized to Integer.
thresholdSignnumI :: forall a. (SmallNum a, Num a, Ord a) => a -> Integer
thresholdSignnumI x
	| x `equivalentSmall` 0 =  0
	| x <=                0 = -1
	| otherwise             =  1

-- | 'nearSignum' specialized to Integer.
nearSignnumI :: forall a. (SmallishNum a, Num a, Ord a) => a -> Integer
nearSignnumI x
	| x `near`  0 =  0
	| x <=      0 = -1
	| otherwise   =  1
