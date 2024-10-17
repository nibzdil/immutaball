{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, InstanceSigs, FlexibleInstances, MultiParamTypeClasses #-}

module Immutaball.Share.Math
	(
		Vec2(..), x2, y2,
		pv2,
		sv2,
		mv2,
		d2,
		r2,
		t2,
		Vec3(..), x3, y3, z3,
		pv3,
		sv3,
		mv3,
		d3,
		Vec4(..), x4, y4, z4, w4,
		pv4,
		sv4,
		mv4,
		d4,
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
		mm3,
		mm4,
		c0_3,
		c1_3,
		c2_3,
		c0_4,
		c1_4,
		c2_4,
		c3_4,
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
		lerpWith,
		lerp,
		lerpV2,
		lerpV3,
		lerpV4,
		ilerpWith,
		ilerp,
		tau,

		WidthHeightI
	) where

import Prelude ()
import Immutaball.Prelude

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

pv2 :: (Num a) => Vec2 a -> Vec2 a -> Vec2 a
pv2 (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax + bx) (ay + by)

sv2 :: (Num a) => a -> Vec2 a -> Vec2 a
sv2 s (Vec2 x y) = Vec2 (s*x) (s*y)

mv2 :: (Num a) => Vec2 a -> Vec2 a -> Vec2 a
mv2 (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax - bx) (ay - by)

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

pv3 :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
pv3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax + bx) (ay + by) (az + bz)

sv3 :: (Num a) => a -> Vec3 a -> Vec3 a
sv3 s (Vec3 x y z) = Vec3 (s*x) (s*y) (s*z)

mv3 :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
mv3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax - bx) (ay - by) (az - bz)

d3 :: (Num a) => Vec3 a -> Vec3 a -> a
d3 (Vec3 ax ay az) (Vec3 bx by bz) = ax*bx + ay*by + az*bz

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

pv4 :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a
pv4 (Vec4 ax ay az aw) (Vec4 bx by bz bw) = Vec4 (ax + bx) (ay + by) (az + bz) (aw + bw)

sv4 :: (Num a) => a -> Vec4 a -> Vec4 a
sv4 s (Vec4 x y z w) = Vec4 (s*x) (s*y) (s*z) (s*w)

mv4 :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a
mv4 (Vec4 ax ay az aw) (Vec4 bx by bz bw) = Vec4 (ax - bx) (ay - by) (az - bz) (aw - bw)

d4 :: (Num a) => Vec4 a -> Vec4 a -> a
d4 (Vec4 ax ay az aw) (Vec4 bx by bz bw) = ax*bx + ay*by + az*bz + aw*bw

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
				offset = c1 `mv2` c0
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

lerpWith :: (a -> a -> a) -> (a -> a -> a) -> (s -> a -> a) -> a -> a -> s -> a
lerpWith plus minus scale from_ to_ v = from_ `plus` (v`scale`(to_ `minus` from_))

lerp :: (Num a) => a -> a -> a -> a
--lerp from to v = from + v*(to - from)
lerp = lerpWith (+) (-) (*)

lerpV2 :: (Num a) => Vec2 a -> Vec2 a -> a -> Vec2 a
lerpV2 = lerpWith pv2 mv2 sv2

lerpV3 :: (Num a) => Vec3 a -> Vec3 a -> a -> Vec3 a
lerpV3 = lerpWith pv3 mv3 sv3

lerpV4 :: (Num a) => Vec4 a -> Vec4 a -> a -> Vec4 a
lerpV4 = lerpWith pv4 mv4 sv4

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
