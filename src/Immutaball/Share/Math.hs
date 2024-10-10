{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, InstanceSigs #-}

module Immutaball.Share.Math
	(
		Vec2(..), x2, y2,
		pv2,
		sv2,
		mv2,
		r2,
		t2,
		Vec3(..), x3, y3, z3,
		pv3,
		sv3,
		mv3,
		Vec4(..), x4, y4, z4, w4,
		pv4,
		sv4,
		mv4,
		Mat3(..), getMat3,
		Mat4(..), getMat4,
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
		lerpWith,
		lerp
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

data Vec3 a = Vec3 {
	_x3 :: a,
	_y3 :: a,
	_z3 :: a
}
	deriving (Eq, Ord, Show)
makeLenses ''Vec3

pv3 :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
pv3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax + bx) (ay + by) (az + bz)

sv3 :: (Num a) => a -> Vec3 a -> Vec3 a
sv3 s (Vec3 x y z) = Vec3 (s*x) (s*y) (s*z)

mv3 :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
mv3 (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax - bx) (ay - by) (az - bz)

data Vec4 a = Vec4 {
	_x4 :: a,
	_y4 :: a,
	_z4 :: a,
	_w4 :: a
}
	deriving (Eq, Ord, Show)
makeLenses ''Vec4

pv4 :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a
pv4 (Vec4 ax ay az aw) (Vec4 bx by bz bw) = Vec4 (ax + bx) (ay + by) (az + bz) (aw + bw)

sv4 :: (Num a) => a -> Vec4 a -> Vec4 a
sv4 s (Vec4 x y z w) = Vec4 (s*x) (s*y) (s*z) (s*w)

mv4 :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a
mv4 (Vec4 ax ay az aw) (Vec4 bx by bz bw) = Vec4 (ax - bx) (ay - by) (az - bz) (aw - bw)

-- | Row-major, like C.
newtype Mat3 a = Mat3 { _getMat3 :: Vec3 (Vec3 a) }
	deriving (Eq, Ord, Show)
makeLenses ''Mat3
-- | Row-major, like C.
newtype Mat4 a = Mat4 { _getMat4 :: Vec4 (Vec4 a) }
	deriving (Eq, Ord, Show)
makeLenses ''Mat4

--r0_3 :: Lens' (Mat3 a) (Vec3 a)
--r1_3 :: Lens' (Mat3 a) (Vec3 a)
--r2_3 :: Lens' (Mat3 a) (Vec3 a)
--c0_3 :: Lens' (Mat3 a) (Vec3 a)
--c1_3 :: Lens' (Mat3 a) (Vec3 a)
--c2_3 :: Lens' (Mat3 a) (Vec3 a)

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

lerpWith :: (a -> a -> a) -> (a -> a -> a) -> (s -> a -> a) -> a -> a -> s -> a
lerpWith plus minus scale from_ to_ v = from_ `plus` (v`scale`(to_ `minus` from_))

lerp :: (Num a) => a -> a -> a -> a
--lerp from to v = from + v*(to - from)
lerp = lerpWith (+) (-) (*)

lerpV2 :: (Num a) => Vec2 a -> Vec2 a -> a -> Vec2 a
lerpV2 = lerpWith pv2 mv2 sv2
