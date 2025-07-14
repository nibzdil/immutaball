{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Immutaball.Share.Math.X3D.Test
	(
		main,
		testsMain,
		tests,

		simpleConstant,
		sampleLine0,
		sampleLine1,
		planeX1,
		planeZ2
	) where

--import Control.Arrow
--import Data.Functor.Identity

import Control.Lens
import Test.HUnit
--import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit hiding ((@?=), assertBool)
import Test.Tasty.QuickCheck

import Immutaball.Share.Math
import Immutaball.Share.Utils
import Test.Immutaball.Share.Math.Core.Orphans ()

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain tests

simpleConstant :: Integer
simpleConstant = 3

sampleLine0 :: Line3 Double
sampleLine0 = line3Points (Vec3 0 0 0) (Vec3 1 1 0)

sampleLine1 :: Line3 Double
sampleLine1 = line3Points (Vec3 1 0 1) (Vec3 1 3 1)

planeX1 :: Plane3 Double
planeX1 = normalPlane3 (Vec3 1 0 0) 1

planeZ2 :: Plane3 Double
planeZ2 = normalPlane3 (Vec3 0 0 1) 2

tests :: TestTree
tests = testGroup "Immutaball.Share.Math.X3D" $
	[
		testCase "simpleConstant == 3" $
			simpleConstant @?= 3,

		testGroup "plane3PointDistance" $
			[
				testCase "simple sample test" $
					(planeX1 `plane3PointDistance` Vec3 7 (-4) 88) `equivalentSmall` 6 @?= True
			],

		testGroup "pointToPlane" $
			[
				testCase "simple sample test" $
					(Vec3 7 (-4) 88 `pointToPlane` planeX1) `eq3` Vec3 1 (-4) 88 @?= True,
				testProperty "the closest point on a random plane to the origin matches its normal and distance" $
					-- Get a random plane.
					\(pabcRaw :: Vec3 Double) ->
					\(pd      :: Double     ) ->
					let pabc = v3normalize pabcRaw & \v -> if' (not $ (v^.r3) `near` 1.0) (Vec3 1.0 0.0 0.0) $ v in
					let plane = normalPlane3 pabc pd in

					let p = pointToPlane zv3 plane in
					p `near3` ((plane^.dp3) `sv3` (plane^.abcp3))
			],

		testGroup "line3PlaneIntersection" $
			[
				testProperty "a random plane's normal intersects the plane at the closest point on the plane to the origin" $
					-- Get a random plane.
					\(pabcRaw :: Vec3 Double) ->
					\(pd      :: Double     ) ->
					let pabc = v3normalize pabcRaw & \v -> if' (not $ (v^.r3) `near` 1.0) (Vec3 1.0 0.0 0.0) $ v in
					let plane = normalPlane3 pabc pd in

					let p = pointToPlane zv3 plane in
					(pure near3 <*> line3PlaneIntersection (line3Axes zv3 (plane^.abcp3)) plane <*> pure p) == Just True
			],

		testGroup "plane3ReflectPoint" $
			[
				testCase "simple sample test" $
					(planeX1 `plane3ReflectPoint` Vec3 7 (-4) 88) `eq3` Vec3 (-5) (-4) 88 @?= True
			],

		testGroup "normalizePlane3" $
			[
				testCase "simple sample test" $
					(Vec3 1 (-4) 88 `normalizePlane3` Vec3 1 0 0) `eqPlane3` planeX1 @?= True
			],

		testGroup "simple line3 tests" $
			[
				testCase "line3NormalizeDisplacement simple test" $
					(line3NormalizeDisplacement $ line3Points (Vec3 0 1 7) (Vec3 1 0 7)) `nearLine3` line3Points (Vec3 0.5 0.5 7) (Vec3 1.5 (-0.5) (7 :: Double)) @?= True,
				testCase "line3DistanceFromOrigin simple test" $
					(line3DistanceFromOrigin $ line3Points (Vec3 0 1 0) (Vec3 1 0 0)) `near` (1 / sqrt 2  :: Double) @?= True,

				testCase "line3Lerp simple test" $
					line3Lerp (line3Points (Vec3 1 2 3) (Vec3 2 4 4)) 3 `near3` Vec3 4 8 (6 :: Double) @?= True,

				testCase "line3CoordAtDistancePlane3 simple test" $
					((line3CoordAtDistancePlane3 planeX1 (line3Points (Vec3 9 824 4) (Vec3 7 (-1) 8)) 2 <&> equivalentSmall) <*> pure 3) @?= Just True,
				testCase "line3CoordAtDistancePlane3 parallel simple test" $
					((line3CoordAtDistancePlane3 planeX1 (line3Points (Vec3 9 824 4) (Vec3 9 (-1) 8)) 2 <&> equivalentSmall) <*> pure 3) @?= Nothing,

				testCase "line3PointCoord simple test" $
					line3PointCoord (line3Points (Vec3 1 1 2) (Vec3 1 8 2)) (Vec3 42 15 78) `equivalentSmall` ( 2 :: Double) @?= True,
				testCase "line3PointCoord negated simple test" $
					line3PointCoord (line3Points (Vec3 1 8 2) (Vec3 1 1 2)) (Vec3 42 15 78) `equivalentSmall` (-1 :: Double) @?= True,

				testCase "line3PointDistance 0 simple test" $
					line3PointDistance (line3Points (Vec3 1 1 2) (Vec3 1 8 2)) (Vec3 1 9   2) `equivalentSmall` (0 :: Double) @?= True,
				testCase "line3PointDistance non-0 simple test" $
					line3PointDistance (line3Points (Vec3 1 1 2) (Vec3 1 8 2)) (Vec3 1 123 3) `equivalentSmall` (1 :: Double) @?= True,

				testCase "line3DistanceCoordFromPoint simple test 0" $
					line3DistanceCoordFromPoint (line3Points (Vec3 1 1 2) (Vec3 1 8 2)) (Vec3 1 9 2) 0 `equivalentSmall` (0 :: Double) @?= True,
				testCase "line3DistanceCoordFromPoint simple test 1" $
					line3DistanceCoordFromPoint (line3Points (Vec3 1 1 2) (Vec3 1 8 2)) (Vec3 1 9 2) 1 `equivalentSmall` ((1/7) :: Double) @?= True,
				testCase "line3DistanceCoordFromPoint simple test 1 2" $
					line3DistanceCoordFromPoint (line3Points (Vec3 1 1 2) (Vec3 1 8 2)) (Vec3 2 9 2) 1 `equivalentSmall` (0 :: Double) @?= True,
				testCase "line3DistanceCoordFromPoint simple test 2" $
					line3DistanceCoordFromPoint (line3Points (Vec3 1 1 2) (Vec3 1 8 2)) (Vec3 2 9 2) 2 `equivalentSmall` ((sqrt 3 / 7) :: Double) @?= True
			],

		testGroup "line3 plane3 tests" $
			[
				testCase "plane3LineSegmentDistance behind simple test" $
					plane3LineSegmentDistance planeX1 (line3Points (Vec3 0 80 90) (Vec3 (-320) (-88) 777)) `near` (-1) @?= True,
				testCase "plane3LineSegmentDistance intersects simple test" $
					plane3LineSegmentDistance planeX1 (line3Points (Vec3 0 80 90) (Vec3 ( 320) (-88) 777)) `near` ( 0) @?= True,
				testCase "line3AxisReflectPlane3 simple test" $
					line3AxisReflectPlane3 (line3Points (Vec3 1 2 8) (Vec3 2 (-3) 7)) (planeX1^.abcp3) `nearLine3` line3Points (Vec3 1 2 8) (Vec3 0 (-3) 7) @?= True
			],

		testGroup "line3 line3 tests" $
			[
				testCase "sample lines are distance 1" $
					line3Line3Distance sampleLine0 sampleLine1 `equivalentSmall` 1 @?= True,
				testCase "sample lines are distance 1 with second z-negated" $
					line3Line3Distance sampleLine0 (sampleLine1 & ol3.z3 %~ negate) `equivalentSmall` 1 @?= True
			],

		testGroup "plane3 plane3 tests" $
			[
				testCase "planeX1 planeZ2 intersects at" $
					let allowOtherDirection = False in
					((pure nearLine3 <*> plane3Plane3 planeX1 planeZ2 <*> pure (line3Points (Vec3 1 0 2) (Vec3 1 (-1) 2))) == Just True) ||
					((pure nearLine3 <*> plane3Plane3 planeX1 planeZ2 <*> pure (line3Points (Vec3 1 0 2) (Vec3 1 1    2))) == Just True && allowOtherDirection) @?= True,
				testProperty "planes intersect with points on both planes" $
					-- Get 2 random planes, pa and pb.
					\(paabcRaw :: Vec3 Double) ->
					\(pbabcRaw :: Vec3 Double) ->
					\(pad      :: Double     ) ->
					\(pbd      :: Double     ) ->
					\(lx       :: Double     ) ->
					let paabc = v3normalize paabcRaw & \v -> if' (not $ (v^.r3) `near` 1.0) (Vec3 1.0 0.0 0.0) $ v in
					let pbabc = v3normalize pbabcRaw & \v -> if' (not $ (v^.r3) `near` 1.0) (Vec3 1.0 0.0 0.0) $ v in
					let pa = normalPlane3 paabc pad in
					let pb = normalPlane3 pbabc pbd in

					-- Get their intersection.
					let ml = plane3Plane3 pa pb in
					case ml of
						Nothing ->
							-- They don't intersect; make sure the cross
							-- product of the 2 normals are smallish.
							let cross = (pa^.abcp3) `vx3` (pb^.abcp3) in
							cross^.r3 <= smallishNum
						Just l ->
							-- Get a random point on the intersection line.
							let p = line3Lerp l lx in

							-- Make sure it's on pa.
							plane3PointDistance pa p `near` 0 &&
							-- Make sure it's on pb.
							plane3PointDistance pb p `near` 0
			]
	]
