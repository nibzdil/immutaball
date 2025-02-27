{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, ScopedTypeVariables #-}

module Test.Immutaball.Share.Math.Test
	(
		main,
		testsMain,
		tests,

		simpleConstant,
		right3,
		forward3,
		up3,
		right2,
		up2,
		eq3Duplicate,
		zint,
		zdouble,
		circle,
		eqEachEach
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
import Test.Immutaball.Share.Math.Orphans ()

-- TODO: remove debugging imports when done.
import Debug.Trace as D------------------------------------------------------ TODO
import Text.Printf

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain tests

simpleConstant :: Integer
simpleConstant = 3

right3 :: Vec3 Double
right3 = Vec3 1.0 0.0 0.0

forward3 :: Vec3 Double
forward3 = Vec3 0.0 1.0 0.0

up3 :: Vec3 Double
up3 = Vec3 0.0 0.0 1.0

right2 :: Vec2 Double
right2 = Vec2 1.0 0.0

up2 :: Vec2 Double
up2 = Vec2 0.0 1.0

eq3Duplicate :: Vec3 Double -> Vec3 Double -> Bool
eq3Duplicate a b = abs ((b - a)^.r3) <= smalld

-- | Helps resolve default type to Integer warnings when using ‘0’.
zint :: Integer
zint = 0

zdouble :: Double
zdouble = 0.0

-- | Similar but to specialize to 'float' so that equivalence checks'
-- thresholds aren't too tight.
circle :: Float
circle = tau

-- | Given a list of values possibly known to already be unique, return a bool
-- that is True if the equality comparison on each possible pairing of values
-- is as expected.
--
-- If e.g. the values are randomly generated, they might not be unique, so only
-- check each value with each other in this case.
eqEachEach :: Bool -> (a -> a -> Bool) -> [a] -> Bool
eqEachEach guaranteedUnique eq vals =
	and $
		[ r
		| ivals <- return $ zip [zint..] vals
		, (ai, av) <- ivals
		, (bi, bv) <- ivals
		, guaranteedUnique || ai == bi
		, r <- return $ (ai == bi) == (av `eq` bv)
		]

tests :: TestTree
tests = testGroup "Immutaball.Share.Math" $
	[
		testCase "simpleConstant == 3" $
			simpleConstant @?= 3,

		testGroup "meta tests" $
			[
				testCase "right3 == right3" $
					right3 `eq3Duplicate` right3 @?= True,
				testCase "right3 /= forward3" $
					right3 `eq3Duplicate` forward3 @?= False,
				testCase "each axis unit vector equality checks correctly with each" $
					and [r | axes <- return $ zip [zint..] [right3, forward3, up3], (ai, av) <- axes, (bi, bv) <- axes, r <- return $ (ai == bi) == (av `eq3Duplicate` bv)] @?= True,
				testCase "each axis unit vector equality checks correctly with each" $
					eqEachEach True eq3Duplicate [right3, forward3, up3] @?= True
			],

		testGroup "eq3 tests" $
			[
				testCase "right3 == right3" $
					right3 `eq3` right3 @?= True,
				testCase "right3 /= forward3" $
					right3 `eq3` forward3 @?= False,
				testCase "each axis unit vector equality checks correctly with each" $
					and [r | axes <- return $ zip [zint..] [right3, forward3, up3], (ai, av) <- axes, (bi, bv) <- axes, r <- return $ (ai == bi) == (av `eq3` bv)] @?= True,
				testCase "each axis unit vector equality checks correctly with each" $
					eqEachEach True eq3 [right3, forward3, up3] @?= True
			],

		testGroup "eqEachEach for vecs and mats" $
			[
				testGroup "randomly generated" $
					[
						testProperty "eqEachEach random vec2" $
							\(vals :: [Vec2 Double]) -> eqEachEach False eq2 vals,
						testProperty "eqEachEach random vec3" $
							\(vals :: [Vec3 Double]) -> eqEachEach False eq3 vals,
						testProperty "eqEachEach random vec4" $
							\(vals :: [Vec4 Double]) -> eqEachEach False eq4 vals,
						testProperty "eqEachEach random mat3" $
							\(vals :: [Mat3 Double]) -> eqEachEach False eqm3 vals,
						testProperty "eqEachEach random mat4" $
							\(vals :: [Mat4 Double]) -> eqEachEach False eqm4 vals
					],

				testGroup "fixed" $
					[
						testCase "eqEachEach fixed vec2" $
							eqEachEach True eq2 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True,
						testCase "eqEachEach fixed vec3" $
							eqEachEach True eq3 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True,
						testCase "eqEachEach fixed vec4" $
							eqEachEach True eq4 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True,
						testCase "eqEachEach fixed mat3" $
							eqEachEach True eqm3 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True,
						testCase "eqEachEach fixed mat4" $
							eqEachEach True eqm4 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True
					],

				testGroup "randomly generated, near" $
					[
						testProperty "eqEachEach random vec2 near" $
							\(vals :: [Vec2 Double]) -> eqEachEach False near2 vals,
						testProperty "eqEachEach random vec3 near" $
							\(vals :: [Vec3 Double]) -> eqEachEach False near3 vals,
						testProperty "eqEachEach random vec4 near" $
							\(vals :: [Vec4 Double]) -> eqEachEach False near4 vals,
						testProperty "eqEachEach random mat3 near" $
							\(vals :: [Mat3 Double]) -> eqEachEach False nearm3 vals,
						testProperty "eqEachEach random mat4 near" $
							\(vals :: [Mat4 Double]) -> eqEachEach False nearm4 vals
					]
			],

		testGroup "3D pointing orientation utils (aiming) - using floats so that equality checks' precisions aren't too tight" $
			[
				testCase "aim right 1/8th circle" $
					--D.trace (printf "DEBUG0: left is %s and right is %s." (show $ aimHoriz3DSimple (circle/8) (Vec3 0.0 1.0 0.0)) (show $ (Vec3 (sqrt 0.5) (sqrt 0.5) 0.0))) $
					aimHoriz3DSimple (circle/8) (Vec3 0.0 1.0 0.0) `eq3` (Vec3 (sqrt 0.5) (sqrt 0.5) 0.0) @?= True,
				testCase "aim right 30 degrees" $
					aimHoriz3DSimple (circle/12) (Vec3 0.0 1.0 0.0) `eq3` (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) 0.0) @?= True,

				testCase "look 45 deg up from 30 deg right" $
					-- length of Vec2 x y should be equal to z here, so that it's a right angle.
					-- prenormalized:
					-- 	  (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) (sqrt $ x^2 + y^2))
					-- 	= (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) (sqrt $ 1/4 + 3/4))
					-- 	= (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) 1.0)
					-- prenormalized^.r: sqrt (1/4 + 3/4 + 1^2) = sqrt (1 + 1) = sqrt 2
					-- normalized:
					-- 	  (Vec3 (1.0      / 2.0) (sqrt 3.0 / 2.0) 1.0             ) / sqrt 2
					-- 	= (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0))
					--D.trace (printf "DEBUG0: left is %s and right is %s." (show $ aimVert3DSimple Nothing (circle/8) (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) 0.0)) (show $ Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0  :: Float))) $
					aimVert3DSimple Nothing (circle/8) (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) 0.0) `near3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)) @?= True,

				testGroup "tilt3y" $
					[
						testCase "tilt3y on 0,1,0 gives same result (look 45 deg up from 30 deg right)" $
							tilt3ySimple (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)) `mv3` forward3 @?= (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)),
						testCase "tilt3y 0,1,0 on (look 45 deg up from 30 deg right) gives (look 45 deg up from 30 deg right) (no change)" $
							tilt3ySimple forward3 `mv3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)) @?= (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)),
						testCase "tilt3y 1,0,0 on (look 45 deg up from 30 deg right) gives (look 45 deg up from 120 deg right) (just xy %~ *i**3 of last test's expected)" $
							(tilt3ySimple right3 `mv3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0))) `near3` (Vec3 (sqrt 1.5 / 2.0) (-sqrt 2.0 / 4.0) (sqrt 2.0 / 2.0)) @?= True,
						testProperty "tilt3y == aimHoriz <> aimVert by near" $
							let v3normalize' v = v3normalize v `v3orWith` right3 in
							-- Apply a random camera target tilt to a random position.  (Camera's at origin.)
							\(relCamTarget_ :: Vec3 Double) (randomPos :: Vec3 Double) ->
							let relCamTarget = v3normalize' relCamTarget_ in
							let byTilt3y = tilt3ySimple relCamTarget `mv3` randomPos in
							let relCamTargetxy = Vec2 (relCamTarget^.x3) (relCamTarget^.y3) in
							let byAims = aimVert3DSimple Nothing ((Vec2 (relCamTargetxy^.r2) (relCamTarget^.z3))^.t2) . aimHoriz3DSimple (up2^.t2 - relCamTargetxy^.t2) $ randomPos in
							byTilt3y `near3` byAims,
						testCase "tilt3y 0,0,1 on (look 45 deg up from 30 deg right) gives (look 45 deg up from 30 deg right then yz %~ *i)" $
							(tilt3ySimple up3 `mv3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0))) `near3` (Vec3 (sqrt 2.0 / 4.0) (-sqrt 2.0 / 2.0) (sqrt 1.5 / 2.0)) @?= True,
						testCase "tilt3y 0,0,-1 on (look 45 deg up from 30 deg right) gives (look 45 deg up from 30 deg right then yz %~ *i**3)" $
							--D.trace (printf "DEBUG0: left is %s and right is %s." (show $ updateit (tilt3ySimple (-up3) `mv3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)))) (show $ (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (-sqrt 2.0 / 2.0)))) $
							(tilt3ySimple (-up3) `mv3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0))) `near3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 2.0 / 2.0) (-sqrt 1.5 / 2.0)) @?= True
					],

				testGroup "tilt3z" $
					[
						testCase "tilt3z on 0,0,1 gives same result (look 45 deg up from 30 deg right)" $
							tilt3zSimple (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)) `mv3` up3 @?= (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)),
						testCase "tilt3z 0,0,1 on (look 45 deg up from 30 deg right) gives (look 45 deg up from 30 deg right) (no change)" $
							tilt3zSimple up3 `mv3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)) @?= (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)),
						-- Roll right a right angle.
						testCase "tilt3z 1,0,0 on (look 45 deg up from 30 deg right) gives (just xz %~ *i**3 of last test's expected)" $
							(tilt3zSimple right3 `mv3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0))) `near3` (Vec3 (sqrt 2.0 / 2.0) (sqrt 1.5 / 2.0) (-sqrt 2.0 / 4.0)) @?= True
						-- TODO:
						{-
						testProperty "tilt3z == unaimHoriz (yaw) <> aimVert (pitch) <> aimHoriz (yaw) by near" $
							let v3normalize' v = v3normalize v `v3orWith` right3 in
							-- Apply a random up vector to a random position.
							\(relUp_ :: Vec3 Double) (randomPos :: Vec3 Double) ->
							let relUp = v3normalize' relUp_ in
							-- Calculate the radians to aim right by with aimHoriz3DSimple.
							t

aimHoriz3DSimple :: (Num a, Floating a) => a -> Vec3 a -> Vec3 a
							let byTilt3z = tilt3zSimple relUp `mv3` randomPos in
							let relUpxz = Vec2 (relUp^.x3) (relUp^.z3) in
							let relUpxy = Vec2 (relUp^.x3) (relUp^.y3) in
							--let byPlaneRots = (rotateyzSimple (up2^.t2 - relUpyz^.t2) <> rotatexzSimple (up2^.t2 - relUpxz^.t2)) `mv3` randomPos in
							let byPlaneRots = (rotateyzSimple ((Vec2 (relUp^.z3) (relUpxy^.r2))^.t2) <> rotatexzSimple (up2^.t2 - relUpxz^.t2)) `mv3` randomPos in
							-- TODO 0,+-1,0 edge case ?
							let debug=map (:[]).(take 10.show).(round.(1000*))$1000*(relUp^.r3)+100*(relUp^.x3)+10*(relUp^.y3)+(relUp^.z3) in
							let debug2=map (:[]).(take 10.show).(round.(1000*))$1000*(randomPos^.r3)+100*(randomPos^.x3)+10*(randomPos^.y3)+(randomPos^.z3) in
							let debug3=concat$(take 5$zipWith (++) debug debug2) ++ ["."] ++ (reverse.take 5$zipWith (++) debug debug2) in
							let verbose = True in
							let if' a b c = if a then b else c in
							D.trace (printf "DEBUG0 %s(%21s)%s: byTilt3z:    %s" (show $ byTilt3z `near3` byPlaneRots) (debug3) (if' verbose (show (relUp_, randomPos)) "") (show $ byTilt3z)) .
							D.trace (printf "DEBUG1 %s(%21s)%s: byPlaneRots: %s" (show $ byTilt3z `near3` byPlaneRots) (debug3) (if' verbose (show (relUp_, randomPos)) "") (show $ byPlaneRots)) $
							not (relUp_ `near3` zv3) ==> -- For our ^.t2 handling, just skip the edge case of zero vectors.
							byTilt3z `near3` byPlaneRots
						-}
					]
			]
	]
