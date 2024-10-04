{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Test.Immutaball.Share.Wire.Test
	(
		main,
		testsMain,
		tests,

		myWire,
		dt,
		accumulateThings,
		derivativeThings,
		stepThrice,
		stepTwice,
		stepOnce
	) where

import Control.Arrow
import Data.Functor.Identity

import Test.HUnit
--import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit hiding ((@?=), assertBool)
--import Test.Tasty.QuickCheck

import Immutaball.Share.Wire

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain tests

myWire :: Wire Identity () Integer
myWire = proc () -> returnA -< 3

dt :: Wire Identity () Integer
dt = proc () -> returnA -< 100

accumulateThings :: Wire Identity () Integer
accumulateThings = proc () -> do
	dt_ <- dt -< ()
	thing <- myWire -< ()
	integrate 0 -< thing * dt_

derivativeThings :: Wire Identity () Integer
derivativeThings = proc () -> do
	dt_ <- dt -< ()
	thing <- myWire -< ()
	derive -< thing * dt_

stepThrice :: Wire Identity () Integer -> Integer
stepThrice w0 =
	let
		(Identity (_y0,  w1)) = stepWire w0 ()
		(Identity (_y1,  w2)) = stepWire w1 ()
		(Identity ( y2, _w3)) = stepWire w2 ()

		result = y2
	in
		result

stepTwice :: Wire Identity () Integer -> Integer
stepTwice w0 =
	let
		(Identity (_y0,  w1)) = stepWire w0 ()
		(Identity ( y1, _w2)) = stepWire w1 ()

		result = y1
	in
		result

stepOnce :: Wire Identity () Integer -> Integer
stepOnce w0 =
	let
		(Identity (y0, _w1)) = stepWire w0 ()

		result = y0
	in
		result

tests :: TestTree
tests = testGroup "Immutaball.Share.Wire" $
	[
		testCase "integrate twice" $
			stepThrice accumulateThings @?= 600,
		testCase "derive twice" $
			stepThrice derivativeThings @?= 0,
		testCase "derive once" $
			stepTwice derivativeThings @?= 300,
		testCase "derive never" $
			stepOnce derivativeThings @?= 0
	]
