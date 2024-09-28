#!/usr/bin/env -S runhaskell '-package containers' '-package bytestring' '-package transformers' '-package pipes' '-package wires' '-package tasty' '-package tasty-hunit' '-package tasty-quickcheck' '-package HUnit' '-package QuickCheck'
{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}

module Main where

import qualified Test.Immutaball.Test

main :: IO ()
main = Test.Immutaball.Test.main
