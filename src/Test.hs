#!/usr/bin/env -S bash -c 'eval "$(cat "$0" | head -n+3 | tail -n+3 | tail -c+3)" "$0" "${@@Q}"'
{-
#!/usr/bin/env -S runhaskell '-i.:./dependency-substitutions/wires-nofallback-meta' '-package containers' '-package bytestring' '-package transformers' '-package pipes' '-package lens' '-package parallel' '-package stm' '-package async' '-package wires' '-package mtl' '-package filepath' '-package directory' '-package prettyprinter' '-package parsec' '-package unbounded-delays' '-package time' '-package sdl2' '-package gl' '-package array' '-package sdl2-ttf' '-package libvorbis' '-package JuicyPixels' '-package curl' '-package i18n' '-package text' '-package tasty' '-package tasty-hunit' '-package tasty-quickcheck' '-package HUnit' '-package QuickCheck'
#!/usr/bin/env -S runhaskell '-i.:./dependency-substitutions/wires-fallback-meta' '-package containers' '-package bytestring' '-package transformers' '-package pipes' '-package lens' '-package parallel' '-package stm' '-package async' '-package mtl' '-package filepath' '-package directory' '-package prettyprinter' '-package parsec' '-package unbounded-delays' '-package time' '-package sdl2' '-package gl' '-package array' '-package sdl2-ttf' '-package libvorbis' '-package JuicyPixels' '-package curl' '-package i18n' '-package text' '-package tasty' '-package tasty-hunit' '-package tasty-quickcheck' '-package HUnit' '-package QuickCheck'
-}
{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}

module Main where

import qualified Test.Immutaball.Test

main :: IO ()
main = Test.Immutaball.Test.main
