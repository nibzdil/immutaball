{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Main.hs: Immutaball.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Ball.Main where

import qualified Immutaball.Ball.CLI

main :: IO ()
main = Immutaball.Ball.CLI.main
