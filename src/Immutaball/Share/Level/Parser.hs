{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Parser.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Level.Parser
	(
		parseLevelFile,
		parseLevelFile',
		levelParser
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow

import qualified Data.ByteString.Lazy as BL
import Text.Parsec

import Immutaball.Share.Level.Base

parseLevelFile :: String -> BL.ByteString -> Either String LevelIB
parseLevelFile inputName inputContents = show +++ id $ parseLevelFile' inputName inputContents

parseLevelFile' :: SourceName -> BL.ByteString -> Either ParseError LevelIB
parseLevelFile' inputName inputContents = parse levelParser inputName inputContents

levelParser :: Parsec BL.ByteString () LevelIB
levelParser = error "TODO: unimplemented."
