{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Video
	(
		reverseRowsImage,

		-- * Shader: high level
		withImmutaballShader,

		-- * Shader: low level
		ImmutaballShaderHandle(..), _ibshVertexShader, _ibshFragmentShader,
			_ibshProgram, _ibshPipeline,
		initImmutaballShader,
		freeImmutaballShader

	) where

import Prelude ()
import Immutaball.Prelude

import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Word

import Control.Lens
import Graphics.GL.Types

import Immutaball.Share.Math
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO

-- TODO: learn the new bytestring builders and probably use them.
reverseRowsImage :: (WidthHeightI, BS.ByteString) -> BS.ByteString
reverseRowsImage ((w, _h), image) = glImage
	where
		glImage = BS.pack glBytes
		bytes :: [Word8]
		bytes = BS.unpack image
		glBytes :: [Word8]
		glBytes = concat glRows
		glRows :: [[Word8]]
		glRows = reverse rows
		rows :: [[Word8]]
		rows = withRemaining bytes
		withRemaining :: [Word8] -> [[Word8]]
		withRemaining [] = []
		withRemaining xs = genericTake row xs : withRemaining (genericDrop row xs)
		row = 4 * w

-- Moved to avoid Template Haskell errors.

data ImmutaballShaderHandle = ImmutaballShaderHandle {
	_ibshVertexShader   :: GLuint,
	_ibshFragmentShader :: GLuint,
	_ibshProgram        :: GLuint,
	_ibshPipeline       :: GLuint
}
makeLenses ''ImmutaballShaderHandle

-- * Shader: high level

withImmutaballShader :: (ImmutaballShaderHandle -> ImmutaballIOF me) -> ImmutaballIOF me
withImmutaballShader withShader = do
	shader <- BasicIBIOF . GLIO $ initImmutaballShader
	me <- withShader shader
	BasicIBIOF . GLIO $ freeImmutaballShader shader
	return me

-- * Shader: low level

-- ImmutaballShaderHandle moved to avoid Template Haskell errors.

initImmutaballShader :: GLIOF ImmutaballShaderHandle
initImmutaballShader =
	_

freeImmutaballShader :: ImmutaballShaderHandle -> GLIOF ()
freeImmutaballShader =
	_
