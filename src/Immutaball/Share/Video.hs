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
		sdlCreateImmutaballShader,

		-- * Shader: low level
		ImmutaballShaderHandle(..), ibshVertexShader, ibshFragmentShader,
			ibshProgram, ibshPipeline,
		initImmutaballShader,
		freeImmutaballShader,

		-- * Utils
		checkGLErrorsIB,
		glErrType
	) where

import Prelude ()
import Immutaball.Prelude

import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Word

import Control.Concurrent.STM.TMVar
import Control.Lens
import Graphics.GL.Compatibility45
--import Graphics.GL.Core45
import Graphics.GL.Types

import Immutaball.Share.Math
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.SDLManager

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

-- | Directly initialize the shader, attaching the lifetime to the caller; does
-- not asynchronously attach the lifetime to the SDL manager thread, which can be done with
-- 'sdlCreateImmutaballShader'.
withImmutaballShader :: SDLManagerHandle -> (ImmutaballShaderHandle -> ImmutaballIOF me) -> ImmutaballIOF me
withImmutaballShader sdlMgr withShader = do
	shader <- initImmutaballShader sdlMgr
	me <- withShader shader
	freeImmutaballShader sdlMgr shader
	return me

-- | Use the SDL Manager thread to manage the immutaball shader resource.
--
-- When the SDL Manager thread exits, it will deallocate the resource.
--
-- The TMVar is set with the resource once the initializer is done.
--
-- The SDLManager initializes this resource concurrently, so it does not block
-- the SDLManager thread.
sdlCreateImmutaballShader :: SDLManagerHandle -> ImmutaballIOF (TMVar ImmutaballShaderHandle)
sdlCreateImmutaballShader sdlMgr =
	JoinIBIOF .
	Atomically (newEmptyTMVar) $ \to_ ->
	attachLifetime sdlMgr (initImmutaballShader sdlMgr) (freeImmutaballShader sdlMgr) to_ to_

-- * Shader: low level

-- ImmutaballShaderHandle moved to avoid Template Haskell errors.

-- | Allocate an immutaball shader.
initImmutaballShader :: SDLManagerHandle -> ImmutaballIOF ImmutaballShaderHandle
initImmutaballShader _sdlMgr =
	--_
	error "TODO: unimplemented."

-- | Deallocate an immutaball shader.
freeImmutaballShader :: SDLManagerHandle -> ImmutaballShaderHandle -> ImmutaballIOF ()
freeImmutaballShader sdlMgr ibsh = do
	let sdlGL1' = sdlGL1 sdlMgr
	sdlGL1' $ do
		GLDeleteProgramPipelines [(ibsh^.ibshPipeline)] ()
		GLDeleteProgram (ibsh^.ibshProgram) ()
		GLDeleteShader (ibsh^.ibshFragmentShader) ()
		GLDeleteShader (ibsh^.ibshVertexShader) ()

-- * Utils

checkGLErrorsIB :: ImmutaballIOF ()
checkGLErrorsIB = do
	error_ <- BasicIBIOF . GLIO $ GLGetError id
	case error_ of
		GL_NO_ERROR -> return ()
		err -> do
			() <- BasicIBIOF $ PutStrLn ("Error: an OpenGL error occurred (" ++ show err ++ "): " ++ glErrType err) ()
			() <- BasicIBIOF $ ExitFailureBasicIOF
			return ()

glErrType :: GLenum -> String
glErrType GL_NO_ERROR                      = "GL_NO_ERROR"
glErrType GL_INVALID_ENUM                  = "GL_INVALID_ENUM"
glErrType GL_INVALID_VALUE                 = "GL_INVALID_VALUE"
glErrType GL_INVALID_OPERATION             = "GL_INVALID_OPERATION"
glErrType GL_INVALID_FRAMEBUFFER_OPERATION = "GL_INVALID_FRAMEBUFFER_OPERATION"
glErrType GL_OUT_OF_MEMORY                 = "GL_OUT_OF_MEMORY"
glErrType GL_STACK_OVERFLOW                = "GL_STACK_OVERFLOW"
glErrType GL_STACK_UNDERFLOW               = "GL_STACK_UNDERLOW"
glErrType _                                = "unknown error type"
