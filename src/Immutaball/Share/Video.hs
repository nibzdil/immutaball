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
		rawInitializeImmutaballShaderContinue,

		-- * Utils
		checkGLErrorsIB,
		glErrType
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Monad
import Data.Bits
import Data.List
import Data.Word

import Control.Concurrent.STM.TMVar
import Control.Lens
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BL
import Graphics.GL.Compatibility45
--import Graphics.GL.Core45
import Graphics.GL.Types

import Immutaball.Share.Math
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.SDLManager
import Immutaball.Share.Video.Shaders

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
--
-- (The SDLManager runs the initializer concurrently.  If it didn't, it would
-- deadlock because we also dispatch to the SDL Manager to run commands.)
initImmutaballShader :: SDLManagerHandle -> ImmutaballIOF ImmutaballShaderHandle
initImmutaballShader sdlMgr =
	JoinIBIOF .
	Atomically (newEmptyTMVar) $ \mibsh ->
	(>>= \() -> Atomically (readTMVar mibsh) id) . sdlIBIO sdlMgr $ do
		-- We're the only shader manager in this application, so we don't need
		-- to worry about exclusion here currently.  But regardless, exclusion
		-- is already present because sdlMgr blocks if we, the caller, don't
		-- fork a thread for a general IBIO dispatch.
		vertexShader_   <- BasicIBIOF . GLIO $ GLCreateShader GL_VERTEX_SHADER id
		fragmentShader_ <- BasicIBIOF . GLIO $ GLCreateShader GL_FRAGMENT_SHADER id
		program         <- BasicIBIOF . GLIO $ GLCreateProgram id
		mpipeline       <- BasicIBIOF . GLIO $ GLGenProgramPipelines 1 id
		let pipeline = unSingleton mpipeline
		let ibsh = ImmutaballShaderHandle {
			_ibshVertexShader   = vertexShader_,
			_ibshFragmentShader = fragmentShader_,
			_ibshProgram        = program,
			_ibshPipeline       = pipeline
		}
		-- Don't write it until we finish initialization, to keep things synchronized.
		rawInitializeImmutaballShaderContinue ibsh
		Atomically (writeTMVar mibsh ibsh) id
	where
		unSingleton [me] = me
		unSingleton _    = error "Internal error: initImmutaballShader expected a single result from GLGenProgramPipelines."

-- | Deallocate an immutaball shader.
freeImmutaballShader :: SDLManagerHandle -> ImmutaballShaderHandle -> ImmutaballIOF ()
freeImmutaballShader sdlMgr ibsh = do
	let sdlGL1' = sdlGL1 sdlMgr
	sdlGL1' $ do
		GLDeleteProgramPipelines [(ibsh^.ibshPipeline)] ()
		GLDeleteProgram (ibsh^.ibshProgram) ()
		GLDeleteShader (ibsh^.ibshFragmentShader) ()
		GLDeleteShader (ibsh^.ibshVertexShader) ()

-- | (Note: we are already in the SDL Manager thread.)
rawInitializeImmutaballShaderContinue :: ImmutaballShaderHandle -> ImmutaballIOF ()
rawInitializeImmutaballShaderContinue ibsh = do
	BasicIBIOF . GLIO $ GLShaderSource (ibsh^.ibshVertexShader) [vertexShader] ()
	checkGLErrorsIB
	BasicIBIOF . GLIO $ GLShaderSource (ibsh^.ibshFragmentShader) [fragmentShader] ()
	checkGLErrorsIB

	BasicIBIOF . GLIO $ GLCompileShader (ibsh^.ibshVertexShader) ()
	checkGLErrorsIB
	successV <- ((/= 0) <$>) . BasicIBIOF . GLIO $ GLGetShaderiv (ibsh^.ibshVertexShader) GL_COMPILE_STATUS id
	checkGLErrorsIB
	when (not successV) $ do
		compileError <- BasicIBIOF . GLIO $ GLGetShaderInfoLog (ibsh^.ibshVertexShader) id
		() <- BasicIBIOF $ PutStrLn ("Error: the vertex shader failed to compile!  OpenGL error: " ++ compileError) ()
		() <- BasicIBIOF $ ExitFailureBasicIOF
		return ()

	BasicIBIOF . GLIO $ GLCompileShader (ibsh^.ibshFragmentShader) ()
	checkGLErrorsIB
	successF <- ((/= 0) <$>) . BasicIBIOF . GLIO $ GLGetShaderiv (ibsh^.ibshFragmentShader) GL_COMPILE_STATUS id
	checkGLErrorsIB
	when (not successF) $ do
		compileError <- BasicIBIOF . GLIO $ GLGetShaderInfoLog (ibsh^.ibshFragmentShader) id
		() <- BasicIBIOF $ PutStrLn ("Error: the fragment shader failed to compile!  OpenGL error: " ++ compileError) ()
		() <- BasicIBIOF $ ExitFailureBasicIOF
		return ()

	BasicIBIOF . GLIO $ GLAttachShader (ibsh^.ibshProgram) (ibsh^.ibshVertexShader) ()
	checkGLErrorsIB
	BasicIBIOF . GLIO $ GLAttachShader (ibsh^.ibshProgram) (ibsh^.ibshFragmentShader) ()
	checkGLErrorsIB
	BasicIBIOF . GLIO $ GLLinkProgram (ibsh^.ibshProgram) ()
	checkGLErrorsIB

	successL <- ((/= 0) <$>) . BasicIBIOF . GLIO $ GLGetProgramiv (ibsh^.ibshProgram) GL_LINK_STATUS id
	checkGLErrorsIB
	when (not successL) $ do
		linkError <- BasicIBIOF . GLIO $ GLGetProgramInfoLog (ibsh^.ibshProgram) id
		() <- BasicIBIOF $ PutStrLn ("Error: the OpenGL GLSL shaders failed to link!  OpenGL error: " ++ linkError) ()
		() <- BasicIBIOF $ ExitFailureBasicIOF
		return ()

	BasicIBIOF . GLIO $ GLUseProgram (ibsh^.ibshProgram) ()
	checkGLErrorsIB
	BasicIBIOF . GLIO $ GLBindProgramPipeline (ibsh^.ibshPipeline) ()
	checkGLErrorsIB
	let stages = foldr (.|.) 0 $
		[
			GL_VERTEX_SHADER_BIT,
			GL_FRAGMENT_SHADER_BIT
		]
	BasicIBIOF . GLIO $ GLUseProgramStages (ibsh^.ibshPipeline) stages (ibsh^.ibshProgram) ()
	checkGLErrorsIB

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
