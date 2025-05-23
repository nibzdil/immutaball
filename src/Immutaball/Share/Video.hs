{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

module Immutaball.Share.Video
	(
		reverseRowsImageReallySlow,
		reverseRowsImage,

		-- * Shader: high level
		withImmutaballShader,
		sdlCreateImmutaballShader,
		sdlCreateImmutaballShaderWith,

		shaderEnableSceneDataLocation,
		shaderEnableBallDataLocation,
		shaderBallRadiusLocation,
		shaderBallPosLocation,
		shaderBallRotLocation,
		shaderBallNumTrianglesLocation,
		shaderSSBOVertexDataLocation,
		shaderSSBOGeomDataLocation,
		shaderSSBOLumpDataLocation,
		shaderSSBOPathDoublesDataLocation,
		shaderSSBOPathInt32sDataLocation,
		shaderSSBOBodyDataLocation,
		shaderSSBOGcDataLocation,
		shaderSSBOAllGeomPassMvDataLocation,
		shaderSSBOAllGeomPassTexturesDataLocation,
		shaderSSBOAllGeomPassGisDataLocation,
		shaderSSBOGeomPassMvRangesDataLocation,
		shaderSSBOGeomPassTexturesRangesDataLocation,
		shaderSSBOGeomPassGisRangesDataLocation,
		shaderSSBOGeomPassBisDataLocation,
		shaderSSBOTransformationLocation,
		shaderSceneGeomPassIdxLocation,
		shaderSSBOTexcoordsDoubleDataLocation,

		ShaderDoubleType,
		toShaderDoubleType,

		-- * Shader: low level
		ImmutaballShaderHandle(..), ibshVertexShader, ibshFragmentShader,
			ibshProgram, ibshPipeline,
		initImmutaballShader,
		freeImmutaballShader,
		rawInitializeImmutaballShaderContinue,

		-- * MtrlMeta
		MtrlMeta(..),

		-- * Errors
		VideoException(..),
		videoExceptionToException,
		videoExceptionFromException,
		GLErrorVideoException(..),

		-- * Utils
		checkGLErrorsIB,
		glErrType,
		glChecked,
		numToGL_TEXTUREi,
		gpuEncodeArray
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Exception
import Control.Monad
import Data.Bits
import Data.List
import Data.Typeable (cast)
import Data.Word

import Control.Concurrent.STM.TMVar
import Control.Lens
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Graphics.GL.Compatibility45
--import Graphics.GL.Core45
import Graphics.GL.Types

import Immutaball.Share.Math
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.SDLManager
import Immutaball.Share.Utils
import Immutaball.Share.Video.LowLevel
import Immutaball.Share.Video.Shaders

-- | Replaced by a newer 'reverseRowsImage' that performs better.
reverseRowsImageReallySlow :: (WidthHeightI, BS.ByteString) -> BS.ByteString
reverseRowsImageReallySlow ((w, _h), image) = glImage
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

-- | 'sdlCreateImmutaballShader' variant that uses a supplied TMVar for storage.
sdlCreateImmutaballShaderWith :: SDLManagerHandle -> TMVar ImmutaballShaderHandle -> ImmutaballIOF ()
sdlCreateImmutaballShaderWith sdlMgr to_ =
	attachLifetime sdlMgr (initImmutaballShader sdlMgr) (freeImmutaballShader sdlMgr) to_ ()

shaderEnableSceneDataLocation :: GLint
shaderEnableSceneDataLocation = fromIntegral (16 :: Integer)

shaderEnableBallDataLocation :: GLint
shaderEnableBallDataLocation = fromIntegral (34 :: Integer)

shaderBallRadiusLocation :: GLint
shaderBallRadiusLocation = fromIntegral (35 :: Integer)

shaderBallNumTrianglesLocation :: GLint
shaderBallNumTrianglesLocation = fromIntegral (36 :: Integer)

shaderBallPosLocation :: GLint
shaderBallPosLocation = fromIntegral (37 :: Integer)

shaderBallRotLocation :: GLint
shaderBallRotLocation = fromIntegral (38 :: Integer)

shaderSSBOVertexDataLocation :: GLuint
shaderSSBOVertexDataLocation = fromIntegral (17 :: Integer)

shaderSSBOGeomDataLocation :: GLuint
shaderSSBOGeomDataLocation = fromIntegral (18 :: Integer)

shaderSSBOLumpDataLocation :: GLuint
shaderSSBOLumpDataLocation = fromIntegral (19 :: Integer)

shaderSSBOPathDoublesDataLocation :: GLuint
shaderSSBOPathDoublesDataLocation = fromIntegral (20 :: Integer)

shaderSSBOPathInt32sDataLocation :: GLuint
shaderSSBOPathInt32sDataLocation = fromIntegral (21 :: Integer)

shaderSSBOBodyDataLocation :: GLuint
shaderSSBOBodyDataLocation = fromIntegral (22 :: Integer)

shaderSSBOGcDataLocation :: GLuint
shaderSSBOGcDataLocation = fromIntegral (23 :: Integer)

shaderSSBOAllGeomPassMvDataLocation :: GLuint
shaderSSBOAllGeomPassMvDataLocation = fromIntegral (24 :: Integer)

shaderSSBOAllGeomPassTexturesDataLocation :: GLuint
shaderSSBOAllGeomPassTexturesDataLocation = fromIntegral (25 :: Integer)

shaderSSBOAllGeomPassGisDataLocation :: GLuint
shaderSSBOAllGeomPassGisDataLocation = fromIntegral (26 :: Integer)

shaderSSBOGeomPassMvRangesDataLocation :: GLuint
shaderSSBOGeomPassMvRangesDataLocation = fromIntegral (27 :: Integer)

shaderSSBOGeomPassTexturesRangesDataLocation :: GLuint
shaderSSBOGeomPassTexturesRangesDataLocation = fromIntegral (28 :: Integer)

shaderSSBOGeomPassGisRangesDataLocation :: GLuint
shaderSSBOGeomPassGisRangesDataLocation = fromIntegral (29 :: Integer)

shaderSSBOGeomPassBisDataLocation :: GLuint
shaderSSBOGeomPassBisDataLocation = fromIntegral (30 :: Integer)

shaderSSBOTransformationLocation :: GLuint
shaderSSBOTransformationLocation = fromIntegral (31 :: Integer)

shaderSSBOTexcoordsDoubleDataLocation :: GLuint
shaderSSBOTexcoordsDoubleDataLocation = fromIntegral (32 :: Integer)

shaderSceneGeomPassIdxLocation :: GLint
shaderSceneGeomPassIdxLocation = fromIntegral (33 :: Integer)

-- | Whether the shaders use doubles or floats for the SSBOs.
--type ShaderDoubleType = Double
type ShaderDoubleType = Float

-- | Convert Doubles to the Double type the shaders use for SSBOs.
toShaderDoubleType :: Double -> ShaderDoubleType
--toShaderDoubleType = id
toShaderDoubleType = realToFrac

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
		vertexShader_   <- (BasicIBIOF . GLIO $ GLCreateShader GL_VERTEX_SHADER id  ) <* checkGLErrorsIB
		fragmentShader_ <- (BasicIBIOF . GLIO $ GLCreateShader GL_FRAGMENT_SHADER id) <* checkGLErrorsIB
		program         <- (BasicIBIOF . GLIO $ GLCreateProgram id                  ) <* checkGLErrorsIB
		mpipeline       <- (BasicIBIOF . GLIO $ GLGenProgramPipelines 1 id          ) <* checkGLErrorsIB
		let pipeline = unSingleton mpipeline
		let ibsh = ImmutaballShaderHandle {
			_ibshVertexShader   = vertexShader_,
			_ibshFragmentShader = fragmentShader_,
			_ibshProgram        = program,
			_ibshPipeline       = pipeline
		}
		warnIf (vertexShader_ == 0 && fragmentShader_ == 0) $ "Warning: initImmutaballShader: vertex and fragment shaders both have names 0!"
		-- Don't write it until we finish initialization, to keep things synchronized.
		rawInitializeImmutaballShaderContinue ibsh
		Atomically (writeTMVar mibsh ibsh) id
	where
		unSingleton [me] = me
		unSingleton _    = error "Internal error: initImmutaballShader expected a single result from GLGenProgramPipelines."
		warnIf :: Bool -> String -> ImmutaballIOF ()
		warnIf condition msg = do
			() <- if' (not condition) (pure ()) . BasicIBIOF $ PutStrLn msg ()
			return ()

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
	glChecked $ GLShaderSource (ibsh^.ibshVertexShader) [vertexShader] ()
	glChecked $ GLShaderSource (ibsh^.ibshFragmentShader) [fragmentShader] ()

	glChecked $ GLCompileShader (ibsh^.ibshVertexShader) ()
	successV <- ((/= 0) <$>) . glChecked $ GLGetShaderiv (ibsh^.ibshVertexShader) GL_COMPILE_STATUS id
	when (not successV) $ do
		compileError <- BasicIBIOF . GLIO $ GLGetShaderInfoLog (ibsh^.ibshVertexShader) id
		() <- BasicIBIOF $ PutStrLn ("Error: the vertex shader failed to compile!  OpenGL error: " ++ compileError) ()
		checkGLErrorsIB
		() <- BasicIBIOF $ ExitFailureBasicIOF
		return ()

	glChecked $ GLCompileShader (ibsh^.ibshFragmentShader) ()
	successF <- ((/= 0) <$>) . glChecked $ GLGetShaderiv (ibsh^.ibshFragmentShader) GL_COMPILE_STATUS id
	when (not successF) $ do
		compileError <- glChecked $ GLGetShaderInfoLog (ibsh^.ibshFragmentShader) id
		() <- BasicIBIOF $ PutStrLn ("Error: the fragment shader failed to compile!  OpenGL error: " ++ compileError) ()
		checkGLErrorsIB
		() <- BasicIBIOF $ ExitFailureBasicIOF
		return ()

	when setupProgramPipeline $ do
		glChecked $ GLProgramParameteri (ibsh^.ibshProgram) GL_PROGRAM_SEPARABLE GL_TRUE ()

	glChecked $ GLAttachShader (ibsh^.ibshProgram) (ibsh^.ibshVertexShader) ()
	glChecked $ GLAttachShader (ibsh^.ibshProgram) (ibsh^.ibshFragmentShader) ()
	glChecked $ GLLinkProgram (ibsh^.ibshProgram) ()

	successL <- ((/= 0) <$>) . glChecked $ GLGetProgramiv (ibsh^.ibshProgram) GL_LINK_STATUS id
	when (not successL) $ do
		linkError <- BasicIBIOF . GLIO $ GLGetProgramInfoLog (ibsh^.ibshProgram) id
		() <- BasicIBIOF $ PutStrLn ("Error: the OpenGL GLSL shaders failed to link!  OpenGL error: " ++ linkError) ()
		checkGLErrorsIB
		() <- BasicIBIOF $ ExitFailureBasicIOF
		return ()

	when (not useProgramPipeline) $ do
		glChecked $ GLUseProgram (ibsh^.ibshProgram) ()
	when setupProgramPipeline $ do
		let stages = foldr (.|.) 0 $
			[
				GL_VERTEX_SHADER_BIT,
				GL_FRAGMENT_SHADER_BIT
			]
		-- TODO FIXME: when 'useProgramPipeline' is True, this fails with GL_INVALID_OPERATION for me.
		-- We're not using it anyway, though, so this isn't particularly urgent.
		glChecked $ GLUseProgramStages (ibsh^.ibshPipeline) stages (ibsh^.ibshProgram) ()
	when useProgramPipeline $ do
		glChecked $ GLUseProgram 0 ()
		glChecked $ GLBindProgramPipeline (ibsh^.ibshPipeline) ()
	where
		setupProgramPipeline :: Bool
		setupProgramPipeline = useProgramPipeline
		-- We only need the program.
		useProgramPipeline :: Bool
		useProgramPipeline = False

-- * MtrlMeta

-- | TODO:
data MtrlMeta = MtrlMeta {
}
	deriving (Eq, Ord, Show)
--makeLenses ''MtrlMeta

-- * Errors

data VideoException = forall e. Exception e => VideoException e
instance Show VideoException where
	show (VideoException e) = show e
instance Exception VideoException
videoExceptionToException :: Exception e => e -> SomeException
videoExceptionToException = toException . VideoException
videoExceptionFromException :: Exception e => SomeException -> Maybe e
videoExceptionFromException x = do
	VideoException a <- fromException x
	cast a

data GLErrorVideoException = GLErrorVideoException String
instance Exception GLErrorVideoException where
	toException = videoExceptionToException
	fromException = videoExceptionFromException
instance Show GLErrorVideoException where
	show (GLErrorVideoException msg) = msg

-- * Utils

checkGLErrorsIB :: ImmutaballIOF ()
checkGLErrorsIB = do
	error_ <- BasicIBIOF . GLIO $ GLGetError id
	case error_ of
		GL_NO_ERROR -> return ()
		err -> do
			let msg = "Error: an OpenGL error occurred (" ++ show err ++ "): " ++ glErrType err
			() <- BasicIBIOF $ PutStrLn msg ()
			() <- ThrowIO (GLErrorVideoException msg)
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

glChecked :: GLIOF me -> ImmutaballIOF me
glChecked m = (BasicIBIOF . GLIO $ m) <* checkGLErrorsIB

numToGL_TEXTUREi :: M.Map Integer GLenum
numToGL_TEXTUREi = M.fromList $
	[
		(0,  GL_TEXTURE0),
		(1,  GL_TEXTURE1),
		(2,  GL_TEXTURE2),
		(3,  GL_TEXTURE3),
		(4,  GL_TEXTURE4),
		(5,  GL_TEXTURE5),
		(6,  GL_TEXTURE6),
		(7,  GL_TEXTURE7),
		(8,  GL_TEXTURE8),
		(9,  GL_TEXTURE9),
		(10, GL_TEXTURE10),
		(11, GL_TEXTURE11),
		(12, GL_TEXTURE12),
		(13, GL_TEXTURE13),
		(14, GL_TEXTURE14),
		(15, GL_TEXTURE15)
	]

makeLenses ''MtrlMeta
