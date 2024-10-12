{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables, ExistentialQuantification #-}

module Immutaball.Share.ImmutaballIO.GLIO
	(
		-- * GLIO
		GLIO,
		GLIOF(..),
		runGLIO,

		-- * mfix
		FixGLIOException(..),
		fixGLIOExceptionToException,
		fixGLIOExceptionFromException,
		PrematureEvaluationFixGLIOException(..),
		EmptyFixGLIOException(..),
		fixGLIOF,
		unsafeFixGLIOFTo,

		-- * Runners
		runGLIOIO,
		hglClearColor,
		hglTexImage2D,
		hglGenTextures,
		hglDeleteTextures,
		hglTexEnvfv,
		hglTexEnviv,
		hglTexParameterfv,
		hglTexParameteriv,
		hglTexParameterIiv,
		hglTexParameterIuiv,
		hglTextureParameterfv,
		hglTextureParameteriv,
		hglTextureParameterIiv,
		hglTextureParameterIuiv,
		hglShaderSource,
		hglGenProgramPipelines,
		hglDeleteProgramPipelines,
		hglGetMaxVertexTextureImageUnits,
		hglGetShaderiv,
		hglGetProgramiv,
		hglGetShaderInfoLog,
		hglGetProgramInfoLog,
		hglGetlUniformfv,
		hglGetlUniformiv,
		hglGetlUniformuiv,
		hglGetlUniformdv,
		hglGenBuffers,
		hglDeleteBuffers,
		hglNamedBufferData,
		hglNamedBufferSubData,
		hglGenVertexArrays,
		hglDeleteVertexArrays,
		hglVertexAttribPointer,
		hglVertexAttribIPointer,
		hglVertexAttribLPointer,

		-- * GLIO aliases that apply the Fixed wrapper
		mkEmptyGLIO,
		mkPureGLIO,
		mkUnfixGLIO,
		mkJoinGLIO,
		mkGLClear,
		mkGLClearColor,
		mkGLTexImage2D,
		mkGLGenTextures,
		mkGLBindTexture,
		mkGLDeleteTextures,
		mkGLGetError,
		mkGLColor4d,
		mkGLBegin,
		mkGLVertex2d,
		mkGLEnd,
		mkGLActiveTexture,
		mkGLClientActiveTexture,
		mkGLEnable,
		mkGLDisable,
		mkGLEnablei,
		mkGLDisablei,
		mkGLTexCoord2d,
		mkGLTexEnvf,
		mkGLTexEnvi,
		mkGLTexEnvfv,
		mkGLTexEnviv,
		mkGLTexParameterf,
		mkGLTexParameteri,
		mkGLTextureParameterf,
		mkGLTextureParameteri,
		mkGLTexParameterfv,
		mkGLTexParameteriv,
		mkGLTexParameterIiv,
		mkGLTexParameterIuiv,
		mkGLTextureParameterfv,
		mkGLTextureParameteriv,
		mkGLTextureParameterIiv,
		mkGLTextureParameterIuiv,
		mkGLDepthMask,
		mkGLDepthFunc,
		mkGLBlendEquationSeparate,
		mkGLBlendEquationSeparatei,
		mkGLBlendFuncSeparate,
		mkGLBlendFuncSeparatei,
		mkGLCreateProgram,
		mkGLDeleteProgram,
		mkGLCreateShader,
		mkGLDeleteShader,
		mkGLShaderSource,
		mkGLCompileShader,
		mkGLAttachShader,
		mkGLDetachShader,
		mkGLLinkProgram,
		mkGLUseProgram,
		mkGLBindProgramPipeline,
		mkGLUseProgramStages,
		mkGLGenProgramPipelines,
		mkGLDeleteProgramPipelines,
		mkGLGenerateMipmap,
		mkGLGenerateTextureMipmap,
		mkGLGetMaxVertexTextureImageUnits,
		mkGLGetShaderiv,
		mkGLGetProgramiv,
		mkGLGetShaderInfoLog,
		mkGLGetProgramInfoLog,
		mkGLUniform1f,
		mkGLUniform2f,
		mkGLUniform3f,
		mkGLUniform4f,
		mkGLUniform1i,
		mkGLUniform2i,
		mkGLUniform3i,
		mkGLUniform4i,
		mkGLUniform1ui,
		mkGLUniform2ui,
		mkGLUniform3ui,
		mkGLUniform4ui,
		mkGLGetlUniformfv,
		mkGLGetlUniformiv,
		mkGLGetlUniformuiv,
		mkGLGetlUniformdv,
		mkGLGenBuffers,
		mkGLDeleteBuffers,
		mkGLNamedBufferData,
		mkGLNamedBufferSubData,
		mkGLGenVertexArrays,
		mkGLDeleteVertexArrays,
		mkGLBindBufferBase,
		mkGLBindBufferRange,
		mkGLBindVertexArray,
		mkGLVertexAttribPointer,
		mkGLVertexAttribIPointer,
		mkGLVertexAttribLPointer
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Monad.Fix
import Data.List
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable (sizeOf)

import Graphics.GL.Compatibility45
--import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Array.Base
import Data.Array.Storable.Safe

import Immutaball.Share.Utils

-- (mfix imports.)
import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import System.IO.Unsafe (unsafePerformIO)

-- * GLIO

type GLIO = Fixed GLIOF
data GLIOF me =
	  EmptyGLIOF
	| PureGLIOF me
	| UnfixGLIOF (GLIOF me)
	| JoinGLIOF (GLIOF (GLIOF me))

	| GLClear GLbitfield me
	| GLClearColor GLdouble GLdouble GLdouble GLdouble me

	-- | Set a texture.
	| GLTexImage2D GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum BL.ByteString me
	-- | Create a texture.
	| GLGenTextures GLsizei ([GLuint] -> me)
	-- | Set OpenGL current texture.
	| GLBindTexture GLenum GLuint me
	-- | Delete a texture.
	| GLDeleteTextures [GLuint] me
	| GLGetError (GLenum -> me)

	| GLColor4d GLdouble GLdouble GLdouble GLdouble me
	| GLBegin GLenum me
	| GLVertex2d GLdouble GLdouble me
	| GLEnd me
	| GLActiveTexture GLenum me
	| GLClientActiveTexture GLenum me
	| GLEnable GLenum me
	| GLDisable GLenum me
	| GLEnablei GLenum GLuint me
	| GLDisablei GLenum GLuint me
	| GLTexCoord2d GLdouble GLdouble me
	| GLTexEnvf GLenum GLenum GLfloat me
	| GLTexEnvi GLenum GLenum GLint me
	| GLTexEnvfv GLenum GLenum [GLfloat] me
	| GLTexEnviv GLenum GLenum [GLint] me
	| GLTexParameterf GLenum GLenum GLfloat me
	| GLTexParameteri GLenum GLenum GLint me
	| GLTextureParameterf GLenum GLenum GLfloat me
	| GLTextureParameteri GLenum GLenum GLint me
	| GLTexParameterfv GLenum GLenum [GLfloat] me
	| GLTexParameteriv GLenum GLenum [GLint] me
	| GLTexParameterIiv GLenum GLenum [GLint] me
	| GLTexParameterIuiv GLenum GLenum [GLuint] me
	| GLTextureParameterfv GLenum GLenum [GLfloat] me
	| GLTextureParameteriv GLenum GLenum [GLint] me
	| GLTextureParameterIiv GLenum GLenum [GLint] me
	| GLTextureParameterIuiv GLenum GLenum [GLuint] me

	| GLDepthMask GLboolean me
	| GLDepthFunc GLenum me

	| GLBlendEquationSeparate GLenum GLenum me
	| GLBlendEquationSeparatei GLuint GLenum GLenum me
	| GLBlendFuncSeparate GLenum GLenum GLenum GLenum me
	| GLBlendFuncSeparatei GLuint GLenum GLenum GLenum GLenum me

	| GLCreateProgram (GLuint -> me)
	| GLDeleteProgram GLuint me
	| GLCreateShader GLenum (GLuint -> me)
	| GLDeleteShader GLuint me

	| GLShaderSource GLuint [String] me
	| GLCompileShader GLuint me
	| GLAttachShader GLuint GLuint me
	| GLDetachShader GLuint GLuint me
	| GLLinkProgram GLuint me
	| GLUseProgram GLuint me
	| GLBindProgramPipeline GLuint me

	| GLUseProgramStages GLuint GLbitfield GLuint me
	| GLGenProgramPipelines GLsizei ([GLuint] -> me)
	| GLDeleteProgramPipelines [GLuint] me

	| GLGenerateMipmap GLenum me
	| GLGenerateTextureMipmap GLuint me

	-- GL_Get is polymorphic in its output size, so since we lack dependent
	-- types, we'll just provide specific specializations of glGet.
	| GLGetMaxVertexTextureImageUnits (GLint64 -> me)

	| GLGetShaderiv       GLuint GLenum (GLint  -> me)
	| GLGetProgramiv      GLuint GLenum (GLint  -> me)
	| GLGetShaderInfoLog  GLuint        (String -> me)
	| GLGetProgramInfoLog GLuint        (String -> me)

	| GLUniform1f  GLint GLfloat                                me
	| GLUniform2f  GLint GLfloat GLfloat                        me
	| GLUniform3f  GLint GLfloat GLfloat GLfloat                me
	| GLUniform4f  GLint GLfloat GLfloat GLfloat GLfloat        me
	| GLUniform1i  GLint GLint                                  me
	| GLUniform2i  GLint GLint   GLint                          me
	| GLUniform3i  GLint GLint   GLint   GLint                  me
	| GLUniform4i  GLint GLint   GLint   GLint   GLint          me
	| GLUniform1ui GLint GLuint                                 me
	| GLUniform2ui GLint GLuint  GLuint                         me
	| GLUniform3ui GLint GLuint  GLuint  GLuint                 me
	| GLUniform4ui GLint GLuint  GLuint  GLuint  GLuint         me

	| GLGetlUniformfv  GLuint GLint Integer ([GLfloat]  -> me)
	| GLGetlUniformiv  GLuint GLint Integer ([GLint]    -> me)
	| GLGetlUniformuiv GLuint GLint Integer ([GLuint]   -> me)
	| GLGetlUniformdv  GLuint GLint Integer ([GLdouble] -> me)

	| GLGenBuffers    GLsizei  ([GLuint] -> me)
	| GLDeleteBuffers [GLuint] me

	| GLNamedBufferData    GLuint         BL.ByteString GLenum me
	| GLNamedBufferSubData GLuint Integer BL.ByteString        me

	| GLGenVertexArrays    GLsizei  ([GLuint] -> me)
	| GLDeleteVertexArrays [GLuint] me

	| GLBindBufferBase  GLenum GLuint GLuint                     me
	| GLBindBufferRange GLenum GLuint GLuint GLintptr GLsizeiptr me

	| GLBindVertexArray GLuint me

	| GLVertexAttribPointer  GLuint GLint GLenum GLboolean GLsizei Integer me
	| GLVertexAttribIPointer GLuint GLint GLenum           GLsizei Integer me
	| GLVertexAttribLPointer GLuint GLint GLenum           GLsizei Integer me
instance Functor GLIOF where
	fmap :: (a -> b) -> (GLIOF a -> GLIOF b)

	fmap _f   (EmptyGLIOF)      = EmptyGLIOF
	fmap  f   (PureGLIOF a)     = PureGLIOF (f a)
	fmap  f   (UnfixGLIOF glio) = UnfixGLIOF (f <$> glio)
	fmap  f   (JoinGLIOF glio)  = JoinGLIOF (fmap f <$> glio)

	fmap f (GLClear      mask_2               withUnit) = GLClear      mask_2               (f withUnit)
	fmap f (GLClearColor red green blue alpha withUnit) = GLClearColor red green blue alpha (f withUnit)

	fmap f (GLTexImage2D     target level internalformat width height border format type_ data_ withUnit) = GLTexImage2D target level internalformat width height border format type_ data_ (f withUnit)
	fmap f (GLGenTextures    numNames       withNames) = GLGenTextures    numNames       (f . withNames)
	fmap f (GLBindTexture    target texture withUnit)  = GLBindTexture    target texture (f withUnit)
	fmap f (GLDeleteTextures textures       withUnit)  = GLDeleteTextures textures       (f withUnit)
	fmap f (GLGetError                      withError) = GLGetError                      (f . withError)

	fmap f (GLColor4d  red green blue alpha                 withUnit) = GLColor4d  red green blue alpha                 (f withUnit)
	fmap f (GLBegin    mode                                 withUnit) = GLBegin    mode                                 (f withUnit)
	fmap f (GLVertex2d x y                                  withUnit) = GLVertex2d x y                                  (f withUnit)
	fmap f (GLEnd                                           withUnit) = GLEnd                                           (f withUnit)
	fmap f (GLActiveTexture texture                         withUnit) = GLActiveTexture texture                         (f withUnit)
	fmap f (GLClientActiveTexture texture                   withUnit) = GLClientActiveTexture texture                   (f withUnit)
	fmap f (GLEnable cap                                    withUnit) = GLEnable cap                                    (f withUnit)
	fmap f (GLDisable cap                                   withUnit) = GLDisable cap                                   (f withUnit)
	fmap f (GLEnablei cap index_                            withUnit) = GLEnablei cap index_                            (f withUnit)
	fmap f (GLDisablei cap index_                           withUnit) = GLDisablei cap index_                           (f withUnit)
	fmap f (GLTexCoord2d s t                                withUnit) = GLTexCoord2d s t                                (f withUnit)
	fmap f (GLTexEnvf target pname param                    withUnit) = GLTexEnvf target pname param                    (f withUnit)
	fmap f (GLTexEnvi target pname param                    withUnit) = GLTexEnvi target pname param                    (f withUnit)
	fmap f (GLTexEnvfv target pname params                  withUnit) = GLTexEnvfv target pname params                  (f withUnit)
	fmap f (GLTexEnviv target pname params                  withUnit) = GLTexEnviv target pname params                  (f withUnit)
	fmap f (GLTexParameterf target pname param              withUnit) = GLTexParameterf target pname param              (f withUnit)
	fmap f (GLTexParameteri target pname param              withUnit) = GLTexParameteri target pname param              (f withUnit)
	fmap f (GLTextureParameterf texture pname param         withUnit) = GLTextureParameterf texture pname param         (f withUnit)
	fmap f (GLTextureParameteri texture pname param         withUnit) = GLTextureParameteri texture pname param         (f withUnit)
	fmap f (GLTexParameterfv target pname params            withUnit) = GLTexParameterfv target pname params            (f withUnit)
	fmap f (GLTexParameteriv target pname params            withUnit) = GLTexParameteriv target pname params            (f withUnit)
	fmap f (GLTexParameterIiv target pname params           withUnit) = GLTexParameterIiv target pname params           (f withUnit)
	fmap f (GLTexParameterIuiv target pname params          withUnit) = GLTexParameterIuiv target pname params          (f withUnit)
	fmap f (GLTextureParameterfv texture pname params       withUnit) = GLTextureParameterfv texture pname params       (f withUnit)
	fmap f (GLTextureParameteriv texture pname params       withUnit) = GLTextureParameteriv texture pname params       (f withUnit)
	fmap f (GLTextureParameterIiv texture pname params      withUnit) = GLTextureParameterIiv texture pname params      (f withUnit)
	fmap f (GLTextureParameterIuiv texture pname params     withUnit) = GLTextureParameterIuiv texture pname params     (f withUnit)

	fmap f (GLDepthMask flag   withUnit) = GLDepthMask flag   (f withUnit)
	fmap f (GLDepthFunc mask_2 withUnit) = GLDepthFunc mask_2 (f withUnit)

	fmap f (GLBlendEquationSeparate modeRGB modeAlpha                withUnit) = GLBlendEquationSeparate modeRGB modeAlpha                (f withUnit)
	fmap f (GLBlendEquationSeparatei buf modeRGB modeAlpha           withUnit) = GLBlendEquationSeparatei buf modeRGB modeAlpha           (f withUnit)
	fmap f (GLBlendFuncSeparate srcRGB dstRGB srcAlpha dstAlpha      withUnit) = GLBlendFuncSeparate srcRGB dstRGB srcAlpha dstAlpha      (f withUnit)
	fmap f (GLBlendFuncSeparatei buf srcRGB dstRGB srcAlpha dstAlpha withUnit) = GLBlendFuncSeparatei buf srcRGB dstRGB srcAlpha dstAlpha (f withUnit)

	fmap f (GLCreateProgram withId)           = GLCreateProgram           (f . withId)
	fmap f (GLDeleteProgram id_ withUnit)     = GLDeleteProgram id_       (f withUnit)
	fmap f (GLCreateShader shaderType withId) = GLCreateShader shaderType (f . withId)
	fmap f (GLDeleteShader id_ withUnit)      = GLDeleteShader id_        (f withUnit)

	fmap f (GLShaderSource shader strings withUnit) = GLShaderSource shader strings (f withUnit)
	fmap f (GLCompileShader id_           withUnit) = GLCompileShader id_           (f withUnit)
	fmap f (GLAttachShader program shader withUnit) = GLAttachShader program shader (f withUnit)
	fmap f (GLDetachShader program shader withUnit) = GLDetachShader program shader (f withUnit)
	fmap f (GLLinkProgram program         withUnit) = GLLinkProgram program         (f withUnit)
	fmap f (GLUseProgram id_              withUnit) = GLUseProgram id_              (f withUnit)
	fmap f (GLBindProgramPipeline id_     withUnit) = GLBindProgramPipeline id_     (f withUnit)

	fmap f (GLUseProgramStages pipeline stages program withUnit)  = GLUseProgramStages pipeline stages program (f withUnit)
	fmap f (GLGenProgramPipelines    numNames          withNames) = GLGenProgramPipelines    numNames             (f . withNames)
	fmap f (GLDeleteProgramPipelines pipelines         withUnit)  = GLDeleteProgramPipelines pipelines            (f withUnit)

	fmap f (GLGenerateMipmap        target  withUnit) = GLGenerateMipmap        target  (f withUnit)
	fmap f (GLGenerateTextureMipmap texture withUnit) = GLGenerateTextureMipmap texture (f withUnit)

	fmap f (GLGetMaxVertexTextureImageUnits withNum) = GLGetMaxVertexTextureImageUnits (f . withNum)

	fmap f (GLGetShaderiv       shader  pname withOut) = GLGetShaderiv       shader  pname (f . withOut)
	fmap f (GLGetProgramiv      program pname withOut) = GLGetProgramiv      program pname (f . withOut)
	fmap f (GLGetShaderInfoLog  shader        withLog) = GLGetShaderInfoLog  shader        (f . withLog)
	fmap f (GLGetProgramInfoLog program       withLog) = GLGetProgramInfoLog program       (f . withLog)

	fmap f (GLUniform1f  location v0          withUnit) = GLUniform1f  location v0          (f withUnit)
	fmap f (GLUniform2f  location v0 v1       withUnit) = GLUniform2f  location v0 v1       (f withUnit)
	fmap f (GLUniform3f  location v0 v1 v2    withUnit) = GLUniform3f  location v0 v1 v2    (f withUnit)
	fmap f (GLUniform4f  location v0 v1 v2 v3 withUnit) = GLUniform4f  location v0 v1 v2 v3 (f withUnit)
	fmap f (GLUniform1i  location v0          withUnit) = GLUniform1i  location v0          (f withUnit)
	fmap f (GLUniform2i  location v0 v1       withUnit) = GLUniform2i  location v0 v1       (f withUnit)
	fmap f (GLUniform3i  location v0 v1 v2    withUnit) = GLUniform3i  location v0 v1 v2    (f withUnit)
	fmap f (GLUniform4i  location v0 v1 v2 v3 withUnit) = GLUniform4i  location v0 v1 v2 v3 (f withUnit)
	fmap f (GLUniform1ui location v0          withUnit) = GLUniform1ui location v0          (f withUnit)
	fmap f (GLUniform2ui location v0 v1       withUnit) = GLUniform2ui location v0 v1       (f withUnit)
	fmap f (GLUniform3ui location v0 v1 v2    withUnit) = GLUniform3ui location v0 v1 v2    (f withUnit)
	fmap f (GLUniform4ui location v0 v1 v2 v3 withUnit) = GLUniform4ui location v0 v1 v2 v3 (f withUnit)

	fmap f (GLGetlUniformfv  program location len withOuts) = GLGetlUniformfv  program location len (f . withOuts)
	fmap f (GLGetlUniformiv  program location len withOuts) = GLGetlUniformiv  program location len (f . withOuts)
	fmap f (GLGetlUniformuiv program location len withOuts) = GLGetlUniformuiv program location len (f . withOuts)
	fmap f (GLGetlUniformdv  program location len withOuts) = GLGetlUniformdv  program location len (f . withOuts)

	fmap f (GLGenBuffers    num   withNames) = GLGenBuffers    num   (f . withNames)
	fmap f (GLDeleteBuffers names withUnit)  = GLDeleteBuffers names (f withUnit)

	fmap f (GLNamedBufferData    buffer        data_ usage withUnit) = GLNamedBufferData    buffer        data_ usage (f withUnit)
	fmap f (GLNamedBufferSubData buffer offset data_       withUnit) = GLNamedBufferSubData buffer offset data_       (f withUnit)

	fmap f (GLGenVertexArrays    num   withNames) = GLGenVertexArrays    num   (f . withNames)
	fmap f (GLDeleteVertexArrays names withUnit)  = GLDeleteVertexArrays names (f withUnit)

	fmap f (GLBindBufferBase  target index_ buffer             withUnit) = GLBindBufferBase  target index_ buffer             (f withUnit)
	fmap f (GLBindBufferRange target index_ buffer offset size withUnit) = GLBindBufferRange target index_ buffer offset size (f withUnit)

	fmap f (GLBindVertexArray array_ withUnit) = GLBindVertexArray array_ (f withUnit)

	fmap f (GLVertexAttribPointer  index_ size type_ normalized stride offset withUnit) = GLVertexAttribPointer  index_ size type_ normalized stride offset (f withUnit)
	fmap f (GLVertexAttribIPointer index_ size type_            stride offset withUnit) = GLVertexAttribIPointer index_ size type_            stride offset (f withUnit)
	fmap f (GLVertexAttribLPointer index_ size type_            stride offset withUnit) = GLVertexAttribLPointer index_ size type_            stride offset (f withUnit)

runGLIO :: GLIO -> IO ()
runGLIO glio = cata runGLIOIO glio

-- TODO: revisit:
{-
instance Foldable SDLIOF where
	foldr :: (a -> b -> b) -> b -> SDLIOF a -> b
	foldr reduce reduction0 (SDLWithInit _subsystems sdlio) = reduce sdlio reduction0
instance Traversable SDLIOF where
	traverse :: Applicative f => (a -> f b) -> SDLIOF a -> f (SDLIOF b)
	traverse traversal (SDLWithInit subsystems sdlio) = pure SDLWithInit <*> pure subsystems <*> traversal sdlio
-}


-- * mfix

data FixGLIOException = forall e. Exception e => FixGLIOException e
instance Show FixGLIOException where
	show (FixGLIOException e) = show e
instance Exception FixGLIOException
fixGLIOExceptionToException :: Exception e => e -> SomeException
fixGLIOExceptionToException = toException . FixGLIOException
fixGLIOExceptionFromException :: Exception e => SomeException -> Maybe e
fixGLIOExceptionFromException x = do
	FixGLIOException a <- fromException x
	cast a

data PrematureEvaluationFixGLIOException = PrematureEvaluationFixGLIOException
	deriving (Show)
instance Exception PrematureEvaluationFixGLIOException where
	toException = fixGLIOExceptionToException
	fromException = fixGLIOExceptionFromException

data EmptyFixGLIOException = EmptyFixGLIOException
	deriving (Show)
instance Exception EmptyFixGLIOException where
	toException = fixGLIOExceptionToException
	fromException = fixGLIOExceptionFromException

--    mfix f = mfix f >>= f
-- => mfix f = join $ f <$> mfix f
-- Incorrect: runs f twice.
	--x -> f undefined >>= mfix f
{-
fixGLIOF :: (me -> GLIOF me) -> GLIOF me
fixGLIOF f = case f (error "Error: fixGLIOF: premature evaluation of result before we could start it!") of
	x -> joinGLIOF $ f <$> x
-}
-- Do it like fixIO and fixST (see also their notes; it's a little tricky).
-- Use a lazily read MVar.
fixGLIOF :: (me -> GLIOF me) -> GLIOF me
fixGLIOF f = unsafePerformIO $ do
	mme <- newEmptyMVar
	return $ unsafeFixGLIOFTo mme f

-- | Helper for fixGLIOF.
unsafeFixGLIOFTo :: MVar me -> (me -> GLIOF me) -> GLIOF me
unsafeFixGLIOFTo mme f = unsafePerformIO $ do
	me_ <- unsafeDupableInterleaveIO (readMVar mme `catch` \BlockedIndefinitelyOnMVar -> throwIO PrematureEvaluationFixGLIOException)
	case f me_ of
		_y@(EmptyGLIOF)        -> throwIO EmptyFixGLIOException
		y@(PureGLIOF a)        -> putMVar mme a >> return y
		_y@(UnfixGLIOF glio)   -> return . UnfixGLIOF . unsafeFixGLIOFTo mme $ const glio
		-- Join: Cover all multi-branching (or else we could hang on multiple putMVars), then just fmap for all other cases.
		-- (No branching GLIOFs currently.  So proceed to the final JoinGLIOF case.)
		_y@(JoinGLIOF glio)    -> return $ JoinGLIOF (unsafeFixGLIOFTo mme . const <$> glio)

		y@( GLClear      _mask                    me) -> putMVar mme me >> return y
		y@( GLClearColor _red _green _blue _alpha me) -> putMVar mme me >> return y

		y@( GLTexImage2D     _target _level _internalformat _width _height _border _format _type _data me) -> putMVar mme me >> return y
		_y@(GLGenTextures    numNames         withNames) -> return $ GLGenTextures numNames ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withNames)
		y@( GLBindTexture    _target _texture me)        -> putMVar mme me >> return y
		y@( GLDeleteTextures _textures        me)        -> putMVar mme me >> return y
		_y@(GLGetError                        withError) -> return $ GLGetError             ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withError)

		y@( GLColor4d  _red _green _blue _alpha            me) -> putMVar mme me >> return y
		y@( GLBegin    _mode                               me) -> putMVar mme me >> return y
		y@( GLVertex2d _ _                                 me) -> putMVar mme me >> return y
		y@( GLEnd                                          me) -> putMVar mme me >> return y
		y@( GLActiveTexture _texture                       me) -> putMVar mme me >> return y
		y@( GLClientActiveTexture _texture                 me) -> putMVar mme me >> return y
		y@( GLEnable _cap                                  me) -> putMVar mme me >> return y
		y@( GLDisable _cap                                 me) -> putMVar mme me >> return y
		y@( GLEnablei _cap _index                          me) -> putMVar mme me >> return y
		y@( GLDisablei _cap _index                         me) -> putMVar mme me >> return y
		y@( GLTexCoord2d _s _t                             me) -> putMVar mme me >> return y
		y@( GLTexEnvf _target _pname _param                me) -> putMVar mme me >> return y
		y@( GLTexEnvi _target _pname _param                me) -> putMVar mme me >> return y
		y@( GLTexEnvfv _target _pname _params              me) -> putMVar mme me >> return y
		y@( GLTexEnviv _target _pname _params              me) -> putMVar mme me >> return y
		y@( GLTexParameterf _target _pname _param          me) -> putMVar mme me >> return y
		y@( GLTexParameteri _target _pname _param          me) -> putMVar mme me >> return y
		y@( GLTextureParameterf _texture _pname _param     me) -> putMVar mme me >> return y
		y@( GLTextureParameteri _texture _pname _param     me) -> putMVar mme me >> return y
		y@( GLTexParameterfv _target _pname _params        me) -> putMVar mme me >> return y
		y@( GLTexParameteriv _target _pname _params        me) -> putMVar mme me >> return y
		y@( GLTexParameterIiv _target _pname _params       me) -> putMVar mme me >> return y
		y@( GLTexParameterIuiv _target _pname _params      me) -> putMVar mme me >> return y
		y@( GLTextureParameterfv _texture _pname _params   me) -> putMVar mme me >> return y
		y@( GLTextureParameteriv _texture _pname _params   me) -> putMVar mme me >> return y
		y@( GLTextureParameterIiv _texture _pname _params  me) -> putMVar mme me >> return y
		y@( GLTextureParameterIuiv _texture _pname _params me) -> putMVar mme me >> return y

		y@( GLDepthMask _flag me) -> putMVar mme me >> return y
		y@( GLDepthFunc _func me) -> putMVar mme me >> return y

		y@( GLBlendEquationSeparate _modeRGB _modeAlpha me)       -> putMVar mme me >> return y
		y@( GLBlendEquationSeparatei _buf _modeRGB _modeAlpha me) -> putMVar mme me >> return y

		y@( GLBlendFuncSeparate _srcRGB _dstRGB _srcAlpha _dstAlpha me)       -> putMVar mme me >> return y
		y@( GLBlendFuncSeparatei _buf _srcRGB _dstRGB _srcAlpha _dstAlpha me) -> putMVar mme me >> return y

		_y@(GLCreateProgram           withId) -> return $ GLCreateProgram           ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withId)
		y@( GLDeleteProgram _id       me)     -> putMVar mme me >> return y
		_y@(GLCreateShader shaderType withId) -> return $ GLCreateShader shaderType ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withId)
		y@( GLDeleteShader _id        me)     -> putMVar mme me >> return y

		y@( GLShaderSource _shader _strings me) -> putMVar mme me >> return y
		y@( GLCompileShader _id             me) -> putMVar mme me >> return y
		y@( GLAttachShader _program _shader me) -> putMVar mme me >> return y
		y@( GLDetachShader _program _shader me) -> putMVar mme me >> return y
		y@( GLLinkProgram _program          me) -> putMVar mme me >> return y
		y@( GLUseProgram _id                me) -> putMVar mme me >> return y
		y@( GLBindProgramPipeline _id       me) -> putMVar mme me >> return y

		y@( GLUseProgramStages _pipeline _stages _program me)        -> putMVar mme me >> return y
		_y@(GLGenProgramPipelines numNames                withNames) -> return $ GLGenProgramPipelines numNames ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withNames)
		y@( GLDeleteProgramPipelines _pipelines           me)        -> putMVar mme me >> return y

		y@( GLGenerateMipmap        _target  me) -> putMVar mme me >> return y
		y@( GLGenerateTextureMipmap _texture me) -> putMVar mme me >> return y

		_y@(GLGetMaxVertexTextureImageUnits withNum) -> return $ GLGetMaxVertexTextureImageUnits ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withNum)

		_y@(GLGetShaderiv       shader  pname withOut) -> return $ GLGetShaderiv       shader  pname ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withOut)
		_y@(GLGetProgramiv      program pname withOut) -> return $ GLGetProgramiv      program pname ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withOut)
		_y@(GLGetShaderInfoLog  shader        withLog) -> return $ GLGetShaderInfoLog  shader        ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withLog)
		_y@(GLGetProgramInfoLog program       withLog) -> return $ GLGetProgramInfoLog program       ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withLog)

		y@( GLUniform1f  _location _v0             me) -> putMVar mme me >> return y
		y@( GLUniform2f  _location _v0 _v1         me) -> putMVar mme me >> return y
		y@( GLUniform3f  _location _v0 _v1 _v2     me) -> putMVar mme me >> return y
		y@( GLUniform4f  _location _v0 _v1 _v2 _v3 me) -> putMVar mme me >> return y
		y@( GLUniform1i  _location _v0             me) -> putMVar mme me >> return y
		y@( GLUniform2i  _location _v0 _v1         me) -> putMVar mme me >> return y
		y@( GLUniform3i  _location _v0 _v1 _v2     me) -> putMVar mme me >> return y
		y@( GLUniform4i  _location _v0 _v1 _v2 _v3 me) -> putMVar mme me >> return y
		y@( GLUniform1ui _location _v0             me) -> putMVar mme me >> return y
		y@( GLUniform2ui _location _v0 _v1         me) -> putMVar mme me >> return y
		y@( GLUniform3ui _location _v0 _v1 _v2     me) -> putMVar mme me >> return y
		y@( GLUniform4ui _location _v0 _v1 _v2 _v3 me) -> putMVar mme me >> return y

		_y@(GLGetlUniformfv  program location len withOuts) -> return $ GLGetlUniformfv  program location len ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withOuts)
		_y@(GLGetlUniformiv  program location len withOuts) -> return $ GLGetlUniformiv  program location len ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withOuts)
		_y@(GLGetlUniformuiv program location len withOuts) -> return $ GLGetlUniformuiv program location len ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withOuts)
		_y@(GLGetlUniformdv  program location len withOuts) -> return $ GLGetlUniformdv  program location len ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withOuts)

		_y@(GLGenBuffers    num    withNames) -> return $ GLGenBuffers num ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withNames)
		y@( GLDeleteBuffers _names me)        -> putMVar mme me >> return y

		y@( GLNamedBufferData    _buffer         _data _usage me) -> putMVar mme me >> return y
		y@( GLNamedBufferSubData _buffer _offset _data        me) -> putMVar mme me >> return y

		_y@(GLGenVertexArrays    num    withNames) -> return $ GLGenVertexArrays num ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withNames)
		y@( GLDeleteVertexArrays _names me)        -> putMVar mme me >> return y

		y@( GLBindBufferBase  _target _index _buffer               me) -> putMVar mme me >> return y
		y@( GLBindBufferRange _target _index _buffer _offset _size me) -> putMVar mme me >> return y

		y@( GLBindVertexArray _array me) -> putMVar mme me >> return y

		y@( GLVertexAttribPointer  _index _size _type _normalized _stride _offset me) -> putMVar mme me >> return y
		y@( GLVertexAttribIPointer _index _size _type             _stride _offset me) -> putMVar mme me >> return y
		y@( GLVertexAttribLPointer _index _size _type             _stride _offset me) -> putMVar mme me >> return y

instance Applicative GLIOF where
	pure = PureGLIOF
	mf <*> ma = JoinGLIOF . flip fmap mf $ \f -> JoinGLIOF .  flip fmap ma $ \a -> pure (f a)
instance Monad GLIOF where
	return = pure
	m >>= f = JoinGLIOF $ f <$> m
instance MonadFix GLIOF where
	mfix :: (a -> GLIOF a) -> GLIOF a
	mfix = fixGLIOF

-- * Runners

runGLIOIO :: GLIOF (IO ()) -> IO ()

runGLIOIO (EmptyGLIOF)      = return ()
runGLIOIO (PureGLIOF a)     = a
runGLIOIO (UnfixGLIOF glio) = runGLIOIO glio
runGLIOIO (JoinGLIOF glio)  = runGLIOIO $ runGLIOIO <$> glio

runGLIOIO (GLClear          mask_2               glio) = glClear       mask_2               >> glio
runGLIOIO (GLClearColor     red green blue alpha glio) = hglClearColor red green blue alpha >> glio
runGLIOIO (GLTexImage2D     target level internalformat width height border format type_ data_ glio) = hglTexImage2D target level internalformat width height border format type_ data_ >> glio
runGLIOIO (GLGenTextures    numNames             withNames) = hglGenTextures numNames       >>= withNames
runGLIOIO (GLBindTexture    target texture       glio) = glBindTexture target texture       >> glio
runGLIOIO (GLDeleteTextures textures             glio) = hglDeleteTextures textures         >> glio
runGLIOIO (GLGetError                            withError) = glGetError                    >>= withError

runGLIOIO (GLColor4d  red green blue alpha             glio) = glColor4d  red green blue alpha              >> glio
runGLIOIO (GLBegin    mode                             glio) = glBegin    mode                              >> glio
runGLIOIO (GLVertex2d x y                              glio) = glVertex2d x y                               >> glio
runGLIOIO (GLEnd                                       glio) = glEnd                                        >> glio
runGLIOIO (GLActiveTexture texture                     glio) = glActiveTexture texture                      >> glio
runGLIOIO (GLClientActiveTexture texture               glio) = glClientActiveTexture texture                >> glio
runGLIOIO (GLEnable cap                                glio) = glEnable cap                                 >> glio
runGLIOIO (GLDisable cap                               glio) = glDisable cap                                >> glio
runGLIOIO (GLEnablei cap index_                        glio) = glEnablei cap index_                         >> glio
runGLIOIO (GLDisablei cap index_                       glio) = glDisablei cap index_                        >> glio
runGLIOIO (GLTexCoord2d s t                            glio) = glTexCoord2d s t                             >> glio
runGLIOIO (GLTexEnvf target pname param                glio) = glTexEnvf target pname param                 >> glio
runGLIOIO (GLTexEnvi target pname param                glio) = glTexEnvi target pname param                 >> glio
runGLIOIO (GLTexEnvfv target pname params              glio) = hglTexEnvfv target pname params              >> glio
runGLIOIO (GLTexEnviv target pname params              glio) = hglTexEnviv target pname params              >> glio
runGLIOIO (GLTexParameterf target pname param          glio) = glTexParameterf target pname param           >> glio
runGLIOIO (GLTexParameteri target pname param          glio) = glTexParameteri target pname param           >> glio
runGLIOIO (GLTextureParameterf texture pname param     glio) = glTextureParameterf texture pname param      >> glio
runGLIOIO (GLTextureParameteri texture pname param     glio) = glTextureParameteri texture pname param      >> glio
runGLIOIO (GLTexParameterfv target pname params        glio) = hglTexParameterfv target pname params        >> glio
runGLIOIO (GLTexParameteriv target pname params        glio) = hglTexParameteriv target pname params        >> glio
runGLIOIO (GLTexParameterIiv target pname params       glio) = hglTexParameterIiv target pname params       >> glio
runGLIOIO (GLTexParameterIuiv target pname params      glio) = hglTexParameterIuiv target pname params      >> glio
runGLIOIO (GLTextureParameterfv texture pname params   glio) = hglTextureParameterfv texture pname params   >> glio
runGLIOIO (GLTextureParameteriv texture pname params   glio) = hglTextureParameteriv texture pname params   >> glio
runGLIOIO (GLTextureParameterIiv texture pname params  glio) = hglTextureParameterIiv texture pname params  >> glio
runGLIOIO (GLTextureParameterIuiv texture pname params glio) = hglTextureParameterIuiv texture pname params >> glio

runGLIOIO (GLDepthMask flag glio) = glDepthMask flag >> glio
runGLIOIO (GLDepthFunc func glio) = glDepthFunc func >> glio

runGLIOIO (GLBlendEquationSeparate modeRGB modeAlpha      glio) = glBlendEquationSeparate modeRGB modeAlpha      >> glio
runGLIOIO (GLBlendEquationSeparatei buf modeRGB modeAlpha glio) = glBlendEquationSeparatei buf modeRGB modeAlpha >> glio

runGLIOIO (GLBlendFuncSeparate srcRGB dstRGB srcAlpha dstAlpha      glio) = glBlendFuncSeparate srcRGB dstRGB srcAlpha dstAlpha      >> glio
runGLIOIO (GLBlendFuncSeparatei buf srcRGB dstRGB srcAlpha dstAlpha glio) = glBlendFuncSeparatei buf srcRGB dstRGB srcAlpha dstAlpha >> glio

runGLIOIO (GLCreateProgram           withId) = glCreateProgram           >>= withId
runGLIOIO (GLDeleteProgram id_       glio)   = glDeleteProgram id_       >> glio
runGLIOIO (GLCreateShader shaderType withId) = glCreateShader shaderType >>= withId
runGLIOIO (GLDeleteShader id_        glio)   = glDeleteShader id_        >> glio

runGLIOIO (GLShaderSource shader strings glio) = hglShaderSource shader strings >> glio
runGLIOIO (GLCompileShader id_           glio) = glCompileShader id_            >> glio
runGLIOIO (GLAttachShader program shader glio) = glAttachShader program shader  >> glio
runGLIOIO (GLDetachShader program shader glio) = glDetachShader program shader  >> glio
runGLIOIO (GLLinkProgram program         glio) = glLinkProgram program          >> glio
runGLIOIO (GLUseProgram id_              glio) = glUseProgram id_               >> glio
runGLIOIO (GLBindProgramPipeline id_     glio) = glBindProgramPipeline id_      >> glio

runGLIOIO (GLUseProgramStages pipeline stages program glio)      = glUseProgramStages pipeline stages program >> glio
runGLIOIO (GLGenProgramPipelines    numNames          withNames) = hglGenTextures numNames                    >>= withNames
runGLIOIO (GLDeleteProgramPipelines pipelines         glio)      = hglDeleteTextures pipelines                >> glio

runGLIOIO (GLGenerateMipmap        target  glio) = glGenerateMipmap        target  >> glio
runGLIOIO (GLGenerateTextureMipmap texture glio) = glGenerateTextureMipmap texture >> glio

runGLIOIO (GLGetMaxVertexTextureImageUnits withNum) = hglGetMaxVertexTextureImageUnits >>= withNum

runGLIOIO (GLGetShaderiv       shader  pname withOut) = hglGetShaderiv       shader  pname >>= withOut
runGLIOIO (GLGetProgramiv      program pname withOut) = hglGetProgramiv      program pname >>= withOut
runGLIOIO (GLGetShaderInfoLog  shader        withLog) = hglGetShaderInfoLog  shader        >>= withLog
runGLIOIO (GLGetProgramInfoLog program       withLog) = hglGetProgramInfoLog program       >>= withLog

runGLIOIO (GLUniform1f  location v0          glio) = glUniform1f  location v0          >> glio
runGLIOIO (GLUniform2f  location v0 v1       glio) = glUniform2f  location v0 v1       >> glio
runGLIOIO (GLUniform3f  location v0 v1 v2    glio) = glUniform3f  location v0 v1 v2    >> glio
runGLIOIO (GLUniform4f  location v0 v1 v2 v3 glio) = glUniform4f  location v0 v1 v2 v3 >> glio
runGLIOIO (GLUniform1i  location v0          glio) = glUniform1i  location v0          >> glio
runGLIOIO (GLUniform2i  location v0 v1       glio) = glUniform2i  location v0 v1       >> glio
runGLIOIO (GLUniform3i  location v0 v1 v2    glio) = glUniform3i  location v0 v1 v2    >> glio
runGLIOIO (GLUniform4i  location v0 v1 v2 v3 glio) = glUniform4i  location v0 v1 v2 v3 >> glio
runGLIOIO (GLUniform1ui location v0          glio) = glUniform1ui location v0          >> glio
runGLIOIO (GLUniform2ui location v0 v1       glio) = glUniform2ui location v0 v1       >> glio
runGLIOIO (GLUniform3ui location v0 v1 v2    glio) = glUniform3ui location v0 v1 v2    >> glio
runGLIOIO (GLUniform4ui location v0 v1 v2 v3 glio) = glUniform4ui location v0 v1 v2 v3 >> glio

runGLIOIO (GLGetlUniformfv  program location len withOuts) = hglGetlUniformfv  program location len >>= withOuts
runGLIOIO (GLGetlUniformiv  program location len withOuts) = hglGetlUniformiv  program location len >>= withOuts
runGLIOIO (GLGetlUniformuiv program location len withOuts) = hglGetlUniformuiv program location len >>= withOuts
runGLIOIO (GLGetlUniformdv  program location len withOuts) = hglGetlUniformdv  program location len >>= withOuts

runGLIOIO (GLGenBuffers    num   withNames) = hglGenBuffers    num   >>= withNames
runGLIOIO (GLDeleteBuffers names glio)      = hglDeleteBuffers names >> glio

runGLIOIO (GLNamedBufferData    buffer        data_ usage glio) = hglNamedBufferData    buffer        data_ usage >> glio
runGLIOIO (GLNamedBufferSubData buffer offset data_       glio) = hglNamedBufferSubData buffer offset data_       >> glio

runGLIOIO (GLGenVertexArrays    num   withNames) = hglGenVertexArrays    num   >>= withNames
runGLIOIO (GLDeleteVertexArrays names glio)      = hglDeleteVertexArrays names >> glio

runGLIOIO (GLBindBufferBase  target index_ buffer             glio) = glBindBufferBase  target index_ buffer             >> glio
runGLIOIO (GLBindBufferRange target index_ buffer offset size glio) = glBindBufferRange target index_ buffer offset size >> glio

runGLIOIO (GLBindVertexArray array_ glio) = glBindVertexArray array_ >> glio

runGLIOIO (GLVertexAttribPointer  index_ size type_ normalized stride offset glio) = hglVertexAttribPointer  index_ size type_ normalized stride offset >> glio
runGLIOIO (GLVertexAttribIPointer index_ size type_            stride offset glio) = hglVertexAttribIPointer index_ size type_            stride offset >> glio
runGLIOIO (GLVertexAttribLPointer index_ size type_            stride offset glio) = hglVertexAttribLPointer index_ size type_            stride offset >> glio

hglClearColor :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
hglClearColor red green blue alpha = glClearColor (realToFrac red) (realToFrac green) (realToFrac blue) (realToFrac alpha)

hglTexImage2D :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> BL.ByteString -> IO ()
hglTexImage2D target level internalformat width height border format type_ data_ = do
	let strictCopy = BL.toStrict data_  -- bytestrings only provides a CString interface to strict.
	BS.useAsCString strictCopy $ \ptr -> glTexImage2D target level internalformat width height border format type_ (castPtr ptr)

hglGenTextures :: GLsizei -> IO [GLuint]
hglGenTextures numNames = do
	let len = fromIntegral numNames  :: Integer
	array_ <- newArray_ (0, numNames - 1)
	withStorableArray array_ $ \ptr -> glGenTextures (fromIntegral len) ptr
	names <- getElems array_
	return names

hglDeleteTextures :: [GLuint] -> IO ()
hglDeleteTextures textures = do
	array_ <- newListArray (0 :: Integer, genericLength textures - 1) textures
	len    <- getNumElements array_
	withStorableArray array_ $ \ptr -> glDeleteTextures (fromIntegral len) (castPtr ptr)

hglTexEnvfv :: GLenum -> GLenum -> [GLfloat] -> IO ()
hglTexEnvfv target pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTexEnvfv target pname (castPtr ptr)

hglTexEnviv :: GLenum -> GLenum -> [GLint] -> IO ()
hglTexEnviv target pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTexEnviv target pname (castPtr ptr)

hglTexParameterfv :: GLenum -> GLenum -> [GLfloat] -> IO ()
hglTexParameterfv target pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTexParameterfv target pname (castPtr ptr)

hglTexParameteriv :: GLenum -> GLenum -> [GLint] -> IO ()
hglTexParameteriv target pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTexParameteriv target pname (castPtr ptr)

hglTexParameterIiv :: GLenum -> GLenum -> [GLint] -> IO ()
hglTexParameterIiv target pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTexParameterIiv target pname (castPtr ptr)

hglTexParameterIuiv :: GLenum -> GLenum -> [GLuint] -> IO ()
hglTexParameterIuiv target pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTexParameterIuiv target pname (castPtr ptr)

hglTextureParameterfv :: GLenum -> GLenum -> [GLfloat] -> IO ()
hglTextureParameterfv texture pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTextureParameterfv texture pname (castPtr ptr)

hglTextureParameteriv :: GLenum -> GLenum -> [GLint] -> IO ()
hglTextureParameteriv texture pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTextureParameteriv texture pname (castPtr ptr)

hglTextureParameterIiv :: GLenum -> GLenum -> [GLint] -> IO ()
hglTextureParameterIiv texture pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTextureParameterIiv texture pname (castPtr ptr)

hglTextureParameterIuiv :: GLenum -> GLenum -> [GLuint] -> IO ()
hglTextureParameterIuiv texture pname params = do
	array_ <- newListArray (0 :: Integer, genericLength params - 1) params
	_len   <- getNumElements array_
	withStorableArray array_ $ \ptr -> glTextureParameterIuiv texture pname (castPtr ptr)

hglShaderSource :: GLuint -> [String] -> IO ()
hglShaderSource shader strings0 = do
	let bstrings = numStrings `seq` map (BS.pack . map truncateChar) strings0
	foldr reduce reduction0 bstrings []
	where
		numStrings = genericLength strings0
		reduce :: BS.ByteString -> ([(Ptr CChar, Int)] -> IO ()) -> ([(Ptr CChar, Int)] -> IO ())
		reduce bstring withStrings = \strings -> BS.useAsCString bstring $ \cstring -> withStrings ((cstring, BS.length bstring):strings)
		reduction0 :: [(Ptr CChar, Int)] -> IO ()
		reduction0 reversedStrings = do
			let strings  = reverse reversedStrings
			let cstrings = map fst strings
			let lens     = map snd strings
			cstrArray <- newListArray (0 :: Integer, genericLength cstrings - 1) cstrings
			_cstrLen  <- getNumElements cstrArray
			lensArray <- newListArray (0 :: Integer, genericLength lens - 1) lens
			_lensLen  <- getNumElements lensArray
			withStorableArray cstrArray $ \cstrPtr -> withStorableArray lensArray $ \lensPtr ->
				glShaderSource shader numStrings (castPtr cstrPtr) (castPtr lensPtr)
		-- bytestring could really use a .UTF8 module, rather than just .Char8.
		truncateChar :: Char -> Word8
		truncateChar = toEnum . fromEnum

hglGenProgramPipelines :: GLsizei -> IO [GLuint]
hglGenProgramPipelines numNames = do
	let len = fromIntegral numNames  :: Integer
	array_ <- newArray_ (0, numNames - 1)
	withStorableArray array_ $ \ptr -> glGenProgramPipelines (fromIntegral len) ptr
	names <- getElems array_
	return names

hglDeleteProgramPipelines :: [GLuint] -> IO ()
hglDeleteProgramPipelines pipelines = do
	array_ <- newListArray (0 :: Integer, genericLength pipelines - 1) pipelines
	len    <- getNumElements array_
	withStorableArray array_ $ \ptr -> glDeleteProgramPipelines (fromIntegral len) (castPtr ptr)

hglGetMaxVertexTextureImageUnits :: IO GLint64
hglGetMaxVertexTextureImageUnits = do
	let numOuts = 1  :: Integer
	let safetyBuffer = 64
	let _len = fromIntegral (1 :: Integer)  :: Integer
	array_ <- newArray_ (0, numOuts - 1 + safetyBuffer)
	withStorableArray array_ $ \ptr -> glGetInteger64v GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS ptr
	outs <- getElems array_
	let num = case outs of
		([])  -> error "Internal error: hglGetMaxVertexTextureImageUnits: empty array result."
		(x:_) -> x
	return num

-- | Currently the docs seem ot only specify single-param calls.
hglGetShaderiv :: GLuint -> GLenum -> IO GLint
hglGetShaderiv shader pname = do
	let numOuts = 1  :: Integer
	let safetyBuffer = 64
	let _len = fromIntegral (1 :: Integer)  :: Integer
	array_ <- newArray_ (0, numOuts - 1 + safetyBuffer)
	withStorableArray array_ $ \ptr -> glGetShaderiv shader pname ptr
	outs <- getElems array_
	let out = case outs of
		([])  -> error "Internal error: hglGetShaderiv: empty array result."
		(x:_) -> x
	return out

-- | Currently the docs seem ot only specify single-param calls.
hglGetProgramiv :: GLuint -> GLenum -> IO GLint
hglGetProgramiv program pname = do
	let numOuts = 1  :: Integer
	let safetyBuffer = 64
	let _len = fromIntegral (1 :: Integer)  :: Integer
	array_ <- newArray_ (0, numOuts - 1 + safetyBuffer)
	withStorableArray array_ $ \ptr -> glGetProgramiv program pname ptr
	outs <- getElems array_
	let out = case outs of
		([])  -> error "Internal error: hglGetProgramiv: empty array result."
		(x:_) -> x
	return out

hglGetlUniformfv :: GLuint -> GLint -> Integer -> IO [GLfloat]
hglGetlUniformfv program location len_ = do
	let len = max 0 $ len_
	let safetyBuffer = 16
	let z = 0.0 :: GLfloat  -- Also specifies the array value type.
	outsArray <- newArray (0, len - 1 + safetyBuffer) z
	withStorableArray outsArray $ \outsPtr -> glGetnUniformfv program location (fromIntegral $ len * (fromIntegral $ sizeOf z)) outsPtr
	outs <- genericTake len <$> getElems outsArray
	return outs

hglGetlUniformiv :: GLuint -> GLint -> Integer -> IO [GLint]
hglGetlUniformiv program location len_ = do
	let len = max 0 $ len_
	let safetyBuffer = 16
	let z = 0 :: GLint  -- Also specifies the array value type.
	outsArray <- newArray (0, len - 1 + safetyBuffer) z
	withStorableArray outsArray $ \outsPtr -> glGetnUniformiv program location (fromIntegral $ len * (fromIntegral $ sizeOf z)) outsPtr
	outs <- genericTake len <$> getElems outsArray
	return outs

hglGetlUniformuiv :: GLuint -> GLint -> Integer -> IO [GLuint]
hglGetlUniformuiv program location len_ = do
	let len = max 0 $ len_
	let safetyBuffer = 16
	let z = 0 :: GLuint  -- Also specifies the array value type.
	outsArray <- newArray (0, len - 1 + safetyBuffer) z
	withStorableArray outsArray $ \outsPtr -> glGetnUniformuiv program location (fromIntegral $ len * (fromIntegral $ sizeOf z)) outsPtr
	outs <- genericTake len <$> getElems outsArray
	return outs

hglGetlUniformdv :: GLuint -> GLint -> Integer -> IO [GLdouble]
hglGetlUniformdv program location len_ = do
	let len = max 0 $ len_
	let safetyBuffer = 16
	let z = 0.0 :: GLdouble  -- Also specifies the array value type.
	outsArray <- newArray (0, len - 1 + safetyBuffer) z
	withStorableArray outsArray $ \outsPtr -> glGetnUniformdv program location (fromIntegral $ len * (fromIntegral $ sizeOf z)) outsPtr
	outs <- genericTake len <$> getElems outsArray
	return outs

-- | glGetShaderInfoLog.
--
-- We don't know the size of the log without adding extra arguments,
-- so to make it length agnostic we attempt a length, but if the result is too
-- close in length, double the length and try again, up to a fixed limit, after
-- which we leave the truncation.
hglGetShaderInfoLog :: GLuint -> IO String
hglGetShaderInfoLog shader = trySize initialSize
	where
		initialSize :: Integer
		initialSize = 4096
		threshold :: Integer
		threshold = flip const 3 8
		safetyBuffer :: Integer
		safetyBuffer = 64
		maxSize :: Integer
		maxSize = 100 * 1024 * 1024
		trySize :: Integer -> IO String
		trySize size_ = do
			let size = min maxSize size_

			let lenLen = 1 :: Integer
			let z = fromIntegral 0
			lenArray <- newArray  (0, max 1 $ lenLen - 1 + safetyBuffer) z
			logArray <- newArray_ (0, max 1 $ size - 1 + safetyBuffer)
			withStorableArray lenArray $ \lenPtr ->
				withStorableArray logArray $ \logPtr ->
					glGetShaderInfoLog shader (fromIntegral size) lenPtr logPtr
			-- Also specifies the array value types.
			(len   :: GLsizei) <- readArray lenArray 0
			(_char :: GLchar ) <- readArray logArray 0
			if size < maxSize && (fromIntegral len) + threshold >= size
				then trySize (2 * size)
				else do
					log <- withStorableArray logArray $ \logPtr -> BS.packCStringLen (logPtr, fromIntegral len)
					let logStr = map asciiChar . BS.unpack $ log
					return logStr
		-- bytestring could really use a .UTF8 module, rather than just .Char8.
		asciiChar :: Word8 -> Char
		asciiChar = toEnum . fromEnum

hglGetProgramInfoLog :: GLuint -> IO String
hglGetProgramInfoLog program = trySize initialSize
	where
		initialSize :: Integer
		initialSize = 4096
		threshold :: Integer
		threshold = flip const 3 8
		safetyBuffer :: Integer
		safetyBuffer = 64
		maxSize :: Integer
		maxSize = 100 * 1024 * 1024
		trySize :: Integer -> IO String
		trySize size_ = do
			let size = min maxSize size_

			let lenLen = 1 :: Integer
			let z = fromIntegral 0
			lenArray <- newArray  (0, max 1 $ lenLen - 1 + safetyBuffer) z
			logArray <- newArray_ (0, max 1 $ size - 1 + safetyBuffer)
			withStorableArray lenArray $ \lenPtr ->
				withStorableArray logArray $ \logPtr ->
					glGetProgramInfoLog program (fromIntegral size) lenPtr logPtr
			-- Also specifies the array value types.
			(len   :: GLsizei) <- readArray lenArray 0
			(_char :: GLchar ) <- readArray logArray 0
			if size < maxSize && (fromIntegral len) + threshold >= size
				then trySize (2 * size)
				else do
					log <- withStorableArray logArray $ \logPtr -> BS.packCStringLen (logPtr, fromIntegral len)
					let logStr = map asciiChar . BS.unpack $ log
					return logStr
		-- bytestring could really use a .UTF8 module, rather than just .Char8.
		asciiChar :: Word8 -> Char
		asciiChar = toEnum . fromEnum

hglGenBuffers :: GLsizei -> IO [GLuint]
hglGenBuffers num = do
	let len = fromIntegral num  :: Integer
	array_ <- newArray_ (0, num - 1)
	withStorableArray array_ $ \ptr -> glGenBuffers (fromIntegral len) ptr
	names <- getElems array_
	return names

hglDeleteBuffers :: [GLuint] -> IO ()
hglDeleteBuffers names = do
	array_ <- newListArray (0 :: Integer, genericLength names - 1) names
	len    <- getNumElements array_
	withStorableArray array_ $ \ptr -> glDeleteBuffers (fromIntegral len) (castPtr ptr)

hglNamedBufferData :: GLuint -> BL.ByteString -> GLenum -> IO ()
hglNamedBufferData buffer data_ usage = do
	let strictCopy = BL.toStrict data_  -- bytestrings only provides a CString interface for strict.
	BS.useAsCStringLen strictCopy $ \(ptr, len) -> do
		glNamedBufferData buffer (fromIntegral len) (castPtr ptr) usage

hglNamedBufferSubData :: GLuint -> Integer -> BL.ByteString -> IO ()
hglNamedBufferSubData buffer offset data_ = do
	let strictCopy = BL.toStrict data_  -- bytestrings only provides a CString interface for strict.
	BS.useAsCStringLen strictCopy $ \(ptr, len) -> do
		glNamedBufferSubData buffer (fromIntegral offset) (fromIntegral len) (castPtr ptr)

hglGenVertexArrays :: GLsizei -> IO [GLuint]
hglGenVertexArrays num = do
	let len = fromIntegral num  :: Integer
	array_ <- newArray_ (0, num - 1)
	withStorableArray array_ $ \ptr -> glGenVertexArrays (fromIntegral len) ptr
	names <- getElems array_
	return names

hglDeleteVertexArrays :: [GLuint] -> IO ()
hglDeleteVertexArrays names = do
	array_ <- newListArray (0 :: Integer, genericLength names - 1) names
	len    <- getNumElements array_
	withStorableArray array_ $ \ptr -> glDeleteVertexArrays (fromIntegral len) (castPtr ptr)

hglVertexAttribPointer :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Integer -> IO ()
hglVertexAttribPointer index_ size type_ normalized stride offset_ = do
	let offset = castPtr $ nullPtr `plusPtr` (fromIntegral offset_)
	glVertexAttribPointer index_ size type_ normalized stride offset

hglVertexAttribIPointer :: GLuint -> GLint -> GLenum -> GLsizei -> Integer -> IO ()
hglVertexAttribIPointer index_ size type_ stride offset_ = do
	let offset = castPtr $ nullPtr `plusPtr` (fromIntegral offset_)
	glVertexAttribIPointer index_ size type_ stride offset

hglVertexAttribLPointer :: GLuint -> GLint -> GLenum -> GLsizei -> Integer -> IO ()
hglVertexAttribLPointer index_ size type_ stride offset_ = do
	let offset = castPtr $ nullPtr `plusPtr` (fromIntegral offset_)
	glVertexAttribLPointer index_ size type_ stride offset

-- * GLIO aliases that apply the Fixed wrapper

mkEmptyGLIO :: GLIO
mkEmptyGLIO = Fixed $ EmptyGLIOF

mkPureGLIO :: GLIO -> GLIO
mkPureGLIO glio = Fixed $ PureGLIOF glio

mkUnfixGLIO :: GLIO -> GLIO
mkUnfixGLIO glio = Fixed $ UnfixGLIOF (getFixed glio)

mkJoinGLIO :: GLIO -> GLIO
mkJoinGLIO glio = Fixed $ JoinGLIOF (getFixed <$> getFixed glio)

mkGLClear :: GLbitfield -> GLIO -> GLIO
mkGLClear mask_2 glio = Fixed $ GLClear mask_2 glio

mkGLClearColor :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLIO -> GLIO
mkGLClearColor red green blue alpha glio = Fixed $ GLClearColor red green blue alpha glio

mkGLTexImage2D :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> BL.ByteString -> GLIO -> GLIO
mkGLTexImage2D target level internalformat width height border format type_ data_ glio = Fixed $ GLTexImage2D target level internalformat width height border format type_ data_ glio

mkGLGenTextures :: GLsizei -> ([GLuint] -> GLIO) -> GLIO
mkGLGenTextures numNames withNames = Fixed $ GLGenTextures numNames withNames

mkGLBindTexture :: GLenum -> GLuint -> GLIO -> GLIO
mkGLBindTexture target texture glio = Fixed $ GLBindTexture target texture glio

mkGLDeleteTextures :: [GLuint] -> GLIO -> GLIO
mkGLDeleteTextures textures glio = Fixed $ GLDeleteTextures textures glio

mkGLGetError :: (GLenum -> GLIO) -> GLIO
mkGLGetError withError = Fixed $ GLGetError withError

mkGLColor4d :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLIO -> GLIO
mkGLColor4d red green blue alpha glio = Fixed $ GLColor4d red green blue alpha glio

mkGLBegin :: GLenum -> GLIO -> GLIO
mkGLBegin mode glio = Fixed $ GLBegin mode glio

mkGLVertex2d :: GLdouble -> GLdouble -> GLIO -> GLIO
mkGLVertex2d x y glio = Fixed $ GLVertex2d x y glio

mkGLEnd :: GLIO -> GLIO
mkGLEnd glio = Fixed $ GLEnd glio

mkGLActiveTexture :: GLenum -> GLIO -> GLIO
mkGLActiveTexture texture glio = Fixed $ GLActiveTexture texture glio

mkGLClientActiveTexture :: GLenum -> GLIO -> GLIO
mkGLClientActiveTexture texture glio = Fixed $ GLClientActiveTexture texture glio

mkGLEnable :: GLenum -> GLIO -> GLIO
mkGLEnable cap glio = Fixed $ GLEnable cap glio

mkGLDisable :: GLenum -> GLIO -> GLIO
mkGLDisable cap glio = Fixed $ GLDisable cap glio

mkGLEnablei :: GLenum -> GLuint -> GLIO -> GLIO
mkGLEnablei cap index_ glio = Fixed $ GLEnablei cap index_ glio

mkGLDisablei :: GLenum -> GLuint -> GLIO -> GLIO
mkGLDisablei cap index_ glio = Fixed $ GLDisablei cap index_ glio

mkGLTexCoord2d :: GLdouble -> GLdouble -> GLIO -> GLIO
mkGLTexCoord2d s t glio = Fixed $ GLTexCoord2d s t glio

mkGLTexEnvf :: GLenum -> GLenum -> GLfloat -> GLIO -> GLIO
mkGLTexEnvf target pname param glio = Fixed $ GLTexEnvf target pname param glio

mkGLTexEnvi :: GLenum -> GLenum -> GLint -> GLIO -> GLIO
mkGLTexEnvi target pname param glio = Fixed $ GLTexEnvi target pname param glio

mkGLTexEnvfv :: GLenum -> GLenum -> [GLfloat] -> GLIO -> GLIO
mkGLTexEnvfv target pname params glio = Fixed $ GLTexEnvfv target pname params glio

mkGLTexEnviv :: GLenum -> GLenum -> [GLint] -> GLIO -> GLIO
mkGLTexEnviv target pname params glio = Fixed $ GLTexEnviv target pname params glio

mkGLTexParameterf :: GLenum -> GLenum -> GLfloat -> GLIO -> GLIO
mkGLTexParameterf target pname param glio = Fixed $ GLTexParameterf target pname param glio

mkGLTexParameteri :: GLenum -> GLenum -> GLint -> GLIO -> GLIO
mkGLTexParameteri target pname param glio = Fixed $ GLTexParameteri target pname param glio

mkGLTextureParameterf :: GLenum -> GLenum -> GLfloat -> GLIO -> GLIO
mkGLTextureParameterf texture pname param glio = Fixed $ GLTextureParameterf texture pname param glio

mkGLTextureParameteri :: GLenum -> GLenum -> GLint -> GLIO -> GLIO
mkGLTextureParameteri texture pname param glio = Fixed $ GLTextureParameteri texture pname param glio

mkGLTexParameterfv :: GLenum -> GLenum -> [GLfloat] -> GLIO -> GLIO
mkGLTexParameterfv target pname params glio = Fixed $ GLTexParameterfv target pname params glio

mkGLTexParameteriv :: GLenum -> GLenum -> [GLint] -> GLIO -> GLIO
mkGLTexParameteriv target pname params glio = Fixed $ GLTexParameteriv target pname params glio

mkGLTexParameterIiv :: GLenum -> GLenum -> [GLint] -> GLIO -> GLIO
mkGLTexParameterIiv target pname params glio = Fixed $ GLTexParameterIiv target pname params glio

mkGLTexParameterIuiv :: GLenum -> GLenum -> [GLuint] -> GLIO -> GLIO
mkGLTexParameterIuiv target pname params glio = Fixed $ GLTexParameterIuiv target pname params glio

mkGLTextureParameterfv :: GLenum -> GLenum -> [GLfloat] -> GLIO -> GLIO
mkGLTextureParameterfv texture pname params glio = Fixed $ GLTextureParameterfv texture pname params glio

mkGLTextureParameteriv :: GLenum -> GLenum -> [GLint] -> GLIO -> GLIO
mkGLTextureParameteriv texture pname params glio = Fixed $ GLTextureParameteriv texture pname params glio

mkGLTextureParameterIiv :: GLenum -> GLenum -> [GLint] -> GLIO -> GLIO
mkGLTextureParameterIiv texture pname params glio = Fixed $ GLTextureParameterIiv texture pname params glio

mkGLTextureParameterIuiv :: GLenum -> GLenum -> [GLuint] -> GLIO -> GLIO
mkGLTextureParameterIuiv texture pname params glio = Fixed $ GLTextureParameterIuiv texture pname params glio

mkGLDepthMask :: GLboolean -> GLIO -> GLIO
mkGLDepthMask flag glio = Fixed $ GLDepthMask flag glio

mkGLDepthFunc :: GLenum -> GLIO -> GLIO
mkGLDepthFunc func glio = Fixed $ GLDepthFunc func glio

mkGLBlendEquationSeparate :: GLenum -> GLenum -> GLIO -> GLIO
mkGLBlendEquationSeparate modeRGB modeAlpha glio = Fixed $ GLBlendEquationSeparate modeRGB modeAlpha glio

mkGLBlendEquationSeparatei :: GLuint -> GLenum -> GLenum -> GLIO -> GLIO
mkGLBlendEquationSeparatei buf modeRGB modeAlpha glio = Fixed $ GLBlendEquationSeparatei buf modeRGB modeAlpha glio

mkGLBlendFuncSeparate :: GLenum -> GLenum -> GLenum -> GLenum -> GLIO -> GLIO
mkGLBlendFuncSeparate srcRGB dstRGB srcALpha dstAlpha glio = Fixed $ GLBlendFuncSeparate srcRGB dstRGB srcALpha dstAlpha glio

mkGLBlendFuncSeparatei :: GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> GLIO -> GLIO
mkGLBlendFuncSeparatei buf srcRGB dstRGB srcALpha dstAlpha glio = Fixed $ GLBlendFuncSeparatei buf srcRGB dstRGB srcALpha dstAlpha glio

mkGLCreateProgram :: (GLuint -> GLIO) -> GLIO
mkGLCreateProgram withId = Fixed $ GLCreateProgram withId

mkGLDeleteProgram :: GLuint -> GLIO -> GLIO
mkGLDeleteProgram id_ glio = Fixed $ GLDeleteProgram id_ glio

mkGLCreateShader :: GLenum -> (GLuint -> GLIO) -> GLIO
mkGLCreateShader shaderType withId = Fixed $ GLCreateShader shaderType withId

mkGLDeleteShader :: GLuint -> GLIO -> GLIO
mkGLDeleteShader id_ glio = Fixed $ GLDeleteShader id_ glio

mkGLShaderSource :: GLuint -> [String] -> GLIO -> GLIO
mkGLShaderSource shader strings glio = Fixed $ GLShaderSource shader strings glio

mkGLCompileShader :: GLuint -> GLIO -> GLIO
mkGLCompileShader id_ glio = Fixed $ GLCompileShader id_ glio

mkGLAttachShader :: GLuint -> GLuint -> GLIO -> GLIO
mkGLAttachShader program shader glio = Fixed $ GLAttachShader program shader glio

mkGLDetachShader :: GLuint -> GLuint -> GLIO -> GLIO
mkGLDetachShader program shader glio = Fixed $ GLDetachShader program shader glio

mkGLLinkProgram :: GLuint -> GLIO -> GLIO
mkGLLinkProgram program glio = Fixed $ GLLinkProgram program glio

mkGLUseProgram :: GLuint -> GLIO -> GLIO
mkGLUseProgram id_ glio = Fixed $ GLUseProgram id_ glio

mkGLBindProgramPipeline :: GLuint -> GLIO -> GLIO
mkGLBindProgramPipeline id_ glio = Fixed $ GLBindProgramPipeline id_ glio

mkGLUseProgramStages :: GLuint -> GLbitfield -> GLuint -> GLIO -> GLIO
mkGLUseProgramStages pipeline stages program glio = Fixed $ GLUseProgramStages pipeline stages program glio

mkGLGenProgramPipelines :: GLsizei -> ([GLuint] -> GLIO) -> GLIO
mkGLGenProgramPipelines numNames withNames = Fixed $ GLGenProgramPipelines numNames withNames

mkGLDeleteProgramPipelines :: [GLuint] -> GLIO -> GLIO
mkGLDeleteProgramPipelines pipelines glio = Fixed $ GLDeleteProgramPipelines pipelines glio

mkGLGenerateMipmap :: GLenum -> GLIO -> GLIO
mkGLGenerateMipmap target glio = Fixed $ GLGenerateMipmap target glio

mkGLGenerateTextureMipmap :: GLuint -> GLIO -> GLIO
mkGLGenerateTextureMipmap texture glio = Fixed $ GLGenerateTextureMipmap texture glio

mkGLGetMaxVertexTextureImageUnits :: (GLint64 -> GLIO) -> GLIO
mkGLGetMaxVertexTextureImageUnits withNum = Fixed $ GLGetMaxVertexTextureImageUnits withNum

mkGLGetShaderiv :: GLuint -> GLenum -> (GLint -> GLIO) -> GLIO
mkGLGetShaderiv shader pname withOut = Fixed $ GLGetShaderiv shader pname withOut

mkGLGetProgramiv :: GLuint -> GLenum -> (GLint -> GLIO) -> GLIO
mkGLGetProgramiv program pname withOut = Fixed $ GLGetProgramiv program pname withOut

mkGLGetShaderInfoLog :: GLuint -> (String -> GLIO) -> GLIO
mkGLGetShaderInfoLog shader withLog = Fixed $ GLGetShaderInfoLog shader withLog

mkGLGetProgramInfoLog :: GLuint -> (String -> GLIO) -> GLIO
mkGLGetProgramInfoLog program withLog = Fixed $ GLGetProgramInfoLog program withLog

mkGLUniform1f :: GLint -> GLfloat -> GLIO -> GLIO
mkGLUniform1f location v0 glio = Fixed $ GLUniform1f location v0 glio

mkGLUniform2f :: GLint -> GLfloat -> GLfloat -> GLIO -> GLIO
mkGLUniform2f location v0 v1 glio = Fixed $ GLUniform2f location v0 v1 glio

mkGLUniform3f :: GLint -> GLfloat -> GLfloat -> GLfloat -> GLIO -> GLIO
mkGLUniform3f location v0 v1 v2 glio = Fixed $ GLUniform3f location v0 v1 v2 glio

mkGLUniform4f :: GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLIO -> GLIO
mkGLUniform4f location v0 v1 v2 v3 glio = Fixed $ GLUniform4f location v0 v1 v2 v3 glio

mkGLUniform1i :: GLint -> GLint -> GLIO -> GLIO
mkGLUniform1i location v0 glio = Fixed $ GLUniform1i location v0 glio

mkGLUniform2i :: GLint -> GLint -> GLint -> GLIO -> GLIO
mkGLUniform2i location v0 v1 glio = Fixed $ GLUniform2i location v0 v1 glio

mkGLUniform3i :: GLint -> GLint -> GLint -> GLint -> GLIO -> GLIO
mkGLUniform3i location v0 v1 v2 glio = Fixed $ GLUniform3i location v0 v1 v2 glio

mkGLUniform4i :: GLint -> GLint -> GLint -> GLint -> GLint -> GLIO -> GLIO
mkGLUniform4i location v0 v1 v2 v3 glio = Fixed $ GLUniform4i location v0 v1 v2 v3 glio

mkGLUniform1ui :: GLint -> GLuint -> GLIO -> GLIO
mkGLUniform1ui location v0 glio = Fixed $ GLUniform1ui location v0 glio

mkGLUniform2ui :: GLint -> GLuint -> GLuint -> GLIO -> GLIO
mkGLUniform2ui location v0 v1 glio = Fixed $ GLUniform2ui location v0 v1 glio

mkGLUniform3ui :: GLint -> GLuint -> GLuint -> GLuint -> GLIO -> GLIO
mkGLUniform3ui location v0 v1 v2 glio = Fixed $ GLUniform3ui location v0 v1 v2 glio

mkGLUniform4ui :: GLint -> GLuint -> GLuint -> GLuint -> GLuint -> GLIO -> GLIO
mkGLUniform4ui location v0 v1 v2 v3 glio = Fixed $ GLUniform4ui location v0 v1 v2 v3 glio

mkGLGetlUniformfv :: GLuint -> GLint -> Integer -> ([GLfloat] -> GLIO) -> GLIO
mkGLGetlUniformfv program location len withOuts = Fixed $ GLGetlUniformfv program location len withOuts

mkGLGetlUniformiv :: GLuint -> GLint -> Integer -> ([GLint] -> GLIO) -> GLIO
mkGLGetlUniformiv program location len withOuts = Fixed $ GLGetlUniformiv program location len withOuts

mkGLGetlUniformuiv :: GLuint -> GLint -> Integer -> ([GLuint] -> GLIO) -> GLIO
mkGLGetlUniformuiv program location len withOuts = Fixed $ GLGetlUniformuiv program location len withOuts

mkGLGetlUniformdv :: GLuint -> GLint -> Integer -> ([GLdouble] -> GLIO) -> GLIO
mkGLGetlUniformdv program location len withOuts = Fixed $ GLGetlUniformdv program location len withOuts

mkGLGenBuffers :: GLsizei -> ([GLuint] -> GLIO) -> GLIO
mkGLGenBuffers num withNames = Fixed $ GLGenBuffers num withNames

mkGLDeleteBuffers :: [GLuint] -> GLIO -> GLIO
mkGLDeleteBuffers names glio = Fixed $ GLDeleteBuffers names glio

mkGLNamedBufferData :: GLuint -> BL.ByteString -> GLenum -> GLIO -> GLIO
mkGLNamedBufferData buffer data_ usage glio = Fixed $ GLNamedBufferData buffer data_ usage glio

mkGLNamedBufferSubData :: GLuint -> Integer -> BL.ByteString -> GLIO -> GLIO
mkGLNamedBufferSubData buffer offset data_ glio = Fixed $ GLNamedBufferSubData buffer offset data_ glio

mkGLGenVertexArrays :: GLsizei -> ([GLuint] -> GLIO) -> GLIO
mkGLGenVertexArrays num withNames = Fixed $ GLGenVertexArrays num withNames

mkGLDeleteVertexArrays :: [GLuint] -> GLIO -> GLIO
mkGLDeleteVertexArrays names glio = Fixed $ GLDeleteVertexArrays names glio

mkGLBindBufferBase :: GLenum -> GLuint -> GLuint -> GLIO -> GLIO
mkGLBindBufferBase target index_ buffer glio = Fixed $ GLBindBufferBase target index_ buffer glio

mkGLBindBufferRange :: GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> GLIO -> GLIO
mkGLBindBufferRange target index_ buffer offset size glio = Fixed $ GLBindBufferRange target index_ buffer offset size glio

mkGLBindVertexArray :: GLuint -> GLIO -> GLIO
mkGLBindVertexArray array_ glio = Fixed $ GLBindVertexArray array_ glio

mkGLVertexAttribPointer :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Integer -> GLIO -> GLIO
mkGLVertexAttribPointer index_ size type_ normalized stride offset glio = Fixed $ GLVertexAttribPointer index_ size type_ normalized stride offset glio

mkGLVertexAttribIPointer :: GLuint -> GLint -> GLenum -> GLsizei -> Integer -> GLIO -> GLIO
mkGLVertexAttribIPointer index_ size type_ stride offset glio = Fixed $ GLVertexAttribIPointer index_ size type_ stride offset glio

mkGLVertexAttribLPointer :: GLuint -> GLint -> GLenum -> GLsizei -> Integer -> GLIO -> GLIO
mkGLVertexAttribLPointer index_ size type_ stride offset glio = Fixed $ GLVertexAttribLPointer index_ size type_ stride offset glio
