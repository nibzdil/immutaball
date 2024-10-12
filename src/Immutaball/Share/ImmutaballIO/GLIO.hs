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
		mkGLDeleteProgramPipelines
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Monad.Fix
import Data.List
import Data.Word
import Foreign.C.Types
import Foreign.Ptr

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
