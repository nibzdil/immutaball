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

		-- * GLIO aliases that apply the Fixed wrapper
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
		mkGLTexEnviv
	) where

import Prelude ()
import Immutaball.Prelude

import Data.List
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
	  GLClear GLbitfield me
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
instance Functor GLIOF where
	fmap :: (a -> b) -> (GLIOF a -> GLIOF b)
	fmap f (GLClear      mask_2               withUnit) = GLClear      mask_2               (f withUnit)
	fmap f (GLClearColor red green blue alpha withUnit) = GLClearColor red green blue alpha (f withUnit)

	fmap f (GLTexImage2D     target level internalformat width height border format type_ data_ withUnit) = GLTexImage2D target level internalformat width height border format type_ data_ (f withUnit)
	fmap f (GLGenTextures    numNames       withNames) = GLGenTextures    numNames       (f . withNames)
	fmap f (GLBindTexture    target texture withUnit)  = GLBindTexture    target texture (f withUnit)
	fmap f (GLDeleteTextures textures       withUnit)  = GLDeleteTextures textures       (f withUnit)
	fmap f (GLGetError                      withError) = GLGetError                      (f . withError)

	fmap f (GLColor4d  red green blue alpha withUnit) = GLColor4d  red green blue alpha (f withUnit)
	fmap f (GLBegin    mode                 withUnit) = GLBegin    mode                 (f withUnit)
	fmap f (GLVertex2d x y                  withUnit) = GLVertex2d x y                  (f withUnit)
	fmap f (GLEnd                           withUnit) = GLEnd                           (f withUnit)
	fmap f (GLActiveTexture texture         withUnit) = GLActiveTexture texture         (f withUnit)
	fmap f (GLClientActiveTexture texture   withUnit) = GLClientActiveTexture texture   (f withUnit)
	fmap f (GLEnable cap                    withUnit) = GLEnable cap                    (f withUnit)
	fmap f (GLDisable cap                   withUnit) = GLDisable cap                   (f withUnit)
	fmap f (GLEnablei cap index_            withUnit) = GLEnablei cap index_            (f withUnit)
	fmap f (GLDisablei cap index_           withUnit) = GLDisablei cap index_           (f withUnit)
	fmap f (GLTexCoord2d s t                withUnit) = GLTexCoord2d s t                (f withUnit)
	fmap f (GLTexEnvf target pname param    withUnit) = GLTexEnvf target pname param    (f withUnit)
	fmap f (GLTexEnvi target pname param    withUnit) = GLTexEnvi target pname param    (f withUnit)
	fmap f (GLTexEnvfv target pname params  withUnit) = GLTexEnvfv target pname params  (f withUnit)
	fmap f (GLTexEnviv target pname params  withUnit) = GLTexEnviv target pname params  (f withUnit)

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
		y@( GLClear      _mask                    me) -> putMVar mme me >> return y
		y@( GLClearColor _red _green _blue _alpha me) -> putMVar mme me >> return y

		y@( GLTexImage2D     _target _level _internalformat _width _height _border _format _type _data me) -> putMVar mme me >> return y
		_y@(GLGenTextures    numNames         withNames) -> return $ GLGenTextures numNames ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withNames)
		y@( GLBindTexture    _target _texture me)        -> putMVar mme me >> return y
		y@( GLDeleteTextures _textures        me)        -> putMVar mme me >> return y
		_y@(GLGetError                        withError) -> return $ GLGetError             ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withError)

		y@( GLColor4d  _red _green _blue _alpha me) -> putMVar mme me >> return y
		y@( GLBegin    _mode                    me) -> putMVar mme me >> return y
		y@( GLVertex2d _ _                      me) -> putMVar mme me >> return y
		y@( GLEnd                               me) -> putMVar mme me >> return y
		y@( GLActiveTexture _texture            me) -> putMVar mme me >> return y
		y@( GLClientActiveTexture _texture      me) -> putMVar mme me >> return y
		y@( GLEnable _cap                       me) -> putMVar mme me >> return y
		y@( GLDisable _cap                      me) -> putMVar mme me >> return y
		y@( GLEnablei _cap _index               me) -> putMVar mme me >> return y
		y@( GLDisablei _cap _index              me) -> putMVar mme me >> return y
		y@( GLTexCoord2d _s _t                  me) -> putMVar mme me >> return y
		y@( GLTexEnvf _target _pname _param     me) -> putMVar mme me >> return y
		y@( GLTexEnvi _target _pname _param     me) -> putMVar mme me >> return y
		y@( GLTexEnvfv _target _pname _params   me) -> putMVar mme me >> return y
		y@( GLTexEnviv _target _pname _params   me) -> putMVar mme me >> return y

-- * Runners

runGLIOIO :: GLIOF (IO ()) -> IO ()
runGLIOIO (GLClear          mask_2               glio) = glClear       mask_2               >> glio
runGLIOIO (GLClearColor     red green blue alpha glio) = hglClearColor red green blue alpha >> glio
runGLIOIO (GLTexImage2D     target level internalformat width height border format type_ data_ glio) = hglTexImage2D target level internalformat width height border format type_ data_ >> glio
runGLIOIO (GLGenTextures    numNames             withNames) = hglGenTextures numNames       >>= withNames
runGLIOIO (GLBindTexture    target texture       glio) = glBindTexture target texture       >> glio
runGLIOIO (GLDeleteTextures textures             glio) = hglDeleteTextures textures         >> glio
runGLIOIO (GLGetError                            withError) = glGetError                    >>= withError

runGLIOIO (GLColor4d  red green blue alpha glio) = glColor4d  red green blue alpha >> glio
runGLIOIO (GLBegin    mode                 glio) = glBegin    mode                 >> glio
runGLIOIO (GLVertex2d x y                  glio) = glVertex2d x y                  >> glio
runGLIOIO (GLEnd                           glio) = glEnd                           >> glio
runGLIOIO (GLActiveTexture texture         glio) = glActiveTexture texture         >> glio
runGLIOIO (GLClientActiveTexture texture   glio) = glClientActiveTexture texture   >> glio
runGLIOIO (GLEnable cap                    glio) = glEnable cap                    >> glio
runGLIOIO (GLDisable cap                   glio) = glDisable cap                   >> glio
runGLIOIO (GLEnablei cap index_            glio) = glEnablei cap index_            >> glio
runGLIOIO (GLDisablei cap index_           glio) = glDisablei cap index_           >> glio
runGLIOIO (GLTexCoord2d s t                glio) = glTexCoord2d s t                >> glio
runGLIOIO (GLTexEnvf target pname param    glio) = glTexEnvf target pname param    >> glio
runGLIOIO (GLTexEnvi target pname param    glio) = glTexEnvi target pname param    >> glio
runGLIOIO (GLTexEnvfv target pname params  glio) = hglTexEnvfv target pname params >> glio
runGLIOIO (GLTexEnviv target pname params  glio) = hglTexEnviv target pname params >> glio

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

-- * GLIO aliases that apply the Fixed wrapper

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
