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
		hglTexImage2D,
		hglGenTextures,
		hglDeleteTextures,

		-- * GLIO aliases that apply the Fixed wrapper
		mkGLClear,
		mkGLClearColor,
		mkGLTexImage2D,
		mkGLGenTextures,
		mkGLBindTexture,
		mkGLDeleteTextures,
		mkGLGetError
	) where

import Prelude ()
import Immutaball.Prelude

import Data.List
import Foreign.Ptr

import Graphics.GL.Core45
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
	| GLClearColor GLfloat GLfloat GLfloat GLfloat me

	-- | Set a texture.
	| GLTexImage2D GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum BL.ByteString me
	-- | Create a texture.
	| GLGenTextures GLsizei ([GLuint] -> me)
	-- | Set OpenGL current texture.
	| GLBindTexture GLenum GLuint me
	-- | Delete a texture.
	| GLDeleteTextures [GLuint] me
	| GLGetError (GLenum -> me)
instance Functor GLIOF where
	fmap :: (a -> b) -> (GLIOF a -> GLIOF b)
	fmap f (GLClear      mask_2               withUnit) = GLClear      mask_2               (f withUnit)
	fmap f (GLClearColor red green blue alpha withUnit) = GLClearColor red green blue alpha (f withUnit)

	fmap f (GLTexImage2D     target level internalformat width height border format type_ data_ withUnit) = GLTexImage2D target level internalformat width height border format type_ data_ (f withUnit)
	fmap f (GLGenTextures    numNames       withNames) = GLGenTextures    numNames       (f . withNames)
	fmap f (GLBindTexture    target texture withUnit)  = GLBindTexture    target texture (f withUnit)
	fmap f (GLDeleteTextures textures       withUnit)  = GLDeleteTextures textures       (f withUnit)
	fmap f (GLGetError                      withError) = GLGetError                      (f . withError)

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

-- * Runners

runGLIOIO :: GLIOF (IO ()) -> IO ()
runGLIOIO (GLClear          mask_2               glio) = glClear       mask_2               >> glio
runGLIOIO (GLClearColor     red green blue alpha glio) = glClearColor  red green blue alpha >> glio
runGLIOIO (GLTexImage2D     target level internalformat width height border format type_ data_ glio) = hglTexImage2D target level internalformat width height border format type_ data_ >> glio
runGLIOIO (GLGenTextures    numNames             withNames) = hglGenTextures numNames       >>= withNames
runGLIOIO (GLBindTexture    target texture       glio) = glBindTexture target texture       >> glio
runGLIOIO (GLDeleteTextures textures             glio) = hglDeleteTextures textures         >> glio
runGLIOIO (GLGetError                            withError) = glGetError                    >>= withError

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

-- * GLIO aliases that apply the Fixed wrapper

mkGLClear :: GLbitfield -> GLIO -> GLIO
mkGLClear mask_2 glio = Fixed $ GLClear mask_2 glio

mkGLClearColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLIO -> GLIO
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
