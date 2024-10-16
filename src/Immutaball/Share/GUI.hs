{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ExplicitForAll, InstanceSigs, ScopedTypeVariables, FlexibleContexts #-}

module Immutaball.Share.GUI
	(
		-- * widgets
		HasWid(..),
		HasWparent(..),
		HasFill(..),
		HasText(..),
		HasRect(..),
		Root(..), rootWid,
		Space(..), spaceWid, spaceWparent, spaceFill,
		Vstack(..), vstackWid, vstackWparent,
		Hstack(..), hstackWid, hstackWparent,
		Label(..), labelWid, labelWparent, labelText, labelRect,
		Button(..), buttonWid, buttonWparent, buttonText, buttonRect,
		Widget(..), AsWidget(..),

		-- * wires
		WidgetRequest(..), AsWidgetRequest(..),
		WidgetResponse(..), AsWidgetResponse(..),
		mkGUI,
		mkWidgetsAnalysis,
		mkWidgetBy,
		mkWidgetIdx,
		mkWidgetToIdx,
		mkGetChildren,
		mkGeometry,
		nextWidgetDirect,
		prevWidgetDirect,
		nextWidgetHier,
		prevWidgetHier,
		isSelectable,
		guiPaint,
		guiPaintWidgetsChunk,
		guiPaintWidgets,
		focusDecayTime,
		focusScale
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
--import Data.Functor.Identity
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Foreign.Storable (sizeOf, Storable)

import Control.Lens
--import Data.Array
import Data.Array.Base
--import Data.Array.MArray
import Data.Array.ST
import Data.Array.Storable
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import Graphics.GL.Compatibility45
--import Graphics.GL.Core45
import Graphics.GL.Types
import qualified SDL.Raw.Enum as Raw

import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.Math
import Immutaball.Share.SDLManager
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Video
import Immutaball.Share.Wire

-- * widgets

data Root id = Root {
	_rootWid :: id
}
	deriving (Eq, Ord, Show)
makeLenses ''Root
makeFields ''Root

data Space id = Space {
	_spaceWid     :: id,
	_spaceWparent :: id,

	-- | Max space to anchor at bottom.
	_spaceFill    :: Bool
}
	deriving (Eq, Ord, Show)
makeLenses ''Space
makeFields ''Space

instance HasWparent (Root id) id where wparent = wid

data Vstack id = Vstack {
	_vstackWid     :: id,
	_vstackWparent :: id
}
	deriving (Eq, Ord, Show)
makeLenses ''Vstack
makeFields ''Vstack

data Hstack id = Hstack {
	_hstackWid     :: id,
	_hstackWparent :: id
}
	deriving (Eq, Ord, Show)
makeLenses ''Hstack
makeFields ''Hstack

data Label id = Label {
	_labelWid     :: id,
	_labelWparent :: id,

	_labelText    :: String,
	_labelRect    :: Maybe (Rect Double)
}
	deriving (Eq, Ord, Show)
makeLenses ''Label
makeFields ''Label

data Button id = Button {
	_buttonWid     :: id,
	_buttonWparent :: id,

	_buttonText    :: String,
	_buttonRect    :: Maybe (Rect Double)
}
	deriving (Eq, Ord, Show)
makeLenses ''Button
makeFields ''Button

data Widget id =
	  RootWidget   (Root   id)
	| SpaceWidget  (Space  id)
	| VstackWidget (Vstack id)
	| HstackWidget (Hstack id)
	| LabelWidget  (Label  id)
	| ButtonWidget (Button id)
	deriving (Eq, Ord, Show)
makeClassyPrisms ''Widget

instance HasWid (Widget id) id where
	wid :: forall f. Functor f => (id -> f id) -> Widget id -> f (Widget id)
	wid accessor (RootWidget   root)   = RootWidget   <$> wid accessor root
	wid accessor (SpaceWidget  space)  = SpaceWidget  <$> wid accessor space
	wid accessor (VstackWidget vstack) = VstackWidget <$> wid accessor vstack
	wid accessor (HstackWidget hstack) = HstackWidget <$> wid accessor hstack
	wid accessor (LabelWidget  label)  = LabelWidget  <$> wid accessor label
	wid accessor (ButtonWidget button) = ButtonWidget <$> wid accessor button
instance HasWparent (Widget id) id where
	wparent :: forall f. Functor f => (id -> f id) -> Widget id -> f (Widget id)
	wparent accessor (RootWidget   root)   = RootWidget   <$> wparent accessor root
	wparent accessor (SpaceWidget  space)  = SpaceWidget  <$> wparent accessor space
	wparent accessor (VstackWidget vstack) = VstackWidget <$> wparent accessor vstack
	wparent accessor (HstackWidget hstack) = HstackWidget <$> wparent accessor hstack
	wparent accessor (LabelWidget  label)  = LabelWidget  <$> wparent accessor label
	wparent accessor (ButtonWidget button) = ButtonWidget <$> wparent accessor button

-- * wires

data WidgetRequest id =
	  GUIDrive Request
	| GUISetText id String
	| GUISetFocus id
	| ResetGUI [Widget id]
	deriving (Eq, Ord, Show)
makeClassyPrisms ''WidgetRequest

data WidgetResponse id =
	  NoWidgetAction
	| WidgetAction id
	deriving (Eq, Ord, Show)
makeClassyPrisms ''WidgetResponse

-- TODO: handle mouse input.
-- TODO: paint.
-- TODO: GUISetText, which clears old text cache if nobody else refers.

mkGUI :: forall id. (Eq id, Ord id) => [Widget id] -> Wire ImmutaballM (WidgetRequest id, IBStateContext) (WidgetResponse id, IBStateContext)
mkGUI initialWidgets = proc (request, cxtn) -> do
	-- Set up widgets.
	resetWidgets <- returnA -< const Nothing ||| Just $ matching _ResetGUI request
	widgets <- hold initialWidgets -< resetWidgets

	-- On reset, clear the text cache; we're the only user.
	cxtnp1 <- returnA ||| clearTextCache -< const cxtn +++ const cxtn $ matching _ResetGUI request
	-- On the first step, clear the text cache; we're the only user.
	isFirst <- delay True -< returnA False
	cxtnp2 <- returnA ||| clearTextCache -< if' (not isFirst) Left Right cxtnp1

	-- Analyze widgets.
	widgetsReq  <- returnA -< resetWidgets
	widgetBy    <- mkWidgetsAnalysis' mkWidgetBy    -< widgetsReq
	widgetIdx   <- mkWidgetsAnalysis' mkWidgetIdx   -< widgetsReq
	widgetToIdx <- mkWidgetsAnalysis' mkWidgetToIdx -< widgetsReq
	getChildren <- mkWidgetsAnalysis' mkGetChildren -< widgetsReq
	geometry    <- mkWidgetsAnalysis' mkGeometry    -< widgetsReq

	nextWidgetHier' <- returnA -< nextWidgetHier widgetBy getChildren
	prevWidgetHier' <- returnA -< prevWidgetHier widgetBy getChildren

	-- Manage focus.
	rec
		initialFocus <- returnA -< maybe 0 id $ flip M.lookup widgetIdx 0 >>= flip M.lookup widgetBy . nextWidgetHier' . prevWidgetHier' . (^.wid) >>= flip M.lookup widgetToIdx
		currentFocus <- holdWith -< (newFocus, initialFocus)
		lastFocus <- delayWith -< (currentFocus, initialFocus)
		newFocus <- case request of
			GUISetFocus wid_ -> returnA -< flip M.lookup widgetBy wid_ >>= flip M.lookup widgetToIdx
			GUIDrive (Keybd char True) -> returnA -<
				if' (char == fromIntegral Raw.SDLK_DOWN) (Just $ nextWidgetDirect widgetIdx lastFocus) .
				if' (char == fromIntegral Raw.SDLK_UP  ) (Just $ prevWidgetDirect widgetIdx lastFocus) $
				Nothing
			_ -> returnA -< Nothing

		-- Find 'widgetsFocusedSinceLastPaintIdx'.
		isPaint <- returnA -< case request of (GUIDrive (Paint _t)) -> True; _ -> False
		lastIsPaint <- delay False -< isPaint
		lastWidgetsFocusedSinceLastPaintIdx <- delay [] -< widgetsFocusedSinceLastPaintIdx
		(widgetsFocusedSinceLastPaintIdx :: [Integer]) <- returnA -<
			catMaybes [newFocus] ++ if' lastIsPaint [] lastWidgetsFocusedSinceLastPaintIdx
		(widgetsFocusedSinceLastPaint :: [id]) <- returnA -<
			flip mapMaybe widgetsFocusedSinceLastPaintIdx $ \idx -> (^.wid) <$> flip M.lookup widgetIdx idx

	-- Paint.
	currentFocusWid <- returnA -< fromMaybe (error "Internal error: mkGUI: currentFocus not in widgetIdx.") $ (^.wid) <$> flip M.lookup widgetIdx currentFocus
	cxtnp3 <- case request of
		GUIDrive (Paint t) -> guiPaint -< (widgets, geometry, widgetBy, widgetsFocusedSinceLastPaint, currentFocusWid, t, cxtnp2)
		_ -> returnA -< cxtnp2

	-- Set up response.
	response <- returnA -< case request of
		GUIDrive (Keybd char True) ->
			if' (char == fromIntegral Raw.SDLK_RETURN) (maybe NoWidgetAction id $ WidgetAction . (^.wid) <$> flip M.lookup widgetIdx currentFocus) $
			NoWidgetAction
		_ -> NoWidgetAction
	returnA -< (response, cxtnp3)
	where
		mkWidgetsAnalysis' = mkWidgetsAnalysis initialWidgets
		_warn :: String -> Wire ImmutaballM () ()
		_warn msg = proc () -> do
			() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn msg ()
			returnA -< ()

-- | Only recalculates the analysis on reset.
mkWidgetsAnalysis :: [Widget id] -> ([Widget id] -> a) -> Wire ImmutaballM (Maybe [Widget id]) a
mkWidgetsAnalysis initialWidgets analyzer = proc resetWidgets -> do
	hold (analyzer initialWidgets) -< analyzer <$> resetWidgets

mkWidgetBy :: (Eq id, Ord id) => [Widget id] -> M.Map id (Widget id)
mkWidgetBy widgets = M.fromList . flip map widgets $ \w -> ((w^.wid), w)

mkWidgetIdx :: [Widget id] -> M.Map Integer (Widget id)
mkWidgetIdx widgets = M.fromList $ zip [0..] widgets

mkWidgetToIdx :: (Eq id, Ord id) => [Widget id] -> M.Map (Widget id) Integer
mkWidgetToIdx widgets = M.fromList $ zip widgets [0..]

mkGetChildren :: (Eq id) => [Widget id] -> id -> [Widget id]
mkGetChildren widgets wid_ = filter (\w -> (w^.wid) == wid_) widgets

-- TODO: make a more sophisticated geometry.
-- For now I'll just specify the rect by hand.
mkGeometry :: forall id. (Eq id, Ord id) => [Widget id] -> M.Map id (Rect Double)
--mkGeometry widgets = M.fromList . flip mapMaybe widgets . (\(a,b)->(a,)<$>b) . (id &&&) $ \w -> case w of
mkGeometry widgets = M.fromList . map (first (^.wid)) . flip mapMaybe widgets . (uncurry fmap .) . ((,) &&&) $ \w -> case w of
	(LabelWidget label) -> label^.labelRect
	(ButtonWidget button) -> button^.buttonRect
	_ -> Nothing

-- | The order is the order reflected in the input widgets.
nextWidgetDirect :: (Eq id, Ord id) => M.Map Integer (Widget id) -> Integer -> Integer
nextWidgetDirect widgetIdx idx = tryNextWidget (idx+1)
	where tryNextWidget idx_
		| idx_ == idx = idx_
		| idx_ >= (fromIntegral $ M.size widgetIdx) = tryNextWidget 0
		| maybe False isSelectable $ M.lookup idx_ widgetIdx = idx_
		| otherwise = tryNextWidget (idx_+1)
prevWidgetDirect :: (Eq id, Ord id) => M.Map Integer (Widget id) -> Integer -> Integer
prevWidgetDirect widgetIdx idx = tryNextWidget (idx-1)
	where tryNextWidget idx_
		| idx_ == idx = idx_
		| idx_ < 0 = tryNextWidget ((fromIntegral $ M.size widgetIdx)-1)
		| maybe False isSelectable $ M.lookup idx_ widgetIdx = idx_
		| otherwise = tryNextWidget (idx_-1)

-- | The order is input order except container hierarchy takes precedence.
-- Does not check for cycles.
nextWidgetHier :: (Eq id, Ord id) => M.Map id (Widget id) -> (id -> [Widget id]) -> id -> id
nextWidgetHier widgetBy getChildren wid0 = maybe wid0 (nextWidgetUnder True (Just wid0) . (^.wparent)) $ M.lookup wid0 widgetBy
	where
		wBy = flip M.lookup widgetBy
		nextWidgetUnder isFirst mwid parent
			| mwid == Just wid0 = wid0
			| Nothing <- mwid
			, Just (rw@RootWidget {}) <- wBy parent =
				nextWidgetUnder isFirst (Just (rw^.wid)) parent
			| otherwise = withRemaining . maybe id (\wid_ -> drop 1 . dropWhile (\w -> (w^.wid) /= wid_)) mwid $ getChildren parent
			where
				withRemaining []                                = maybe wid0 (\parent_ -> nextWidgetUnder False (Just (parent_^.wid)) (parent_^.wparent)) $ wBy parent
				withRemaining (w:_remaining) | (w^.wid) == wid0 = w^.wid
				withRemaining (w:_remaining) | isSelectable w   = w^.wid
				withRemaining (w:_remaining) | otherwise        = nextWidgetUnder False Nothing (w^.wid)
prevWidgetHier :: (Eq id, Ord id) => M.Map id (Widget id) -> (id -> [Widget id]) -> id -> id
prevWidgetHier widgetBy getChildren wid0 = maybe wid0 (prevWidgetUnder True (Just wid0) . (^.wparent)) $ M.lookup wid0 widgetBy
	where
		wBy = flip M.lookup widgetBy
		prevWidgetUnder isFirst mwid parent
			| mwid == Just wid0 = wid0
			| Nothing <- mwid
			, Just (rw@RootWidget {}) <- wBy parent =
				prevWidgetUnder isFirst (Just (rw^.wid)) parent
			| otherwise = withRemaining . maybe id (\wid_ -> drop 1 . dropWhile (\w -> (w^.wid) /= wid_) . reverse) mwid $ getChildren parent
			where
				withRemaining []                                = maybe wid0 (\parent_ -> prevWidgetUnder False (Just (parent_^.wid)) (parent_^.wparent)) $ wBy parent
				withRemaining (w:_remaining) | (w^.wid) == wid0 = w^.wid
				withRemaining (w:_remaining) | isSelectable w   = w^.wid
				withRemaining (w:_remaining) | otherwise        = prevWidgetUnder False Nothing (w^.wid)

isSelectable :: Widget id -> Bool
isSelectable (ButtonWidget {}) = True
isSelectable _                 = False

guiPaint :: forall id. (Eq id, Ord id) => Wire ImmutaballM ([Widget id], M.Map id (Rect Double), M.Map id (Widget id), [id], id, Double, IBStateContext) IBStateContext
guiPaint = proc (widgets, geometry, widgetBy, widgetsFocusedSinceLastPaint, currentFocusWid, t, cxtn) -> do
	_dt <- differentiate -< t
	rec
		(widgetLastFocusLast :: M.Map id Double) <- delay M.empty -< widgetLastFocus
		widgetLastFocus <- returnA -< M.filter (> t - focusDecayTime) $ foldr (\wid_ -> M.insert wid_ t) widgetLastFocusLast widgetsFocusedSinceLastPaint

	chunks' <- returnA -< chunksOf (cxtn^.ibContext.ibStaticConfig.x'cfgMaxPassTextures)
	cxtnp1 <- foldrA guiPaintWidgetsChunk -< (cxtn, flip map (chunks' widgets) $ \ws -> (ws, widgetLastFocus, geometry, widgetBy, currentFocusWid, t))
	returnA -< cxtnp1

guiCachingRenderText :: forall id. (Eq id, Ord id) => Wire ImmutaballM (Widget id, IBStateContext) (Maybe (WidthHeightI, GLuint), IBStateContext)
guiCachingRenderText = proc (widget, cxtn) -> do
	let mtext = case widget of
		(ButtonWidget button) -> Just (button^.text)
		(LabelWidget  label)  -> Just (label^.text)
		_                     -> Nothing
	(mdimName, cxtnp1) <- returnA ||| (first Just) <$> cachingRenderText -< maybe (Left (Nothing, cxtn)) (\text_ -> Right (T.pack $ text_, cxtn)) $ mtext
	returnA -< (mdimName, cxtnp1)

-- | Just process 16 at a time, the number of textures our shader can handle at a time.
guiPaintWidgetsChunk :: forall id. (Eq id, Ord id) => Wire ImmutaballM (([Widget id], M.Map id Double, M.Map id (Rect Double), M.Map id (Widget id), id, Double), IBStateContext) IBStateContext
guiPaintWidgetsChunk = proc ((widgets, widgetLastFocus, geometry, widgetBy, currentFocusWid, t), cxtn) -> do
	-- let guiCacheRenderText' = guiCacheRenderText that accumulates on its left input.
	--guiCacheRenderText' <- returnA -< guiCacheRenderText …
	(mdimNames :: [Maybe (WidthHeightI, GLuint)], cxtnp1) <- foldrA guiCacheRenderText' -< (([], cxtn), widgets)
	let (mRects :: [Maybe (Rect Double)]) = flip map widgets $ \w -> M.lookup (w^.wid) geometry
	let (wids :: [Maybe id]) = flip map widgets $ \w -> Just (w^.wid)
	--let paintWidgets = catMaybes $ zipWith (liftA2 (,)) mdimNames mRects
	let paintWidgets = catMaybes . zipWith (liftA2 (,)) wids $ zipWith (liftA2 (,)) mdimNames mRects
	cxtnp2 <- guiPaintWidgets -< (paintWidgets, widgetLastFocus, widgetBy, currentFocusWid, t, cxtnp1)
	returnA -< cxtnp2
	where
		-- let guiCacheRenderText' = guiCacheRenderText that accumulates on its left input.
		guiCacheRenderText' = proc (w, (mdimNames, icxtn)) -> do
			(mdimName, icxtnp1) <- guiCachingRenderText -< (w, icxtn)
			returnA -< (mdimName : mdimNames, icxtnp1)

-- | OpenGL paint the GUI widgets.
--
-- BUild vertex data to upload to the GPU, set the texture name
-- shader-variables (aka uniforms), and then issue the GL draw commands.  The
-- higher order callbacks (aka shaders) we installed on the GPU will procses it.
guiPaintWidgets :: forall id. (Eq id, Ord id) => Wire ImmutaballM ([(id, ((WidthHeightI, GLuint), Rect Double))], M.Map id Double, M.Map id (Widget id), id, Double, IBStateContext) IBStateContext
guiPaintWidgets = proc (paintWidgets, widgetLastFocus, _widgetBy, currentFocusWid, t, cxtn) -> do
	-- let sdlGL1' = sdlGL1 h
	sdlGL1' <- returnA -< liftIBIO . sdlGL1 (cxtn^.ibContext.ibSDLManagerHandle)

	-- Animate grow and shrinking on focus.
	let widgetLastFocus' = M.filter (> t - focusDecayTime) $ widgetLastFocus
	rec
		lastFocusGrow <- delay M.empty -< focusGrow
		focusGrow <- returnA -< M.union ((\paintTime -> lerp focusScale 1.00 (ilerp t (t + focusDecayTime) paintTime)) <$> widgetLastFocus') lastFocusGrow

	let wfocusScale wid_ = fromMaybe (1.00 :: Double) $ M.lookup wid_ focusGrow

	-- Shared info.
	let ( sd :: Integer) = fromIntegral $ sizeOf (0.0 :: GLdouble)
	let (_si :: Integer) = fromIntegral $ sizeOf (0   :: GLint)

	let fi = fromIntegral

	--let (vertexStride  :: Integer) = 9*sd + 1*si
	let (vertexStride  :: Integer) = 10*sd
	let (vertexStride' :: GLsizei) = fromIntegral vertexStride

	let (vertexStrideByDoubles :: Integer) = 10

	-- Build vertex data (and element data) to upload to the GPU.

	-- Note that while most of the elements are 8-byte doubles, we want the
	-- texture layers to be interpreted as 4-byte ints, so when constructing these
	-- arrays we do a cast to build the texture layer ints.
	let numWidgets  = genericLength (paintWidgets)
	let trianglesPerWidget = 3
	let verticesPerWidget = 2 * trianglesPerWidget
	let numVertices = numWidgets * verticesPerWidget
	let (vertexArray  :: UArray Integer GLdouble) = runSTUArray $ do
		array_ <- newArray (0, numVertices * vertexStrideByDoubles - 1) 0.0
		when (numWidgets > 0) $ do
			arrayAsGLuint <- castSTUArray' array_
			-- Specify the element type for arrayAsGLuint
			(_firstElem :: GLuint) <- readArray arrayAsGLuint 0

			-- Write the array elements for each widget.
			-- > "layout(location = 0) in vec3 position;",
			-- > "layout(location = 1) in vec4 modulateColor;",
			-- > "layout(location = 2) in vec2 texCoords;",
			-- > "layout(location = 3) in int  texLayer;",
			forM_ (zip [0..] paintWidgets) $ \(idx, (wid_, ((((_w, _h), _texture)), wrect))) -> do
				let (Vec3 vp1 vp2 vp3, Vec3 vp4 vp5 vp6) = rectToTriangles $ wrect & rectAvgSideAboutCenter %~ (wfocusScale wid_ *)
				let (Vec3 vt1 vt2 vt3, Vec3 vt4 vt5 vt6) = rectToTriangles $ (Rect (Vec2 0.0 0.0) (Vec2 1.1 1.1))

				let withVertex vp vt vidx = do
					-- Position.
					writeArray array_ (vidx*10 + 0) $ vp^.x2
					writeArray array_ (vidx*10 + 1) $ vp^.y2
					writeArray array_ (vidx*10 + 2) $ 0.0
					-- Color.
					if wid_ /= currentFocusWid
						then do
							writeArray array_ (vidx*10 + 3) $ 0.3
							writeArray array_ (vidx*10 + 4) $ 0.3
							writeArray array_ (vidx*10 + 5) $ 0.7
							writeArray array_ (vidx*10 + 6) $ 1.0
						else do
							writeArray array_ (vidx*10 + 3) $ 0.6
							writeArray array_ (vidx*10 + 4) $ 0.6
							writeArray array_ (vidx*10 + 5) $ 0.9
							writeArray array_ (vidx*10 + 6) $ 1.0
					-- Tex coords.
					writeArray array_ (vidx*10 + 7) $ vt^.x2
					writeArray array_ (vidx*10 + 8) $ vt^.y2
					-- Tex layer (double indices since from this view elements are half as long).
					-- (See note near glActiveTexture on the valeu OpenGL uses.)
					--writeArray arrayAsGLuint (2*(vidx*10 + 9) + 0) $ texture
					writeArray arrayAsGLuint (2*(vidx*10 + 9) + 0) $ (fromIntegral idx  :: GLuint)

				withVertex vp1 vt1 (verticesPerWidget * idx + 0)
				withVertex vp2 vt2 (verticesPerWidget * idx + 1)
				withVertex vp3 vt3 (verticesPerWidget * idx + 2)
				withVertex vp4 vt4 (verticesPerWidget * idx + 3)
				withVertex vp5 vt5 (verticesPerWidget * idx + 4)
				withVertex vp6 vt6 (verticesPerWidget * idx + 5)
		return array_
	--let (elementArray :: UArray Integer GLuint)   = runSTUArray $ do
	let (elementArray :: UArray Integer GLuint)   =
		Data.Array.Base.listArray (0, numVertices - 1) [0..(fromIntegral numVertices) - 1]

	(vertexStorableArray  :: StorableArray Integer GLdouble) <- monadic -< liftIBIO $ ThawIO (vertexArray  :: UArray Integer GLdouble) id
	(elementStorableArray :: StorableArray Integer GLuint)   <- monadic -< liftIBIO $ ThawIO (elementArray :: UArray Integer GLuint)   id

	(vertexData  :: GLData) <- monadic -< liftIBIO $ bsToGLData <$> ArrayToBS vertexStorableArray id
	(elementData :: GLData) <- monadic -< liftIBIO $ bsToGLData <$> ArrayToBS elementStorableArray id
	let (numElements_ :: Integer) = fromIntegral $ numElements (elementArray :: UArray Integer GLuint)

	() <- monadic -< sdlGL1' $ do
		-- First set the 16 texture name uniforms, and make them active.
		forM_ (zip [0..] $ map (^._2._1._2) paintWidgets) $ \(idx, texture) -> do
			case flip M.lookup numToGL_TEXTUREi idx of
				Nothing -> return ()
				Just gl_TEXTUREi -> do
					-- Apparently actually we set GL_TEXTUREi for the texture()
					-- GLSL call, not the texture name.  So set the i used in
					-- GL_TEXTUREi as the value, not the texture identifier.
					--GLUniform1i (fromIntegral idx) (fromIntegral texture) ()
					GLUniform1i (fromIntegral idx) (fromIntegral idx) ()

					--GLActiveTexture GL_TEXTUREi ()
					GLActiveTexture gl_TEXTUREi ()
					GLBindTexture GL_TEXTURE_2D texture ()

		-- Now upload the vertex data and draw.
		vao        <- unSingleton <$> GLGenVertexArrays 1 id
		vertexBuf  <- unSingleton <$> GLGenBuffers      1 id
		elementBuf <- unSingleton <$> GLGenBuffers      1 id

		GLBindVertexArray vao ()

		GLBindBuffer GL_ARRAY_BUFFER vertexBuf                  ()
		GLBufferData GL_ARRAY_BUFFER vertexData GL_DYNAMIC_DRAW ()

		GLBindBuffer GL_ELEMENT_ARRAY_BUFFER elementBuf                  ()
		GLBufferData GL_ELEMENT_ARRAY_BUFFER elementData GL_DYNAMIC_DRAW ()

		-- … stride offset ()

		-- location 0: vec3 positions.
		GLVertexAttribPointer 0 3 GL_DOUBLE GL_FALSE vertexStride' (fi$sum[]*sd) ()
		GLEnableVertexAttribArray 0 ()

		-- location 1: vec4 modulateColors.
		GLVertexAttribPointer 1 4 GL_DOUBLE GL_FALSE vertexStride' (fi$sum[3]*sd) ()
		GLEnableVertexAttribArray 1 ()

		-- location 2: vec2 texCoords.
		GLVertexAttribPointer 2 2 GL_DOUBLE GL_FALSE vertexStride' (fi$sum[3,4]*sd) ()
		GLEnableVertexAttribArray 2 ()

		-- location 3: vec1 texLayers.
		GLVertexAttribIPointer 3 1 GL_INT vertexStride' (fi$sum[3,4,2]*sd) ()
		GLEnableVertexAttribArray 3 ()

		GLDrawElementsRaw GL_TRIANGLES (fromIntegral numElements_) GL_UNSIGNED_INT 0 ()

		GLDeleteBuffers      [vertexBuf]  ()
		GLDeleteBuffers      [elementBuf] ()
		GLDeleteVertexArrays [vao]        ()

	returnA -< cxtn
	where
		unSingleton [me] = me
		unSingleton _    = error "Internal error: guiPaintWidgets expected a single result from GLGenVertexArrays or GLGenBuffers."
		rectToTriangles :: Rect Double -> (Vec3 (Vec2 Double), Vec3 (Vec2 Double))
		rectToTriangles r = (Vec3 (r'^.rectLowerLeft) (r'^.rectLowerRight) (r'^.rectUpperRight), Vec3 (r'^.rectLowerLeft) (r'^.rectUpperLeft) (r'^.rectUpperRight))
			where r' = rectNormalize r

		-- | 'castSTUArray' doesn't update the range.
		-- This version does.  Thus it prevents out of index errors when casting from one array type to another with a different element size.
		-- This version only handles single-dimensional indices.
		castSTUArray' :: forall ix a b s. (Num ix, Integral ix, Storable a, Storable b) => STUArray s ix a -> ST s (STUArray s ix b)
		castSTUArray' (STUArray l u _n marr) = return (STUArray l' u' n' marr)
			where
				btm = error "Internal error: castSTUArray': sizeOf value accessed."
				len = u - l + 1
				len' = ((sa * len) + (sa-1)) `div` sb
					where
						sb = fromIntegral $ sizeOf (btm :: b)
						sa = fromIntegral $ sizeOf (btm :: a)
				-- l and u are actually lower and upper, not length and start.
				u' = l + len' - 1
				l' = l
				n' = fromIntegral $ len'

-- Optionally this could be moved to static config.
focusDecayTime :: Double
focusDecayTime = 0.25

focusScale :: Double
focusScale = 1.10
