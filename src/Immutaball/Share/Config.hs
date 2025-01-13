{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Config
	(
		StaticConfig'(..), minClockPeriod, maxClockPeriod, maxFrameEvents,
			allowWireForks, maxStepFrameSize, maxResponseFrameSize,
			defaultStaticDataDir, defaultUserDataDir, defaultUserConfigDir,
			configFilename, x'cfgInitialWireWithCxt, immutaballFont,
			immutaballFontSize, sdlManagerStaticConfig, glManagerStaticConfig,
			x'cfgUseExistingSDLManager, x'cfgUseExistingGLManager,
			x'cfgMaxPassTextures, x'cfgPrecacheMtrls, x'cfgPrecacheMisc,
			x'cfgBallTriangles, x'cfgDebugFreeCamera, x'cfgVertUpKey,
			x'cfgVertDownKey, x'cfgFreeCameraToggleKey,
		defaultStaticConfig,
		Neverballrc,
		Config(..), fullscreen, display, width, height, stereo, camera,
			textures, reflection, multisample, mipmap, aniso, background,
			shadow, audioBuff, mouseSense, mouseResp, mouseInvert, mouseVsync,
			hmd, highdpi, mouseCamera1, mouseCamera2, mouseCameraToggle,
			mouseCameraL, mouseCameraR, nice, fps, soundVolume, musicVolume,
			joystick, joystickResp, joystickAxisX0, joystickAxisY0,
			joystickAxisX1, joystickAxisY1, joystickAxisX0Invert,
			joystickAxisY0Invert, joystickAxisX1Invert, joystickAxisY1Invert,
			joystickButtonA, joystickButtonB, joystickButtonX, joystickButtonY,
			joystickButtonL1, joystickButtonL2, joystickButtonR1,
			joystickButtonR2, joystickButtonSelect, joystickButtonStart,
			joystickDpadL, joystickDpadR, joystickDpadU, joystickDpadD,
			wiimoteInvertPitch, wiimoteInvertRoll, wiimotePitchSens,
			wiimoteRollSense, wiimoteSmoothAlpha, wiimoteHoldSideways,
			keyCamera1, keyCamera2, keyCamera3, keyCameraToggle, keyCameraR,
			keyCameraL, keyForward, keyBackward, keyLeft, keyRight, keyRestart,
			keyScoreNext, keyRotateFast, viewFov, viewDp, viewDc, viewDz,
			rotateFast, rotateSlow, cheat, stats, screenshot, lockGoals,
			camera1Speed, camera2Speed, camera3Speed, cameraTouchRotate,
			player, ballFile, wiimoteAddr, replayName, language, theme,
		defaultConfig
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens
import qualified SDL.Raw.Enum as Raw

import Immutaball.Share.ImmutaballIO.DirectoryIO
import Immutaball.Share.GLManager
import Immutaball.Share.GLManager.Config
import Immutaball.Share.SDLManager
import Immutaball.Share.SDLManager.Config

data StaticConfig' initialWireWithCxt = StaticConfig {
	-- | Maximum FPS, including non-clock SDL events.
	_minClockPeriod :: Maybe Double,
	-- | If at least one non-clock event has been stepped, if more SDL
	-- events are available, delay processing those events if at least this
	-- much time has passed.  This might also be known as ‘maxEventPeriod’ with
	-- respect to clock steps.
	_maxClockPeriod :: Maybe Double,
	-- | If a single frame has more than this limit events, drop all after the
	-- first limit tunmber.
	_maxFrameEvents :: Maybe Integer,
	-- | Drop step requests after this many requests if they are all given
	-- at once.  Also do not send more than this many requests in one step.
	_maxStepFrameSize :: Maybe Integer,
	-- | Drop step responses after this many responses if they are all
	-- at once.  Also do not send more than this many responses in one
	-- step.  The wire may opt to priority queue responses exceeding the
	-- capacity if there's a response it doesn't want to drop.
	_maxResponseFrameSize :: Maybe Integer,
	_allowWireForks :: Bool,

	_defaultStaticDataDir :: Either (DirectoryIOF FilePath) FilePath,
	_defaultUserDataDir   :: Either (DirectoryIOF FilePath) FilePath,
	_defaultUserConfigDir :: Either (DirectoryIOF FilePath) FilePath,

	_configFilename :: FilePath,

	_x'cfgInitialWireWithCxt :: Maybe initialWireWithCxt,

	-- | Relative to the static data dir.
	_immutaballFont     :: FilePath,
	_immutaballFontSize :: Integer,

	-- | Use separate records to allow these modules to have their own static
	-- config.
	_sdlManagerStaticConfig :: SDLManagerStaticConfig,
	_glManagerStaticConfig  :: GLManagerStaticConfig,

	-- | Allow tests to share a single manager.
	_x'cfgUseExistingSDLManager :: Maybe SDLManagerHandle,
	_x'cfgUseExistingGLManager  :: Maybe GLManagerHandle,

	-- | How many textures can we use at once in a single call to
	-- 'glDrawElements'?
	_x'cfgMaxPassTextures :: Integer,

	-- | Request a thread is spawned in the background to precache Mtrl.
	_x'cfgPrecacheMtrls :: Bool,
	-- | Request a thread is spawned in the background to do misc precaching
	-- when setting up allocated textures.
	_x'cfgPrecacheMisc :: Bool,

	_x'cfgBallTriangles :: Integer,

	-- | Whether to allow entering debug free camera mode.
	_x'cfgDebugFreeCamera :: Bool,

	-- | Debug free camera: move vertically up and down (e.g. spacebar and c as in jump and crouch).
	_x'cfgVertUpKey           :: Int,
	_x'cfgVertDownKey         :: Int,
	_x'cfgFreeCameraToggleKey :: Int
}
makeLenses ''StaticConfig'

-- | Default static config.
defaultStaticConfig :: StaticConfig' a
defaultStaticConfig = StaticConfig {
	_minClockPeriod       = Just 0.001,  -- ^ Max FPS: 1000.
	_maxClockPeriod       = Just 0.5,    -- ^ Don't let external events block the clock for more than 500ms.
	_maxFrameEvents       = Nothing,
	_maxStepFrameSize     = Just 8,      -- ^ Don't request more than 8 at a time.
	_maxResponseFrameSize = Nothing,     -- ^ Don't request more than 8 at a time.
	_allowWireForks       = False,       -- ^ Don't let immutaball signals fork.

	_defaultStaticDataDir = Right "./data",
	_defaultUserDataDir   = Left $ GetXdgDirectoryDataSync "immutaball" id,
	_defaultUserConfigDir = Left $ GetXdgDirectoryConfigSync "immutaball" id,

	_configFilename          = "neverballrc",
	_x'cfgInitialWireWithCxt = Nothing,

	_immutaballFont     = "ttf/DejaVuSans-Bold.ttf",
	_immutaballFontSize = 256,

	_sdlManagerStaticConfig = defaultSDLManagerStaticConfig,
	_glManagerStaticConfig  = defaultGLManagerStaticConfig,

	_x'cfgUseExistingSDLManager = Nothing,
	_x'cfgUseExistingGLManager  = Nothing,

	_x'cfgMaxPassTextures = 16,

	_x'cfgPrecacheMtrls = True,
	_x'cfgPrecacheMisc  = True,

	_x'cfgBallTriangles = 8 * 64,  -- 512

	_x'cfgDebugFreeCamera = False,

	_x'cfgVertUpKey   = fromIntegral Raw.SDLK_SPACE,
	_x'cfgVertDownKey = fromIntegral Raw.SDLK_c,
	_x'cfgFreeCameraToggleKey = fromIntegral Raw.SDLK_f
}

type Neverballrc = Config
data Config = Config {
	_fullscreen  :: Bool,  -- "fullscreen"     False
	_display     :: Int,   -- "display"        0
	_width       :: Int,   -- "width"          800
	_height      :: Int,   -- "height"         600
	_stereo      :: Bool,  -- "stereo"         False
	_camera      :: Int,   -- "camera"         0
	_textures    :: Bool,  -- "textures"       True
	_reflection  :: Bool,  -- "reflection"     True
	_multisample :: Bool,  -- "multisample"    False
	_mipmap      :: Bool,  -- "mipmap"         True
	_aniso       :: Int,   -- "aniso"          8
	_background  :: Bool,  -- "background"     True
	_shadow      :: Bool,  -- "shadow"         True
	_audioBuff   :: Int,   -- "audio_buff"     2048
	_mouseSense  :: Int,   -- "mouse_sense"    444
	_mouseResp   :: Int,   -- "mouse_response" 50
	_mouseInvert :: Bool,  -- "mouse_invert"   False
	_mouseVsync  :: Bool,  -- "vsync"          True
	_hmd         :: Bool,  -- "hmd"            False
	_highdpi     :: Bool,  -- "highdpi"        True

	_mouseCamera1      :: Int,   -- "mouse_camera_1"      0
	_mouseCamera2      :: Int,   -- "mouse_camera_1"      0
	_mouseCameraToggle :: Int,   -- "mouse_camera_toggle" 2  -- SDL_BUTTON_MIDDLE
	_mouseCameraL      :: Int,   -- "mouse_camera_l"      1  -- SDL_BUTTON_LEFT
	_mouseCameraR      :: Int,   -- "mouse_camera_r"      3  -- SDL_BUTTON_RIGHT

	_nice        :: Int,  -- "nice"         0
	_fps         :: Int,  -- "fps"          0
	_soundVolume :: Int,  -- "sound_volume" 10
	_musicVolume :: Int,  -- "music_volume" 6

	_joystick             :: Bool,  -- "joystick"          True
	_joystickResp         :: Int,   -- "joystick_response" 50
	_joystickAxisX0       :: Int,   -- "joystick_axis_x0"  0
	_joystickAxisY0       :: Int,   -- "joystick_axis_x1"  1
	_joystickAxisX1       :: Int,   -- "joystick_axis_y0"  3
	_joystickAxisY1       :: Int,   -- "joystick_axis_y1"  4
	_joystickAxisX0Invert :: Bool,  -- "joystick_axis_x0"  False
	_joystickAxisY0Invert :: Bool,  -- "joystick_axis_x1"  False
	_joystickAxisX1Invert :: Bool,  -- "joystick_axis_y0"  False
	_joystickAxisY1Invert :: Bool,  -- "joystick_axis_y1"  False

	_joystickButtonA      :: Int,  -- "joystick_button_a"      0
	_joystickButtonB      :: Int,  -- "joystick_button_b"      1
	_joystickButtonX      :: Int,  -- "joystick_button_x"      2
	_joystickButtonY      :: Int,  -- "joystick_button_y"      3
	_joystickButtonL1     :: Int,  -- "joystick_button_l1"     4
	_joystickButtonL2     :: Int,  -- "joystick_button_r1"     5
	_joystickButtonR1     :: Int,  -- "joystick_button_l2"     -1
	_joystickButtonR2     :: Int,  -- "joystick_button_r2"     -1
	_joystickButtonSelect :: Int,  -- "joystick_button_select" 6
	_joystickButtonStart  :: Int,  -- "joystick_button_start"  7
	_joystickDpadL        :: Int,  -- "joystick_dpad_l"        8
	_joystickDpadR        :: Int,  -- "joystick_dpad_r"        9
	_joystickDpadU        :: Int,  -- "joystick_dpad_u"        10
	_joystickDpadD        :: Int,  -- "joystick_dpad_d"        11

	_wiimoteInvertPitch  :: Bool,  -- "wiimote_invert_pitch"      False
	_wiimoteInvertRoll   :: Bool,  -- "wiimote_invert_roll"       False
	_wiimotePitchSens    :: Int,   -- "wiimote_pitch_sensitivity" 100
	_wiimoteRollSense    :: Int,   -- "wiimote_roll_sensitivity"  100
	_wiimoteSmoothAlpha  :: Int,   -- "wiimote_smooth_alpha"      50
	_wiimoteHoldSideways :: Bool,  -- "wiimote_hold_sideways"     False

	_keyCamera1      :: Int,  -- "key_camera_1"      0x31        -- SDLK_1
	_keyCamera2      :: Int,  -- "key_camera_2"      0x32        -- SDLK_2
	_keyCamera3      :: Int,  -- "key_camera_3"      0x33        -- SDLK_3
	_keyCameraToggle :: Int,  -- "key_camera_toggle" 0x65        -- SDLK_e
	_keyCameraR      :: Int,  -- "key_camera_r"      0x64        -- SDLK_d
	_keyCameraL      :: Int,  -- "key_camera_l"      0x6C        -- SDLK_s
	_keyForward      :: Int,  -- "key_forward"       0x40000052  -- SDLK_UP
	_keyBackward     :: Int,  -- "key_backward"      0x40000051  -- SDLK_DOWN
	_keyLeft         :: Int,  -- "key_left"          0x40000050  -- SDLK_LEFT
	_keyRight        :: Int,  -- "key_right"         0x4000004F  -- SDLK_RIGHT
	_keyRestart      :: Int,  -- "key_restart"       0x72        -- SDLK_r
	_keyScoreNext    :: Int,  -- "key_score_next"    0x09        -- SDLK_TAB
	_keyRotateFast   :: Int,  -- "key_rotate_fast"   0x400000E1  -- SDLK_LSHIFT

	_viewFov    :: Int,   -- "view_fov"    50
	_viewDp     :: Int,   -- "view_dp"     75
	_viewDc     :: Int,   -- "view_dc"     25
	_viewDz     :: Int,   -- "view_dz"     200
	_rotateFast :: Int,   -- "rotate_fast" 300
	_rotateSlow :: Int,   -- "rotate_slow" 300
	_cheat      :: Bool,  -- "cheat"       False
	_stats      :: Bool,  -- "stats"       False
	_screenshot :: Bool,  -- "screenshot"  False
	_lockGoals  :: Bool,  -- "lock_goals"  True

	_camera1Speed :: Int,  -- "camera_1_speed" 250
	_camera2Speed :: Int,  -- "camera_2_speed" 0
	_camera3Speed :: Int,  -- "camera_3_speed" -1

	_cameraTouchRotate :: Int,  -- "touch_rotate" 16

	_player      :: String,  -- "player"       ""
	_ballFile    :: String,  -- "ball_file"    "ball/basic-ball/basic-ball"
	_wiimoteAddr :: String,  -- "wiimote_addr" ""
	_replayName  :: String,  -- "replay_name"  "%s-%l"
	_language    :: String,  -- "language"     ""
	_theme       :: String   -- "theme"        "classic"
}
	deriving (Eq, Ord, Show)
makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config {
	_fullscreen  = False,
	_display     = 0,
	_width       = 800,
	_height      = 600,
	_stereo      = False,
	_camera      = 0,
	_textures    = True,
	_reflection  = True,
	_multisample = False,
	_mipmap      = True,
	_aniso       = 8,
	_background  = True,
	_shadow      = True,
	_audioBuff   = 2048,
	_mouseSense  = 444,
	_mouseResp   = 50,
	_mouseInvert = False,
	_mouseVsync  = True,
	_hmd         = False,
	_highdpi     = True,

	_mouseCamera1      = 0,
	_mouseCamera2      = 0,
	_mouseCameraToggle = 2,  -- SDL_BUTTON_MIDDLE
	_mouseCameraL      = 1,  -- SDL_BUTTON_LEFT
	_mouseCameraR      = 3,  -- SDL_BUTTON_RIGHT

	_nice        = 0,
	_fps         = 0,
	_soundVolume = 10,
	_musicVolume = 6,

	_joystick             = True,
	_joystickResp         = 50,
	_joystickAxisX0       = 0,
	_joystickAxisY0       = 1,
	_joystickAxisX1       = 3,
	_joystickAxisY1       = 4,
	_joystickAxisX0Invert = False,
	_joystickAxisY0Invert = False,
	_joystickAxisX1Invert = False,
	_joystickAxisY1Invert = False,

	_joystickButtonA      = 0,
	_joystickButtonB      = 1,
	_joystickButtonX      = 2,
	_joystickButtonY      = 3,
	_joystickButtonL1     = 4,
	_joystickButtonL2     = 5,
	_joystickButtonR1     = -1,
	_joystickButtonR2     = -1,
	_joystickButtonSelect = 6,
	_joystickButtonStart  = 7,
	_joystickDpadL        = 8,
	_joystickDpadR        = 9,
	_joystickDpadU        = 10,
	_joystickDpadD        = 11,

	_wiimoteInvertPitch  = False,
	_wiimoteInvertRoll   = False,
	_wiimotePitchSens    = 100,
	_wiimoteRollSense    = 100,
	_wiimoteSmoothAlpha  = 50,
	_wiimoteHoldSideways = False,

	_keyCamera1      = 0x31,        -- SDLK_1
	_keyCamera2      = 0x32,        -- SDLK_2
	_keyCamera3      = 0x33,        -- SDLK_3
	_keyCameraToggle = 0x65,        -- SDLK_e
	_keyCameraR      = 0x64,        -- SDLK_d
	_keyCameraL      = 0x6C,        -- SDLK_s
	_keyForward      = 0x40000052,  -- SDLK_UP
	_keyBackward     = 0x40000051,  -- SDLK_DOWN
	_keyLeft         = 0x40000050,  -- SDLK_LEFT
	_keyRight        = 0x4000004F,  -- SDLK_RIGHT
	_keyRestart      = 0x72,        -- SDLK_r
	_keyScoreNext    = 0x09,        -- SDLK_TAB
	_keyRotateFast   = 0x400000E1,  -- SDLK_LSHIFT

	_viewFov    = 50,
	_viewDp     = 75,
	_viewDc     = 25,
	_viewDz     = 200,
	_rotateFast = 300,
	_rotateSlow = 300,
	_cheat      = False,
	_stats      = False,
	_screenshot = False,
	_lockGoals  = True,

	_camera1Speed = 250,
	_camera2Speed = 0,
	_camera3Speed = -1,

	_cameraTouchRotate = 16,

	_player      = "",
	_ballFile    = "ball/basic-ball/basic-ball",
	_wiimoteAddr = "",
	_replayName  = "%s-%l",
	_language    = "",
	_theme       = "classic"
}

-- TODO: print and parse.
