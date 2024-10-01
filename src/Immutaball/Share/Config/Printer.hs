{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010, RankNTypes #-}

module Immutaball.Share.Config.Printer
	(
		boolToInt,
		showIntConf,
		showBoolConf,
		showStringConf,
		showNeverballrc
	) where

import Prelude ()
import Immutaball.Prelude

import Data.List
import Text.Printf

import Control.Lens

import Immutaball.Share.Config

boolToInt :: (Integral i) => Bool -> i
boolToInt True = fromIntegral (1 :: Integer)
boolToInt False = fromIntegral (0 :: Integer)

showIntConf :: (Integral i, Show i) => Getter Neverballrc i -> String -> Neverballrc -> String
showIntConf field key cfg = printf "%-25s %s" key $ show (cfg^.field)

showBoolConf :: Getter Neverballrc Bool -> String -> Neverballrc -> String
showBoolConf field key cfg = printf "%-25s %s" key $ show (boolToInt $ cfg^.field  :: Integer)

showStringConf :: Getter Neverballrc String -> String -> Neverballrc -> String
showStringConf field key cfg = printf "%-25s %s" key $ cfg^.field

showNeverballrc :: Neverballrc -> String
showNeverballrc cfg = intercalate "\n" . map ($ cfg) $
	[
		showBoolConf fullscreen  "fullscreen",
		showIntConf  display     "display",
		showIntConf  width       "width",
		showIntConf  height      "height",
		showBoolConf stereo      "stereo",
		showIntConf  camera      "camera",
		showBoolConf textures    "textures",
		showBoolConf reflection  "reflection",
		showBoolConf multisample "multisample",
		showBoolConf mipmap      "mipmap",
		showIntConf  aniso       "aniso",
		showBoolConf background  "background",
		showBoolConf shadow      "shadow",
		showIntConf  audioBuff   "audio_buff",
		showIntConf  mouseSense  "mouse_sense",
		showIntConf  mouseResp   "mouse_response",
		showBoolConf mouseInvert "mouse_invert",
		showBoolConf mouseVsync  "vsync",
		showBoolConf hmd         "hmd",
		showBoolConf highdpi     "highdpi",

		showIntConf  mouseCamera1      "mouse_camera_1",
		showIntConf  mouseCamera2      "mouse_camera_1",
		showIntConf  mouseCameraToggle "mouse_camera_toggle",
		showIntConf  mouseCameraL      "mouse_camera_l",
		showIntConf  mouseCameraR      "mouse_camera_r",

		showIntConf  nice        "nice",
		showIntConf  fps         "fps",
		showIntConf  soundVolume "sound_volume",
		showIntConf  musicVolume "music_volume",

		showBoolConf joystick             "joystick",
		showIntConf  joystickResp         "joystick_response",
		showIntConf  joystickAxisX0       "joystick_axis_x0",
		showIntConf  joystickAxisY0       "joystick_axis_x1",
		showIntConf  joystickAxisX1       "joystick_axis_y0",
		showIntConf  joystickAxisY1       "joystick_axis_y1",
		showBoolConf joystickAxisX0Invert "joystick_axis_x0",
		showBoolConf joystickAxisY0Invert "joystick_axis_x1",
		showBoolConf joystickAxisX1Invert "joystick_axis_y0",
		showBoolConf joystickAxisY1Invert "joystick_axis_y1",

		showIntConf  joystickButtonA      "joystick_button_a",
		showIntConf  joystickButtonB      "joystick_button_b",
		showIntConf  joystickButtonX      "joystick_button_x",
		showIntConf  joystickButtonY      "joystick_button_y",
		showIntConf  joystickButtonL1     "joystick_button_l1",
		showIntConf  joystickButtonL2     "joystick_button_r1",
		showIntConf  joystickButtonR1     "joystick_button_l2",
		showIntConf  joystickButtonR2     "joystick_button_r2",
		showIntConf  joystickButtonSelect "joystick_button_select",
		showIntConf  joystickButtonStart  "joystick_button_start",
		showIntConf  joystickDpadL        "joystick_dpad_l",
		showIntConf  joystickDpadR        "joystick_dpad_r",
		showIntConf  joystickDpadU        "joystick_dpad_u",
		showIntConf  joystickDpadD        "joystick_dpad_d",

		showBoolConf wiimoteInvertPitch  "wiimote_invert_pitch",
		showBoolConf wiimoteInvertRoll   "wiimote_invert_roll",
		showIntConf  wiimotePitchSens    "wiimote_pitch_sensitivity",
		showIntConf  wiimoteRollSense    "wiimote_roll_sensitivity",
		showIntConf  wiimoteSmoothAlpha  "wiimote_smooth_alpha",
		showBoolConf wiimoteHoldSideways "wiimote_hold_sideways",

		showIntConf  keyCamera1      "key_camera_1",
		showIntConf  keyCamera2      "key_camera_2",
		showIntConf  keyCamera3      "key_camera_3",
		showIntConf  keyCameraToggle "key_camera_toggle",
		showIntConf  keyCameraR      "key_camera_r",
		showIntConf  keyCameraL      "key_camera_l",
		showIntConf  keyForward      "key_forward",
		showIntConf  keyBackward     "key_backward",
		showIntConf  keyLeft         "key_left",
		showIntConf  keyRight        "key_right",
		showIntConf  keyRestart      "key_restart",
		showIntConf  keyScoreNext    "key_score_next",
		showIntConf  keyRotateFast   "key_rotate_fast",

		showIntConf  viewFov    "view_fov",
		showIntConf  viewDp     "view_dp",
		showIntConf  viewDc     "view_dc",
		showIntConf  viewDz     "view_dz",
		showIntConf  rotateFast "rotate_fast",
		showIntConf  rotateSlow "rotate_slow",
		showBoolConf cheat      "cheat",
		showBoolConf stats      "stats",
		showBoolConf screenshot "screenshot",
		showBoolConf lockGoals  "lock_goals",

		showIntConf  camera1Speed "camera_1_speed",
		showIntConf  camera2Speed "camera_2_speed",
		showIntConf  camera3Speed "camera_3_speed",

		showIntConf  cameraTouchRotate "touch_rotate",

		showStringConf player      "player",
		showStringConf ballFile    "ball_file",
		showStringConf wiimoteAddr "wiimote_addr",
		showStringConf replayName  "replay_name",
		showStringConf language    "language",
		showStringConf theme       "theme"
	]
