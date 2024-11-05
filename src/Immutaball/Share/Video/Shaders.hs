{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Video.Shaders
	(
		vertexShader,
		fragmentShader
	) where

import Prelude ()
import Immutaball.Prelude

vertexShader :: String
vertexShader = unlines $
	[
		"#version 450 core",
		"",
		"layout(location = 0) in vec3 position;",
		"layout(location = 1) in vec4 modulateColor;",
		"layout(location = 2) in vec2 texCoords;",
		"layout(location = 3) in int  texLayer;",
		"",
		"out      vec4 vertexModulateColor;",
		"out      vec2 vertexTexCoords;",
		"out flat int  vertexTexLayer;",
		"",
		"layout(location = 16) uniform int enableSceneData;",
		"",
		"// scene data; not used for the GUI, but for rendering levels; 2 arrays, texture indices and triangles.",
		"layout(std430, binding = 17) buffer layoutSSBOTextures {",
		"\tfloat ssboTextures[];",
		"};",
		"layout(std430, binding = 18) buffer layoutSSBOGis {",
		"\tfloat ssboGis[];",
		"};",
		"",
		"// Required for linking with GL_PROGRAM_SEPARABLE apparently.",
		"out gl_PerVertex {",
		"\tvec4  gl_Position;",
		"\tfloat gl_PointSize;",
		"\tfloat gl_ClipDistance[];",
		"};",
		"",
		"void main() {",
		"\tgl_Position = vec4(position, 1.0);",
		"\tvertexModulateColor = modulateColor;",
		"\tvertexTexCoords = texCoords;",
		"\tvertexTexLayer = texLayer;",
		"}",
		"",
		""
	]

fragmentShader :: String
fragmentShader = unlines $
	[
		"#version 450 core",
		"",
		"out vec4 fragmentColor;",
		"",
		"in      vec4 vertexModulateColor;",
		"in      vec2 vertexTexCoords;",
		"in flat int  vertexTexLayer;",
		"",
		"layout(location = 16) uniform int       enableSceneData;",
		"",
		"layout(location =  0) uniform sampler2D texture0;",
		"layout(location =  1) uniform sampler2D texture1;",
		"layout(location =  2) uniform sampler2D texture2;",
		"layout(location =  3) uniform sampler2D texture3;",
		"layout(location =  4) uniform sampler2D texture4;",
		"layout(location =  5) uniform sampler2D texture5;",
		"layout(location =  6) uniform sampler2D texture6;",
		"layout(location =  7) uniform sampler2D texture7;",
		"layout(location =  8) uniform sampler2D texture8;",
		"layout(location =  9) uniform sampler2D texture9;",
		"layout(location = 10) uniform sampler2D texture10;",
		"layout(location = 11) uniform sampler2D texture11;",
		"layout(location = 12) uniform sampler2D texture12;",
		"layout(location = 13) uniform sampler2D texture13;",
		"layout(location = 14) uniform sampler2D texture14;",
		"layout(location = 15) uniform sampler2D texture15;",
		"",
		"void main() {",
		--"\tfragmentColor = texture(texture0, vertexTexCoords);",
		"\tfragmentColor = ",
		"\t\tvertexTexLayer ==  0 ? texture(texture0,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  1 ? texture(texture1,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  2 ? texture(texture2,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  3 ? texture(texture3,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  4 ? texture(texture4,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  5 ? texture(texture5,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  6 ? texture(texture6,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  7 ? texture(texture7,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  8 ? texture(texture8,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  9 ? texture(texture9,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 10 ? texture(texture10, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 11 ? texture(texture11, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 12 ? texture(texture12, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 13 ? texture(texture13, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 14 ? texture(texture14, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 15 ? texture(texture15, vertexTexCoords) * vertexModulateColor :",
		"\t\tvec4(0.0, 0.0, 0.0, 0.0);",
		"}",
		"",
		""
	]
