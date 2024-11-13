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
		"layout(location = 4) in int  elem;",
		"",
		"out      vec4 vertexModulateColor;",
		"out      vec2 vertexTexCoords;",
		"out flat int  vertexTexLayer;",
		"",
		"layout(location = 16) uniform int enableSceneData;",
		"",
		"// 14 SOL render analysis arrays (scene data), locations 17-30",
		"layout(std430, binding = 17) buffer layoutSSBOVertexData {",
		"\tdouble ssboVertexData[];",
		"};",
		"layout(std430, binding = 18) buffer layoutSSBOGeomData {",
		"\tint ssboGeomData[];",
		"};",
		"layout(std430, binding = 19) buffer layoutSSBOLumpData {",
		"\tint ssboLumpData[];",
		"};",
		"layout(std430, binding = 20) buffer layoutSSBOPathDoublesData {",
		"\tdouble ssboPathDoublesData[];",
		"};",
		"layout(std430, binding = 21) buffer layoutSSBOPathInt32sData {",
		"\tdouble ssboPathInt32sData[];",
		"};",
		"layout(std430, binding = 22) buffer layoutSSBOBodyData {",
		"\tint ssboBodyData[];",
		"};",
		"layout(std430, binding = 23) buffer layoutSSBOGcData {",
		"\tint ssboGcData[];",
		"};",
		"layout(std430, binding = 24) buffer layoutSSBOAllGeomPassMvData {",
		"\tint ssboAllGeomPassMvData[];",
		"};",
		"layout(std430, binding = 25) buffer layoutSSBOAllGeomPassTexturesData {",
		"\tint ssboAllGeomPassTexturesData[];",
		"};",
		"layout(std430, binding = 26) buffer layoutSSBOAllGeomPassGisData {",
		"\tint ssboAllGeomPassGisData[];",
		"};",
		"layout(std430, binding = 27) buffer layoutSSBOGeomPassMvRangesData {",
		"\tint ssboGeomPassMvRangesData[];",
		"};",
		"layout(std430, binding = 28) buffer layoutSSBOGeomPassTexturesRangesData {",
		"\tint ssboGeomPassTexturesRangesData[];",
		"};",
		"layout(std430, binding = 29) buffer layoutSSBOGeomPassGisRangesData {",
		"\tint ssboGeomPassGisRangesData[];",
		"};",
		"layout(std430, binding = 30) buffer layoutSSBOGeomPassBisData {",
		"\tint ssboGeomPassBisData[];",
		"};",
		"",
		"layout(std430, binding = 31) buffer layoutSSBOTransformationData {",
		"\tdouble ssboTransformationData[];",
		"};",
		"",
		"layout(location = 32) uniform int sceneGeomPassIdx;",
		"",
		"// Required for linking with GL_PROGRAM_SEPARABLE apparently.",
		"out gl_PerVertex {",
		"\tvec4  gl_Position;",
		"\tfloat gl_PointSize;",
		"\tfloat gl_ClipDistance[];",
		"};",
		"",
		"void main() {",
		"\tif (!(enableSceneData != 0)) {",
		"\t\tgl_Position = vec4(position, 1.0);",
		"\t\tvertexModulateColor = modulateColor;",
		"\t\tvertexTexCoords = texCoords;",
		"\t\tvertexTexLayer = texLayer;",
		"\t} else {",
		"\t\t// Render elem ‘elem’ in the geom pass ‘sceneGeomPassIdx’.",
		"\t\tint geomPassTexture0 = layoutSSBOGeomPassTexturesRangesData[2*sceneGeomPassIdx+0];",
		"\t\tint geomPassTexturec = layoutSSBOGeomPassTexturesRangesData[2*sceneGeomPassIdx+1];",
		"\t\tint geomPassGi0      = layoutSSBOGeomPassGisRangesData[2*sceneGeomPassIdx+0];",
		"\t\tint geomPassGic      = layoutSSBOGeomPassGisRangesData[2*sceneGeomPassIdx+1];",
		"",
		"\t\tint texture = layoutSSBOGeomPassGisRangesData[2*sceneGeomPassIdx+1];",
		"\t\tint gi      = layoutSSBOGeomPassGisRangesData[2*sceneGeomPassIdx+1];",
		"",
		"\t\tint whichVert = elem % 3;",
		"",
		"\t\tint vi = layoutSSBOGeomData[9*gi+0];",
		"\t\tint vj = layoutSSBOGeomData[9*gi+1];",
		"\t\tint vk = layoutSSBOGeomData[9*gi+2];",
		"\t\tint ti = layoutSSBOGeomData[9*gi+3];",
		"\t\tint tj = layoutSSBOGeomData[9*gi+4];",
		"\t\tint tk = layoutSSBOGeomData[9*gi+5];",
		"\t\tint si = layoutSSBOGeomData[9*gi+6];",
		"\t\tint sj = layoutSSBOGeomData[9*gi+7];",
		"\t\tint sk = layoutSSBOGeomData[9*gi+8];",
		"",
		"\t\tint v = layoutSSBOGeomData[9*gi+0+(whichVert)];",
		"\t\tint t = layoutSSBOGeomData[9*gi+3+(whichVert)];",
		"\t\tint s = layoutSSBOGeomData[9*gi+6+(whichVert)];",
		"",
		"\t\tdouble vx = layoutSSBOVertexData[3*v+0];",
		"\t\tdouble vy = layoutSSBOVertexData[3*v+1];",
		"\t\tdouble vz = layoutSSBOVertexData[3*v+2];",
		"\t\tfloat vxf = float(vx);",
		"\t\tfloat vyf = float(vy);",
		"\t\tfloat vzf = float(vz);",
		"\t\tvec3 vf = vec3(vxf, vyf, vzf);",
		"\t\tvec4 out_gl_Position = vec4(position, 1.0);",
		"",
		"\t\tvec4 out_modulateColor = vec4(1.0, 1.0, 1.0, 1.0);",
		"",
		"\t\t// TODO: texture coords.",
		"\t\tvec2 out_texCoords = vec2(vxf, vyf);",
		"",
		"\t\tint out_texLayer = texture;",
		"",
		"\t\tgl_Position = out_gl_Position;",
		"\t\tvertexModulateColor = out_modulateColor;",
		"\t\tvertexTexCoords = texCoords;",
		"\t\tvertexTexLayer = texLayer;",
		"\t}",
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
