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
		"out flat int  doDiscard;",
		"",
		"// 3 modes: drawing GUI (false, false) drawing scene / level (true, false), and drawing ball (false, true)",
		"layout(location = 16) uniform int   enableSceneData;",
		"layout(location = 34) uniform int   enableBallData;",
		"layout(location = 35) uniform float ballRadius;",
		"layout(location = 36) uniform int   ballNumTriangles;",
		"layout(location = 37) uniform vec3  ballPos;",
		"layout(location = 38) uniform vec3  ballRot;",
		"",
		"// 16 SOL render analysis arrays (scene data), locations 17-32",
		"layout(std430, binding = 17) buffer layoutSSBOVertexData {",
		--"\tdouble ssboVertexData[];",
		"\t float ssboVertexData[];",
		"};",
		"layout(std430, binding = 18) buffer layoutSSBOGeomData {",
		"\tint ssboGeomData[];",
		"};",
		"layout(std430, binding = 19) buffer layoutSSBOLumpData {",
		"\tint ssboLumpData[];",
		"};",
		"layout(std430, binding = 20) buffer layoutSSBOPathDoublesData {",
		--"\tdouble ssboPathDoublesData[];",
		"\t float ssboPathDoublesData[];",
		"};",
		"layout(std430, binding = 21) buffer layoutSSBOPathInt32sData {",
		"\tint ssboPathInt32sData[];",
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
		--"\tdouble ssboTransformationData[];",
		"\t float ssboTransformationData[];",
		"};",
		"",
		"layout(std430, binding = 32) buffer layoutSSBOTexcoordsDoubleData {",
		--"\tdouble ssboTexcoordsDoubleData[];",
		"\t float ssboTexcoordsDoubleData[];",
		"};",
		"",
		"layout(location = 33) uniform int sceneGeomPassIdx;",
		"",
		"// Required for linking with GL_PROGRAM_SEPARABLE apparently.",
		"out gl_PerVertex {",
		"\tvec4  gl_Position;",
		"\tfloat gl_PointSize;",
		"\tfloat gl_ClipDistance[];",
		"};",
		"",
		--"vec4 rowMajorMatMulVecdf(double mat[16], vec4 untransformed) {",
		"vec4 rowMajorMatMulVecdf( float mat[16], vec4 untransformed) {",
		"\tfloat transformedx = float(mat[0]) * untransformed.x + float(mat[1]) * untransformed.y + float(mat[2]) * untransformed.z + float(mat[3]) * untransformed.w;",
		"\tfloat transformedy = float(mat[4]) * untransformed.x + float(mat[5]) * untransformed.y + float(mat[6]) * untransformed.z + float(mat[7]) * untransformed.w;",
		"\tfloat transformedz = float(mat[8]) * untransformed.x + float(mat[9]) * untransformed.y + float(mat[10]) * untransformed.z + float(mat[11]) * untransformed.w;",
		"\tfloat transformedw = float(mat[12]) * untransformed.x + float(mat[13]) * untransformed.y + float(mat[14]) * untransformed.z + float(mat[15]) * untransformed.w;",
		"\treturn vec4(transformedx, transformedy, transformedz, transformedw);",
		"}",
		"",
		"vec4 transfMul(vec4 untransformed) {",
		"\t//vec4 transformed = rowMajorMatMulVecdf(ssboTransformationData, untransformed);",
		"\tfloat transformedx = float(ssboTransformationData[0]) * untransformed.x + float(ssboTransformationData[1]) * untransformed.y + float(ssboTransformationData[2]) * untransformed.z + float(ssboTransformationData[3]) * untransformed.w;",
		"\tfloat transformedy = float(ssboTransformationData[4]) * untransformed.x + float(ssboTransformationData[5]) * untransformed.y + float(ssboTransformationData[6]) * untransformed.z + float(ssboTransformationData[7]) * untransformed.w;",
		"\tfloat transformedz = float(ssboTransformationData[8]) * untransformed.x + float(ssboTransformationData[9]) * untransformed.y + float(ssboTransformationData[10]) * untransformed.z + float(ssboTransformationData[11]) * untransformed.w;",
		"\tfloat transformedw = float(ssboTransformationData[12]) * untransformed.x + float(ssboTransformationData[13]) * untransformed.y + float(ssboTransformationData[14]) * untransformed.z + float(ssboTransformationData[15]) * untransformed.w;",
		"\tvec4 transformed = vec4(transformedx, transformedy, transformedz, transformedw);",
		"\treturn transformed;",
		"}",
		"",
		"void mainGUI() {",
		"\tgl_Position = vec4(position, 1.0);",
		"\tvertexModulateColor = modulateColor;",
		"\tvertexTexCoords = texCoords;",
		"\tvertexTexLayer = texLayer;",
		"\tdoDiscard = 0;",
		"}",
		"",
		"void mainScene() {",
		"\t// Render elem ‘elem’ in the geom pass ‘sceneGeomPassIdx’.",
		"\tint geomPassTexture0 = ssboGeomPassTexturesRangesData[2*sceneGeomPassIdx+0];",
		"\tint geomPassTexturec = ssboGeomPassTexturesRangesData[2*sceneGeomPassIdx+1];",
		"\tint geomPassGi0      = ssboGeomPassGisRangesData[2*sceneGeomPassIdx+0];",
		"\tint geomPassGic      = ssboGeomPassGisRangesData[2*sceneGeomPassIdx+1];",
		"",
		"\tint whichElem = elem / 3;",
		"\tint whichVert = elem % 3;",
		"",
		"\tint texture = ssboAllGeomPassTexturesData[( geomPassTexture0 + whichElem )];",
		"\tint gi      = ssboAllGeomPassGisData[(      geomPassGi0      + whichElem )];",
		"",
		"\tint vi = ssboGeomData[9*gi+0];",
		"\tint vj = ssboGeomData[9*gi+1];",
		"\tint vk = ssboGeomData[9*gi+2];",
		"\tint ti = ssboGeomData[9*gi+3];",
		"\tint tj = ssboGeomData[9*gi+4];",
		"\tint tk = ssboGeomData[9*gi+5];",
		"\tint si = ssboGeomData[9*gi+6];",
		"\tint sj = ssboGeomData[9*gi+7];",
		"\tint sk = ssboGeomData[9*gi+8];",
		"",
		"\tint v = ssboGeomData[9*gi+0+(whichVert)];",
		"\tint t = ssboGeomData[9*gi+3+(whichVert)];",
		"\tint s = ssboGeomData[9*gi+6+(whichVert)];",
		"",
		"\t// Texture coords.",
		--"\tdouble texs  = ssboTexcoordsDoubleData[2*t+0];",
		--"\tdouble text  = ssboTexcoordsDoubleData[2*t+1];",
		"\t float texs  = ssboTexcoordsDoubleData[2*t+0];",
		"\t float text  = ssboTexcoordsDoubleData[2*t+1];",
		"\tfloat  texsf = float(texs);",
		"\tfloat  textf = float(text);",
		"",
		--"\tdouble vx = ssboVertexData[3*v+0];",
		--"\tdouble vy = ssboVertexData[3*v+1];",
		--"\tdouble vz = ssboVertexData[3*v+2];",
		"\t float vx = ssboVertexData[3*v+0];",
		"\t float vy = ssboVertexData[3*v+1];",
		"\t float vz = ssboVertexData[3*v+2];",
		"\tfloat vxf = float(vx);",
		"\tfloat vyf = float(vy);",
		"\tfloat vzf = float(vz);",
		"\tvec3 vfbase = vec3(vxf, vyf, vzf);",
		"\tvec4 vfuntransformed = vec4(vfbase, 1.0);",
		"\tvec4 vf = transfMul(vfuntransformed);",
		"\tvec4 out_gl_Position = vf;",
		-- TODO DEBUG
		--"\tout_gl_Position = (!(gi > 50)) ? vf : (whichVert==0?vec4(vf.x,0.0,0.0,1.0):(whichVert==1?vec4(0.0,1.0,0.0,1.0):vec4(1.0,1.0,0.0,1.0)));",
		"",
		"\tvec4 out_modulateColor = vec4(1.0, 1.0, 1.0, 1.0);",
		"",
		"\tvec2 out_texCoords = vec2(texsf, textf);",
		"",
		"\tint out_texLayer = texture;",
		"",
		"\tgl_Position = out_gl_Position;",
		--"\tgl_Position = whichVert==0?vec4(vf.x,0.0,0.0,1.0):(whichVert==1?vec4(0.0,1.0,0.0,1.0):vec4(1.0,1.0,0.0,1.0));",  -- TODO DEBUG
		--"\tgl_Position = vfuntransformed;",  -- TODO DEBUG
		--"\tgl_Position = (sceneGeomPassIdx%3)==0?vec4(0.0,0.0,0.0,1.0):((sceneGeomPassIdx%3)==1?vec4(0.0,1.0,0.0,1.0):vec4(1.0,1.0,0.0,1.0));",  -- TODO DEBUG
		"\tvertexModulateColor = out_modulateColor;",
		"\tvertexTexCoords = out_texCoords;",
		"\tvertexTexLayer = out_texLayer;",
		"\tdoDiscard = 0;",
		"}",
		"",
		"// glDrawArrays is called with ballNumTriangles * 3, and elem is vertex to render.",
		"void mainBall() {",
		"\tint whichElem = elem / 3;",
		"\tint whichVert = elem % 3;  // Which of the 3 vertices of the triangle?",
		"",
		"\t// Now partition into squares and then triangles; consecutive",
		"\t// triangle pairs represent which of the 2 triangles of the square",
		"\t// to use.",
		--"\tint whichDivision = whichElem / 2;",
		--"\tint whichTriPart = whichElem % 2;  // First triangle or second triangle of the square?",
		"\tint whichSquare = whichElem / 2;",
		"\tint whichSquareTri = whichElem % 2;  // First triangle or second triangle of the square?",
		"",
		"\t// Make a circle on xy plane, then repeat with the xy plane tilted",
		"\t// until the tilt makes a full circle.",
		"\t// The number of circles equals the number of tilts.",
		"\tint squaresInOneCircle = int(round(sqrt(float(ballNumTriangles / 2)) - 0.6));",
		"\tint numTilts           = squaresInOneCircle;",
		"",
		"\t// 4 points for 2 triangles: inner p0, inner p1, outer p0, outer p1",
		"\t// (outer is 1 tilt further than inner, p1 is one sample in the circle",
		"\t// further than p0).",
		"\tint whichSquarePoint = whichVert + whichSquareTri;",
		"\tbool sp0 = whichSquarePoint == 0;",
		"\tbool sp1 = whichSquarePoint == 1;",
		"\tbool sp2 = whichSquarePoint == 2;",
		"\tbool sp3 = whichSquarePoint == 3;",
		"",
		"\tint onNextSample = (sp1 || sp3) ? 1 : 0;  // Is this point p1 and not p0?  (Advance the sample / partition index within the tilt by 1.)",
		"\tint onNextTilt   = (sp2 || sp3) ? 1 : 0;  // Is this point on the next tilt, rather than the current?",
		"",
		"\tconst float pi  = radians(180);",
		"\tconst float tau = radians(360);",
		"",
		"\tint whichCircleSample  = whichSquare % numTilts           + onNextSample;",
		"\tint whichTilt          = whichSquare / squaresInOneCircle + onNextTilt;",
		"\tfloat circleSample     = float(whichCircleSample) / float(squaresInOneCircle);  // 0.0 represents the start of the circle; 1.0 represents 360 degrees.",
		"\tfloat tilt             = float(whichTilt)         / float(numTilts);            // 0.0 represents the first tilt; 1.0 represents all tilts are done.",
		"\tfloat circleSampleRad_ = tau*circleSample;  // In radians.",
		"\tfloat tiltRad_         = tau*tilt;          // In radians.",
		"",
		"\tfloat circleSampleRad  = circleSampleRad_ + ballRot.z;  // rotatexy (aim right) - first transformation",
		"\tfloat spinRad          =                    ballRot.y;  // second transformation - only handles ball rotation about y axis.",
		"\tfloat tiltRad          = tiltRad_         + ballRot.x;  // rotateyz (aim down) - third transformation",
		"",
		"\t// Now calculate the raw vertex based on circleSample and tilt, all 3 axis rotations in fact, starting at radius 1 before we scale and apply pos and rot.",
		"\t// For the math, one way is to compose 3 rotatexx matrices.",
		"\t// rotateyz tiltRad <> rotatexz spinRad <> rotatexy circleSampleRad =",
		"\t//          (rotxy)",
		"\t//          c   s   0",
		"\t//          -s  c   0",
		"\t//          0   0   1",
		"\t// (rxz)",
		"\t// C  0  S  Cc  Cs  S",
		"\t// 0  1  0  -s  c   0",
		"\t// -S 0  C  -Sc -Ss C …",
		"",
		"\t//          (…)",
		"\t//          Cc  Cs  S",
		"\t//          -s  c   0",
		"\t//          -Sc -Ss C",
		"\t// (ryz)",
		"\t// 1  0  0  Cc  Cs  S",
		"\t// 0  č  š  (-čs-šSc) ( čc-šSs) (šC)",
		"\t// 0  -š č  ( šs-čSc) (-šc-čSs) (čC)",
		"\t//",
		"\t// Multiply the resulting matrix by the starting point 1,0,0 to get the raw position.",
		"\t// vec3 rawVertPos = Cc, -čs-šSc, šs-čSc",
		"\tfloat angleszc = cos(circleSampleRad);",
		"\tfloat angleszs = sin(circleSampleRad);",
		"\tfloat anglesyc = cos(spinRad);",
		"\tfloat anglesys = sin(spinRad);",
		"\tfloat anglesxc = cos(tiltRad);",
		"\tfloat anglesxs = sin(tiltRad);",
		-- TODO FIXME: monolith ball until the ball vec gets fixed.
		"\tvec3 rawVertPos = vec3(anglesyc * angleszc, -anglesxc*angleszs-anglesxs*anglesys*angleszc, anglesxs*angleszs-anglesxc*anglesys*angleszc);",
		--"\tvec3 rawVertPosDebugMonolith = vec3(circleSampleRad, 0.0, tiltRad);  // Can be used for debugging purposes to see a monolith 2D ball.",
		"\tvec3 vertPos = ballPos + ballRadius*rawVertPos;",
		"\t// Apply the global transformation.",
		"\tvec4 glVertPos = transfMul(vec4(vertPos, 1.0));",
		"",
		"\tbool blackNotWhite = int(round(4*circleSample)) % 2 == int(round(4*tilt)) % 2;",
		"\tvec4 ballTriangleColor = blackNotWhite ? vec4(0.0, 0.0, 0.0, 0.8) : vec4(1.0, 1.0, 1.0, 0.8);",
		"",
		"\t// Write output.",
		"\tgl_Position = glVertPos;",
		"\tvertexModulateColor = ballTriangleColor;",
		"\tvertexTexCoords = vec2(0.0, 0.0);  // Fragment shader will ignore.",
		"\tvertexTexLayer = 0;  // Fragment shader will ignore.",
		"\tdoDiscard = 0;",
		"}",
		"",
		"void mainUnknown() {",
		"\tgl_Position = vec4(0.0, 0.0, 0.0, 1.0);",
		"\tvertexModulateColor = vec4(0.0, 0.0, 0.0, 0.0);",
		"\tvertexTexCoords = vec2(0.0, 0.0);",
		"\tvertexTexLayer = 0;",
		"\tdoDiscard = 1;",
		"}",
		"",
		"void main() {",
		"\tif (!(enableSceneData != 0) && !(enableBallData != 0)) {",
		"\t\tmainGUI();",
		"\t} else if ((enableSceneData != 0) && !(enableBallData != 0)) {",
		"\t\tmainScene();",
		"\t} else if (!(enableSceneData != 0) && (enableBallData != 0)) {",
		"\t\tmainBall();",
		"\t} else {",
		"\t\t// Unknown configuration / shader mode; unknown what to draw.",
		"\t\tmainUnknown();",
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
		"in flat int  doDiscard;",
		"",
		"layout(location = 16) uniform int       enableSceneData;",
		"layout(location = 34) uniform int       enableBallData;",
		"layout(location = 35) uniform float     ballRadius;",
		"layout(location = 36) uniform int       ballNumTriangles;",
		"layout(location = 37) uniform vec3      ballPos;",
		"layout(location = 38) uniform vec3      ballRot;",
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
		"\t// Draw the GUI or the level.",
		"void mainGUIOrScene() {",
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
		"\t// Draw the ball.",
		"void mainBall() {",
		"\tfragmentColor = vertexModulateColor;",
		"}",
		"",
		"void main() {",
		--"\tfragmentColor = texture(texture0, vertexTexCoords);",
		"\tif ((doDiscard != 0)) {",
		"\t\tdiscard;",
		"\t} else if (!(enableSceneData != 0) && (enableBallData != 0)) {",
		"\t\tmainBall();",
		"\t} else {",
		"\t\tmainGUIOrScene();",
		"\t}",
		"}",
		"",
		""
	]
