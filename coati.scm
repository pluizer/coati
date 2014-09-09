#|
Copyright (c) 2014 Richard van Roy

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#

(module coati
	*
	(import chicken scheme foreign extras irregex)
	(use srfi-1 data-structures srfi-4 foreigners mystruct lolevel)
	(import-for-syntax matchable clojurian-syntax)
#>
#include "audio.h"
#include "core.h"
#include "input.h"
#include <float.h>
<#

;; Helper macros and functions
(include "src/scheme/utils.scm")

;; Error handling functions
(include "src/scheme/error.scm")

;; Shader
(include "src/scheme/shader.scm")

;; Window
(include "src/scheme/window.scm")

;; Image
(include "src/scheme/image.scm")

;; Colour
(include "src/scheme/colour.scm")

;; Blending
(include "src/scheme/blending.scm")

;; Texture
(include "src/scheme/texture.scm")

;; Transformation
(include "src/scheme/transformation.scm")

;; Target
(include "src/scheme/target.scm")

;; Batch
(include "src/scheme/batch.scm")

;; Font
(include "src/scheme/font.scm")

;; Translation
(include "src/scheme/translation.scm")

;; Audio
(include "src/scheme/audio.scm")

;; Sprites
(include "src/scheme/sprite.scm")

;; Sprite batch
(include "src/scheme/sprite-batch.scm")

;; Input
(include "src/scheme/input.scm")

;; Primitive functions
(include "src/scheme/primitives.scm")

;; Tilemap
(include "src/scheme/tilemap.scm")

)
