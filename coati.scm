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
	(use srfi-1 data-structures srfi-4 foreigners mystruct)
	(import-for-syntax matchable clojurian-syntax)
	(include "primitives.scm")
#>
#include <coati/audio.h>
#include <coati/core.h>
#include <coati/input.h>
#include <float.h>
<#

;; Binding helpers
;; (bind-coati NAME RET-TYPE VAR-TYPES [FUNC/OPTION])
;; RET-TYPE 	: foreign-type
;;		: (xxx-vector size)
;; VAR-TYPES    : (type ...)
;; FUNC/OPTION	: function
;;		: (finally finalizer-function)
;;		: (parameter setter-function)
(define-syntax bind-coati
  (ir-macro-transformer
   (lambda (expression inject compare?)

     ;; Converts a scheme function name to a C function name
     ;; using Coati's naming conventions.
     ;; an:example 	-> ct_an_example
     ;; an-example? 	-> ct_is_an_example
     ;; %an-example? 	-> ct_is_an_example
     ;; one->three	-> ct_one_to_three
     ;; test-set!	-> ct_test_set!
     (define (c-function-name symbol)
       (define (replace str rex sub)
	 (irregex-replace/all rex str sub))
       (let ((name (symbol->string symbol)))
	 (string-append
	  "ct_"
	  (if (irregex-match ".*\\?" name) "is_" "")
	  (-> name
	      (replace "[!?%]" "")
	      (replace "\\->" "_to_")
	      (replace "[:-]" "_")))))

     (define (bounded name ret-type var-types)
       (cond
	;; Binds C functions that use the last argument as a return value (array)
	;; of size N.
	;; (bind-coati NAME (xxx-VECTOR SIZE) [FUNC/OPTION])
	((list? ret-type)
	 `(lambda (#!rest args)
	    (let ((ret (apply ,(car ret-type) (make-list ,(cadr ret-type) 0))))
	      (apply (foreign-lambda void
				     ,(c-function-name (inject name))
				     ,@(append var-types (list (car ret-type))))
		     (append args (list ret)))
	      ret)))
	;; Binds normal C functions
	;; (bind-coati NAME RET-TYPE VAR-TYPES [FUNC/OPTION])
	(else
	 `(foreign-lambda ,ret-type
			  ,(c-function-name (inject name))
			  ,@var-types))))

     ;; Generates a list of unique symbols
     (define (unique-symbols n)
       (map gensym (make-list n 'arg)))
     (match expression
	    ;; Simply bind a Coati function
	    ;; (bind-coati NAME RET-TYPE (VAR-TYPES)
	    ((_ name ret-type var-types)
	     `(define ,name ,(bounded name ret-type var-types)))
	    ((_ name ret-type var-types func/option)
	     (cond ((and (list? func/option)
			 (eq? (car func/option) (inject 'finally)))
		    ;; Wraps the function around finalizer FUNC
		    ;; (bind-coati NAME RET-TYPE (VAR-TYPES) (finally FUNC)
		    `(define (,name . args)
		       (set-finalizer! (apply ,(bounded name ret-type var-types) args)
				       ,(cadr func/option))))
		   ;; Adds a optional argument (VALUE) to the bound function which,
		   ;; when supplied, calls (FUNC VARS VALUE). Where the function is
		   ;; intended to be a setter, turning this binding into a parameter.
		   ;; (bind-coati NAME RET-TYPE (VAR-TYPES) (PARAMETER FUNC))
		   ((and (list? func/option)
			 (eq? (car func/option) (inject 'parameter)))
		    (let ((vars (unique-symbols (length var-types)))
			  ;; Use 'not-supplied instead of nothing (#f) so it is 
			  ;; possible to pass #f as a value.
			  (not-supplied (gensym 'not-supplied)))
		      `(define (,name ,@vars #!optional (value ',not-supplied))
			 (if (eq? value ',not-supplied)
			     (,(bounded name ret-type var-types) ,@vars)
			     (,(cadr func/option) ,@vars value)))))
		   ;; Wrap bound function around a scheme function.
		   ;; (bind-coati NAME RET-TYPE (VAR-TYPES) FUNC)
		   (else
		    `(define (,name . args)
		       (,func/option (apply ,(bounded name ret-type var-types) args))))))
	    ))))

;; define /and/ export foreign enums
(define-syntax define-foreign-enum*
  (syntax-rules ()
    ((_ (type foreign-type)
	(scheme-symbol c-symbol) ...)
     (begin (define-foreign-type type foreign-type)
	    (define scheme-symbol
	      (foreign-value c-symbol type)) ...))))

;; Misc
(define (uber-ref obj index)
  ;; Sorted by likely order of prevalence.
  ((cond 
    ((f32vector? obj) f32vector-ref)
    ((s32vector? obj) s32vector-ref)
    ((u32vector? obj) u32vector-ref)
    ((list?      obj) list-ref)
    ((vector?    obj) vector-ref)
    ((u8vector?  obj) u8vector-ref)
    ((s8vector?  obj) s8vector-ref)
    ((u16vector? obj) u16vector-ref)
    ((s16vector? obj) s16vector-ref)
    ((f64vector? obj) f64vector-ref)) 
  obj index))

(define-syntax pop-cycle
  (syntax-rules ()
    ((_ lst)
     (let ((ret (car lst)))
       (set! lst (append (cdr lst) (list ret)))
       ret))))

;; Error
(bind-coati get-error c-string ())
(bind-coati set-error void (c-string))

;; Window
(bind-coati window:init bool ())
(bind-coati window:quit void ())
(bind-coati %window:resolution-set! void (u32vector))
(bind-coati window:resolution (u32vector 2) ()
	    (parameter %window:resolution-set!))
(bind-coati %window:fullscreen-set! void (bool))
(bind-coati window:fullscreen bool ()
	    (parameter %window:fullscreen-set!))
(bind-coati window:update void ())
(bind-coati window:clear void (f32vector))

;; Image
(define-foreign-type image (c-pointer "CT_Image"))
(bind-coati image:load image (c-string)
	    (finally %image:free))
(bind-coati image:create image (unsigned-int unsigned-int)
	    (finally %image:free))
(bind-coati %image:free void (image))
(bind-coati image:size (f32vector 2) (image))

;; Colour
(bind-coati %colour:push void (f32vector))
(bind-coati %colour:pop void ())
(define-syntax with-colour
  (syntax-rules ()
    ((_ colour form ...)
     (begin (%colour:push colour) form ... (%colour:pop)))))

;; Blending
(define-foreign-enum* (blend-mode (enum "_CT_BlendMode"))
  (blend-mode:normal  CT_BLEND_MODE_NORMAL)
  (blend-mode:add     CT_BLEND_MODE_ADD)
  (blend-mode:trans   CT_BLEND_MODE_TRANS)
  (blend-mode:one-one CT_BLEND_MODE_ONE_ONE))
(bind-coati %blend-mode:push void (blend-mode))
(bind-coati %blend-mode:pop void ())
(define-syntax with-blend-mode
  (syntax-rules ()
    ((_ blend-mode form ...)
     (begin (%blend-mode:push blend-mode) form ... (%blend-mode:pop)))))

;; Texture
(define-foreign-type texture (c-pointer "CT_Texture"))
(bind-coati image->texture texture (image)
	    (finally %texture:free))
(bind-coati texture:create texture (unsigned-int unsigned-int)
	    (finally %texture:free))
(bind-coati texture:copy texture (texture)
	    (finally %texture:free))
(bind-coati texture:load texture (c-string)
	    (finally %texture:free))
(bind-coati %texture:free void (texture))
(bind-coati texture:screen? bool (texture))
(bind-coati texture:size (f32vector 2) (texture))
(bind-coati texture:clear void (texture f32vector))
(bind-coati texture:render void (texture transformation))

;; Transformation
(define-foreign-type transformation (c-pointer "CT_Transformation"))
(bind-coati array->transformation transformation (f32vector)
	    (finally %transformation:free))
(bind-coati %transformation:free void (transformation))

;; Target
(bind-coati %target:push void (texture))
(bind-coati %target:pop void ())
(define-syntax with-target
  (syntax-rules ()
    ((_ target form ...)
     (begin (%target:push target) form ... (%target:pop)))))

;; Batch
(define-foreign-type batch (c-pointer "CT_Batch"))
(bind-coati batch:create batch (unsigned-int)
	    (finally %batch:free))
(bind-coati %batch:free void (batch))
(bind-coati batch:push unsigned-int (batch transformation))
(bind-coati batch:remove void (batch unsigned-int))
(bind-coati batch:change void (batch unsigned-int transformation))
(bind-coati batch:render void (batch texture))

;; Font
(define-foreign-type font (c-pointer "CT_Font"))
(bind-coati %font:free void (font))
(bind-coati font:load font (c-string)
 	    (finally %font:free))
(bind-coati string->texture texture (font unsigned-int c-string f32vector))

;; Camera
(bind-coati %camera:push void (f32vector float float))
(bind-coati %camera:pop void ())
; Note that camera's do not nest.
(define-syntax with-camera
  (syntax-rules ()
    ((_ (position scale rotation) form ...)
     (begin (%camera:push position scale rotation) form ... (%camera:pop)))))
; Returns the screens bb ignoring rotation 
(bind-coati camera:rect (f32vector 4) ())

;; Trans
(define (trans:create source-rect #!optional
		      (dest-rect source-rect)
		      (origin (vect:create 0 0))
		      (rotation 0)
		      flip-h
		      flip-v)
  (array->transformation
   (apply f32vector (flatten (f32vector->list source-rect)
			     (f32vector->list dest-rect)
			     (f32vector->list origin)
			     rotation (if flip-h 1 -1) (if flip-v 1 -1)))))

(define (trans:make source-rect #!key
		    (dest-rect source-rect)
		    (origin (vect:create 0 0))
		    (rotation 0)
		    (flip-h #f)
		    (flip-v #f))
  (trans:create source-rect dest-rect origin rotation 
		flip-h
		flip-v))

;; Input
(bind-coati quitting? bool ())
(bind-coati key:pressed? bool (key))
(bind-coati key:released? bool (key))
(bind-coati key:holded? bool (key))
(bind-coati mouse:position (f32vector 2) ())
(bind-coati %holded-keys
	    (u32vector (foreign-value "CT_MAX_INPUT_STACK_SIZE" int))
	    ()
	    (lambda (v) (remove zero? (u32vector->list v))))
(bind-coati poll-input void ())
(bind-coati key:name c-string (key))

;; Audio
(define-foreign-type sample (c-pointer "CT_Sample"))
(define-foreign-type channel integer)
(bind-coati %sample:free void (sample))
(bind-coati sample:load sample (c-string)
	    (finally %sample:free))
(bind-coati sample:play channel (sample f32vector bool))

;; Sprite
(define-mystruct sprite
  (rect))

(define-general :rect sprite? sprite:rect)

;; Animation sprite
(define-mystruct animation
  (rects
   (interval value: (/ 1000 20))
   (%epoch   value: (current-milliseconds))))

(define (animation:new-frame? ani)
  (> (- (current-milliseconds)
	(animation:%epoch ani))
     (animation:interval ani)))

(define (animation:rect ani)
  (when (animation:new-frame? ani)
	(pop-cycle (animation:rects ani))
	(set! (animation:%epoch ani) (current-milliseconds)))
  (car (animation:rects ani)))

(define-general :rect animation? animation:rect)

(define-general :new-frame animation? animation:new-frame?)

;; Sprite batch
(define-mystruct sprite-batch
  (max-size
   (%closures value: (list))
   (%batch value: (batch:create max-size))))

(define (sprite-batch:push sbatch sprite)
  (let* ((batch (sprite-batch:%batch sbatch))
	 (translation (trans:create (:rect sprite)))
	 (id (batch:push batch translation))
	 (c-position (vect:create 0 0))
	 (c-origin   (vect:create 0 0))
	 (c-rotation 0)
	 (c-scale    (vect:create 1 1))
	 (c-flip-h   #f)
	 (c-flip-v   #f)
	 (closure
	  (lambda (#!key position 
			 origin
			 rotation
			 (scale (vect:create 1 1)) 
			 flip-h flip-v
			 remove)
	    (if remove (batch:remove batch id)
		(begin
		  (when position (set! c-position position))
		  (when origin   (set! c-origin   origin))
		  (when rotation (set! c-rotation rotation))
		  (when scale    (set! c-scale    scale ))
		  (when flip-h   (set! c-flip-h   flip-h))
		  (when flip-v   (set! c-flip-v   flip-v))
		  (let* ((rect (:rect sprite))
			 (x (vect:x c-position))
			 (y (vect:y c-position))
			 (w (* (- (rect:r rect) (rect:l rect))
			       (vect:x c-scale)))
			 (h (* (- (rect:t rect) (rect:b rect))
			       (vect:y c-scale))))
		    (batch:change batch id
				  (trans:make rect
					      dest-rect:
					      (rect:create x (+ w x) y (+ h y))
					      origin: c-origin
					      rotation: c-rotation
					      flip-h: c-flip-h
					      flip-v: c-flip-v))))))))
    (set! (sprite-batch:%closures sbatch)
	  (cons closure (sprite-batch:%closures sbatch)))
    closure))

(define (sprite-batch:poll-animations sbatch)
  (for-each (lambda (x) (x)) (sprite-batch:%closures sbatch)))

(define (sprite-batch:render sbatch texture)
  (batch:render (sprite-batch:%batch sbatch) texture))

;; Keys
(define-foreign-enum* (key (enum "_CT_Key"))
  (key:backspace CT_KEY_BACKSPACE)
  (key:tab CT_KEY_TAB)
  (key:clear CT_KEY_CLEAR)
  (key:return CT_KEY_RETURN)
  (key:pause CT_KEY_PAUSE)
  (key:escape CT_KEY_ESCAPE)
  (key:space CT_KEY_SPACE)
  (key:exclaim CT_KEY_EXCLAIM)
  (key:quotedbl CT_KEY_QUOTEDBL)
  (key:hash CT_KEY_HASH)
  (key:dollar CT_KEY_DOLLAR)
  (key:ampersand CT_KEY_AMPERSAND)
  (key:quote CT_KEY_QUOTE)
  (key:leftparen CT_KEY_LEFTPAREN)
  (key:rightparen CT_KEY_RIGHTPAREN)
  (key:asterisk CT_KEY_ASTERISK)
  (key:plus CT_KEY_PLUS)
  (key:comma CT_KEY_COMMA)
  (key:minus CT_KEY_MINUS)
  (key:period CT_KEY_PERIOD)
  (key:slash CT_KEY_SLASH)
  (key:0 CT_KEY_0)
  (key:1 CT_KEY_1)
  (key:2 CT_KEY_2)
  (key:3 CT_KEY_3)
  (key:4 CT_KEY_4)
  (key:5 CT_KEY_5)
  (key:6 CT_KEY_6)
  (key:7 CT_KEY_7)
  (key:8 CT_KEY_8)
  (key:9 CT_KEY_9)
  (key:colon CT_KEY_COLON)
  (key:semicolon CT_KEY_SEMICOLON)
  (key:less CT_KEY_LESS)
  (key:equals CT_KEY_EQUALS)
  (key:greater CT_KEY_GREATER)
  (key:question CT_KEY_QUESTION)
  (key:at CT_KEY_AT)
  (key:leftbracket CT_KEY_LEFTBRACKET)
  (key:backslash CT_KEY_BACKSLASH)
  (key:rightbracket CT_KEY_RIGHTBRACKET)
  (key:caret CT_KEY_CARET)
  (key:underscore CT_KEY_UNDERSCORE)
  (key:backquote CT_KEY_BACKQUOTE)
  (key:a CT_KEY_A)
  (key:b CT_KEY_B)
  (key:c CT_KEY_C)
  (key:d CT_KEY_D)
  (key:e CT_KEY_E)
  (key:f CT_KEY_F)
  (key:g CT_KEY_G)
  (key:h CT_KEY_H)
  (key:i CT_KEY_I)
  (key:j CT_KEY_J)
  (key:k CT_KEY_K)
  (key:l CT_KEY_L)
  (key:m CT_KEY_M)
  (key:n CT_KEY_N)
  (key:o CT_KEY_O)
  (key:p CT_KEY_P)
  (key:q CT_KEY_Q)
  (key:r CT_KEY_R)
  (key:s CT_KEY_S)
  (key:t CT_KEY_T)
  (key:u CT_KEY_U)
  (key:v CT_KEY_V)
  (key:w CT_KEY_W)
  (key:x CT_KEY_X)
  (key:y CT_KEY_Y)
  (key:z CT_KEY_Z)
  (key:delete CT_KEY_DELETE)
  (key:kp0 CT_KEY_KP0)
  (key:kp1 CT_KEY_KP1)
  (key:kp2 CT_KEY_KP2)
  (key:kp3 CT_KEY_KP3)
  (key:kp4 CT_KEY_KP4)
  (key:kp5 CT_KEY_KP5)
  (key:kp6 CT_KEY_KP6)
  (key:kp7 CT_KEY_KP7)
  (key:kp8 CT_KEY_KP8)
  (key:kp9 CT_KEY_KP9)
  (key:kp_period CT_KEY_KP_PERIOD)
  (key:kp_divide CT_KEY_KP_DIVIDE)
  (key:kp_multiply CT_KEY_KP_MULTIPLY)
  (key:kp_minus CT_KEY_KP_MINUS)
  (key:kp_plus CT_KEY_KP_PLUS)
  (key:kp_enter CT_KEY_KP_ENTER)
  (key:kp_equals CT_KEY_KP_EQUALS)
  (key:up CT_KEY_UP)
  (key:down CT_KEY_DOWN)
  (key:right CT_KEY_RIGHT)
  (key:left CT_KEY_LEFT)
  (key:insert CT_KEY_INSERT)
  (key:home CT_KEY_HOME)
  (key:end CT_KEY_END)
  (key:pageup CT_KEY_PAGEUP)
  (key:pagedown CT_KEY_PAGEDOWN)
  (key:f1 CT_KEY_F1)
  (key:f2 CT_KEY_F2)
  (key:f3 CT_KEY_F3)
  (key:f4 CT_KEY_F4)
  (key:f5 CT_KEY_F5)
  (key:f6 CT_KEY_F6)
  (key:f7 CT_KEY_F7)
  (key:f8 CT_KEY_F8)
  (key:f9 CT_KEY_F9)
  (key:f10 CT_KEY_F10)
  (key:f11 CT_KEY_F11)
  (key:f12 CT_KEY_F12)
  (key:f13 CT_KEY_F13)
  (key:f14 CT_KEY_F14)
  (key:f15 CT_KEY_F15)
  (key:numlock CT_KEY_NUMLOCK)
  (key:capslock CT_KEY_CAPSLOCK)
  (key:scrollock CT_KEY_SCROLLOCK)
  (key:rshift CT_KEY_RSHIFT)
  (key:lshift CT_KEY_LSHIFT)
  (key:rctrl CT_KEY_RCTRL)
  (key:lctrl CT_KEY_LCTRL)
  (key:ralt CT_KEY_RALT)
  (key:lalt CT_KEY_LALT)
  (key:rmeta CT_KEY_RMETA)
  (key:lmeta CT_KEY_LMETA)
  (key:lsuper CT_KEY_LSUPER)
  (key:rsuper CT_KEY_RSUPER)
  (key:mode CT_KEY_MODE)
  (key:compose CT_KEY_COMPOSE)
  (key:help CT_KEY_HELP)
  (key:print CT_KEY_PRINT)
  (key:sysreq CT_KEY_SYSREQ)
  (key:break CT_KEY_BREAK)
  (key:menu CT_KEY_MENU)
  (key:power CT_KEY_POWER)
  (key:euro CT_KEY_EURO)
  (key:undo CT_KEY_UNDO)
  (button:left CT_BUTTON_LEFT)
  (button:middle CT_BUTTON_MIDDLE)
  (button:right CT_BUTTON_RIGHT)
  (button:wheelup CT_BUTTON_WHEELUP)
  (button:wheeldown CT_BUTTON_WHEELDOWN)
  (button:x1 CT_BUTTON_X1)
  (button:x2 CT_BUTTON_X2))

)
