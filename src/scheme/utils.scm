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
