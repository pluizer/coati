(bind-coati %push-default-shader void ())
(bind-coati %shader:pop void ())
(define-syntax with-default-shader
  (syntax-rules ()
    ((_ form ...)
     (begin (%push-default-shader) form ... (%shader:pop)))))
