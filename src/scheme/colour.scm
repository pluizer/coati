(bind-coati %colour:push void (f32vector))
(bind-coati %colour:pop void ())
(define-syntax with-colour
  (syntax-rules ()
    ((_ colour form ...)
     (begin (%colour:push colour) form ... (%colour:pop)))))
