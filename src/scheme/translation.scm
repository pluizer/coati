(bind-coati %translation:push void (f32vector float float))
(bind-coati %translation:pop void ())
; Note that translation's do not nest.
(define-syntax with-translation
  (syntax-rules ()
    ((_ (position scale rotation) form ...)
     (begin (%translation:push position scale rotation) form ... (%translation:pop)))))
