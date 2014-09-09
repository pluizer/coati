(bind-coati %target:push void (texture))
(bind-coati %target:pop void ())
(define-syntax with-target
  (syntax-rules ()
    ((_ target form ...)
     (begin (%target:push target) form ... (%target:pop)))))
