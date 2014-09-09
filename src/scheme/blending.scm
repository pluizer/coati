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
