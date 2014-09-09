(define-foreign-type batch (c-pointer "CT_Batch"))
(bind-coati batch:create batch (unsigned-int)
	    (finally %batch:free))
(bind-coati %batch:free void (batch))
(bind-coati batch:push unsigned-int (batch transformation))
(bind-coati batch:remove void (batch unsigned-int))
(bind-coati batch:change void (batch unsigned-int transformation))
(bind-coati batch:render void (batch texture))
(bind-coati batch:size unsigned-int (batch))

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
