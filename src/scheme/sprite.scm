(define-mystruct sprite
  (rect))

(define-general :rect sprite? sprite:rect)

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
