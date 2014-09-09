(define sprite-batch:default-size-hint (make-parameter 20))

(define-mystruct sprite-batch
  ((%handles value: (list))
   (%batch value: (batch:create (sprite-batch:default-size-hint)))))

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
	 (handle
	  (lambda (#!key position 
			 origin
			 rotation
			 (scale (vect:create 1 1)) 
			 flip-h flip-v
			 remove)
	    (if remove 
		(begin
		  ;; Remove this handle from the table.
		  (set! (sprite-batch:%handles sbatch) 
			(alist-delete! id (sprite-batch:%handles sbatch)))
		  ;; Remove it from the batch.
		  (batch:remove batch id))
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
    ;; Store the handle, so it can be polled if it is an animation.
    ;; We store it by id, else we cannot delete in inside the handle
    ;; closure.
    (set! (sprite-batch:%handles sbatch)
	  (cons (cons id handle) (sprite-batch:%handles sbatch)))
    handle))

(define (sprite-batch:poll-animations sbatch)
  (for-each (lambda (x) (cdr x)) (sprite-batch:%handles sbatch)))

(define (sprite-batch:render sbatch texture)
  (batch:render (sprite-batch:%batch sbatch) texture))

(define (sprite-batch:clear sbatch)
  (for-each (lambda (handle-pair)
	      ((cdr handle-pair) remove: #t))
	    (sprite-batch:%handles sbatch)))
