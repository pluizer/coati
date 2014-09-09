(define-foreign-type transformation (c-pointer "CT_Transformation"))

(define (trans:create source-rect #!optional
		      (dest-rect source-rect)
		      (origin (vect:create 0 0))
		      (rotation 0)
		      flip-h
		      flip-v)
  (make-locative (apply f32vector (flatten (f32vector->list source-rect)
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
