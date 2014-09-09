#|
This functions returns a function that checks if
the values passed to it is the same as last time.
|#
(define (make-change-check)
  (let* ((old-value #f))
    (lambda (#!rest values)
      (if (equal? values old-value)
	  #f
	  (begin (set! old-value values)
		 #t)))))
#|
Creates a new tilemap and returns a function to render it.
- tile-size	: The size (for both with and height) of the tile.
- tilemap-size	: The maximum number of tiles the tilemap has to render
		  meaning that ``width`` * ``height`` in the render
		  function can never be higher than this.

Returned handle to render the tilemap takes these arguments:
- coord		: The coordinate of the top-left tile.
- width, height : The size of the map in tiles.
- coord->sprite	: A function that the coordinate of a tile
		  and returns a sprite.
|#
(define (tilemap:create tile-size)
  (let* (
	 ;; The batch where all the to be rendered sprite are.
	 (sbatch (sprite-batch:create))
	 ;; Rememer the last added coordinate and the width and height
	 ;; so that the sprite-batch does not have to be repopulated
	 ;; when these values haven't changed.
	 (changed? (make-change-check)))
    ;; Function that renders the tilemap from a specific coordinate.
    ;; width, and height specify the number of tiles to render.
    (let ((raw
	   (lambda (coord 
		    width height 
		    ;; The texturemap to use for rendering the tiles.
		    texture
		    ;; A function that takes a coordinate and returns a tile number.
		    coord->sprite)
	     ;; If ``coord`` ``width`` ``height`` or ``coord->sprite`` 
	     ;; changed we'llo repopulate te sprite-batch.
	     (when (changed? coord width height coord->sprite)
	       (let ( ;; List of all coordinates
		     (coords
		      (map (lambda (x)
			     (coord:create (+ (modulo x width)
					      (coord:x coord))
					   (+ (floor (/ x width))
					      (coord:y coord))))
			   (iota (* width height)))))
		 ;; Clear the previously added sprites.
		 (sprite-batch:clear sbatch)
		 ;; Add the new sprites
		 (for-each (lambda (tile-coord)
			     (let ((sprite (coord->sprite tile-coord)))
			       ;; It is possible not to have a sprite at these coords.
			       (when sprite
				 (let ((handle (sprite-batch:push 
						sbatch 
						(coord->sprite tile-coord))))
				   ;; Put the newly added sprite in its right position.
				   (handle position: (vect:create
						      (* (- (coord:x tile-coord) 
							    (coord:x coord)) tile-size)
						      (* (- (coord:y tile-coord)
							    (coord:y coord)) 
							 tile-size)))))))
			   coords)))
	     ;; Render the sprite-batch
	     (sprite-batch:render sbatch texture))))
      #|
      Function returned by ``tilemap:create``. Renders the map from
      the ``top-left`` coordinate. (which is a vect not a coord so
      it it can have fraction).
      |#
      (lambda (top-left width height texture coord->sprite)
	(let* ((x (vect:x top-left))
	       (y (vect:y top-left))
	       (fx (floor x))
	       (fy (floor y))
	       (rx (- x fx))
	       (ry (- y fy)))
	  (with-translation ((vect+ (vect:create (* rx tile-size)
						 (* ry tile-size))
				    (vect:create tile-size tile-size)) 1 0)
	    (raw (coord:create (- (- fx) 1) (- (- fy) 1))
		 (+ width 1) (+ height 1)
		 texture coord->sprite)))))))
