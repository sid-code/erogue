(require 'gv)

(cl-defstruct erogue-entity
  (name        nil :type string)
  (description nil :type string)
  display-char)

(cl-defstruct erogue-stats
  (health 0)
  (stamina 0)
  (strength 0)
  (intelligence 0)
  (luck 0))

(cl-defstruct erogue-world maps)
(cl-defstruct erogue-map
  (width  nil :type integer)
  (height nil :type integer)
  walls
  entities
  brightness)

(cl-defstruct erogue-game
  world
  (player-mapindex nil :type integer))

(defun erogue--make-border (map)
  "Creates walls around the map."
  (let ((width  (erogue-map-width map))
	(height (erogue-map-height map))
	(walls  (erogue-map-walls map)))
    (cl-loop for i from 0 to (- width 1)
	     do
	     (setf (aref walls (erogue--map-index map i 0)) t)
	     (setf (aref walls (erogue--map-index map i (- height 1))) t))
    (cl-loop for i from 0 to (- height 1)
	     do
	     (setf (aref walls (erogue--map-index map 0 i)) t)
	     (setf (aref walls (erogue--map-index map (- width 1) i)) t)))
  map)

(defun erogue--map-index (map x y)
  "Calculates the internal array index of the point at (X, Y) in MAP.
X is the number of tiles right from the top-left corner.
Y is the number of tiles below the top-left corner."
  (+ x (* y (erogue-map-width map))))

(defun erogue--init-map (map)
  (let ((vec-len (* (erogue-map-width map) (erogue-map-height map))))
    (setf (erogue-map-entities map) (make-vector vec-len nil))
    (setf (erogue-map-walls map) (make-vector vec-len nil))
    (setf (erogue-map-brightness map) (make-vector vec-len 50)))
  map)

(defun erogue--map-has-wall (map x y)
  (not (null (aref (erogue-map-walls map) (erogue--map-index map x y)))))

(defun erogue--map-entity (map x y)
  (aref (erogue-map-entities map) (erogue--map-index map x y)))

(gv-define-expander erogue--map-entity
  (lambda (do place x y)
    (gv-letplace (getter setter) place
      (macroexp-let2* (x y)
          (funcall do `(erogue--map-entity ,getter ,x ,y)
                   (lambda (v)
                     (macroexp-let2* (v)
                         `(progn
                            ,(funcall setter `(let ((g ,getter))
                                                (setf (aref (erogue-map-entities g)
                                                            (erogue--map-index g ,x ,y))
                                                      ,v)
                                                g))))))))))

(defun erogue--init-world ()
  (make-erogue-world :maps '()))

(defun erogue--make-test-game ()
  (make-erogue-game
   :world (let ((world (erogue--init-world))
                (map (erogue--make-border
                      (erogue--init-map
		       (make-erogue-map
			:width 10
			:height 10))))
                (player (make-erogue-entity
                         :name "Player"
                         :description "The Player"
                         :display-char "@")))
            (push player (erogue--map-entity map 1 1))
	    (push map (erogue-world-maps world))
	    world)
   :player-mapindex 0))

(defun erogue--get-current-map (game)
  (let* ((world       (erogue-game-world game))
	 (playerindex (erogue-game-player-mapindex game))
	 (maps        (erogue-world-maps world)))
    (cond ((<= (length maps) playerindex)
	   (error "Invalid player map index %d, maximum index %d."
		  playerindex (length maps)))
	  ((< playerindex 0)
	   (error "Invalid player map index %d, must be non-negative."
		  playerindex))
	  (t (nth playerindex maps)))))

(defun erogue--display-primitive (game)
  (let ((viewport-width  (window-total-width))
	(viewport-height (window-total-height))
	(map             (erogue--get-current-map game)))
    (delete-region (point-min) (point-max))
    (cl-loop
     for row from 0 to (- (erogue-map-height map) 1)
     do (progn
          (cl-loop
	   for col from 0 to (- (erogue-map-width map) 1)
	   do (let ((entity (erogue--map-entity map col row)))
                (cond
		 ((not (null entity))
                  (insert
                   (erogue-entity-display-char
                    (if (listp entity)
                        (car entity)
                      entity))))
		 ((erogue--map-has-wall map col row) (insert ?\#))
		 (t (insert ?\ )))))
          (insert "\n")))))

(defun erogue--redisplay (game)
  )

(defmacro with-writable-buffer (&rest body)
  `(progn
     (when buffer-read-only
       (read-only-mode -1))
     ,@body
     (read-only-mode)))


(define-derived-mode erogue-mode special-mode "Erogue" "Erogue is an Emacs-based roguelike"
  (setq-local scroll-conservatively 1))

(defun erogue ()
  (interactive)
  (let ((game (erogue--make-test-game))
	(buf (get-buffer-create "*erogue*")))
    (switch-to-buffer buf)
    (erogue-mode)
    (with-writable-buffer (erogue--display-primitive game))))
