(in-package :clobber)

(defclass world ()
  ((layers :type vector :initform (list (make-array '(10 10)) (make-array '(10 10))) :accessor layers)
   (objects :type hash-table :initform (make-hash-table) :accessor objects)
   (instances :type list :initform () :accessor instances)
   (sprites :type hash-table :initform (make-hash-table) :accessor sprites)))

(defclass object ()
  ((layer :accessor layer :initarg :layer :initform *current-layer*)
   (sprite :accessor sprite :initarg :sprite :initform nil)
   (top-sprite :accessor top-sprite :initarg :top-sprite :initform nil)
   (x    :accessor x  :initarg :x  :initform nil)
   (y    :accessor y  :initarg :y  :initform nil)
   (z    :accessor z  :initarg :z  :initform nil)
   (n    :accessor n  :initarg :n  :initform nil)
   (hp   :accessor hp :initarg :hp :initform  10)
   (tool :accessor hp :initarg :hp :initform nil)
   (size :accessor hp :initarg :hp :initform  10)
   (inventory :accessor hp :initarg :hp :initform 10)))

(defclass player (object) ())
(defclass earth (object) ())
(defclass stone (object) ())
(defclass water (object) ())
(defclass dirt (object) ())
(defclass grass (object) ())
(defclass fire (object) ())
(defclass clay (object) ())
(defclass air (object) ())

(defparameter *object-list* (list 'player
                                  'earth
                                  'stone
                                  'water
                                  'dirt
                                  'grass
                                  'fire
                                  'clay
                                  'air))

(defparameter *world*         (make-instance 'world))

(defparameter *player*        (make-instance 'player :x 5
                                              :y 5
                                              :z 0
                                              :n 1
                                              :top-sprite nil
                                              :sprite nil
                                              :layer 0))

(defgeneric attach (thing obj)
  (:documentation "Attaches a thing to the appropriate slot of obj")
  (:method ((thing sdl:surface) obj)
    (setf (sprite obj)
          (sdl:load-image
           (asdf:system-relative-pathname :clobber
                                          (concatenate 'string
                                                       "images/"
                                                       (string-downcase (string thing))
                                                       ".png"))))))

(defgeneric move (layer obj nx ny)
  (:documentation "Renders a object onto the default sdl window.")
  (:method (layer (player player) nx ny)
    (let ((px (x player))
          (py (y player)))
      (setf (aref (nth layer (layers *world*)) py px) 0)
      (setf (x player) nx)
      (setf (y player) ny)
      (setf (aref (nth layer (layers *world*)) ny nx) 1))))

(defgeneric render (thing x y)
  (:documentation "Renders a object onto the default sdl window.")
  (:method ((sprite sdl:surface) x y)
    (sdl:draw-surface-at-* sprite x y))
  (:method ((uid fixnum) x y)
    (sdl:draw-surface-at-* (gethash uid (objects *world*)) x y))
  (:method ((name symbol) x y)
    (sdl:draw-surface-at-* (gethash name (objects *world*)) x y)))

(defgeneric create (unit)
  (:documentation "Creates any type of unit and pushes it onto that unit's stack.")
  (:method ((player player))
    (make-instance 'player))
  (:method (unit)
    (make-instance unit)))

(defgeneric locate (object)
  (:documentation "Get the (x . y) coordinate cons for unique objects or a list of coordinates for generic objects.")
  (:method ((player player))
    (values (layer player)
            (player player))))

(defun lookup-sprite (object)
  (sprite object))

(defun lookup-object (id)
  (gethash id (objects *world*)))

;(val (layer-value-at (layer-at-player) i j))

(defun render-tiles ()
  "This function renders  tiles across the default SDL window."
  (loop for i from 0 to 9 do
       (loop for j from 0 to 9 do
            (let* ((obj (lookup-object (value-at i j))) ; val or (value-at i j)
                   (sprite (lookup-sprite obj)))
              (render sprite (* i 8) (* j 8))))))

(defun initialize-object-table ()
  (dolist (name *object-list*)
    (push `,(make-instance name) (instances *world*)))
  (clrhash (objects *world*))
  (let ((counter 0))
    (dolist (i *object-list*)
      (setf (gethash i (objects *world*)) counter)
      (setf (gethash counter (objects *world*)) i)
      (incf counter))
    (objects *world*)))

(defun initialize-sprite-table ()
  (dolist (name *object-list*)
    `(attach-sprite ,name ,name))
  (clrhash (sprites *world*))
  (let ((counter 0))
    (dolist (i *object-list*)
      (setf (gethash counter (sprites *world*)) (sdl:load-image
         (asdf:system-relative-pathname
          :clobber
          (concatenate 'string
                       "images/"
                       (string-downcase (symbol-name i))
                       ".png"))))
      (incf counter))
    (objects *world*)))

(defun attach-sprite (object name)
  "Adds a sprite"
  (setf (sprite object)
        (sdl:load-image
         (asdf:system-relative-pathname
          :clobber
          (concatenate 'string
                       "images/"
                       (string-downcase (symbol-name name))
                       ".png")))))

(defun player (player)
  (cons (x player) (y player)))

(defun add-layer ()
  (push (make-array '(10 10)) (layers *world*)))

(defun value-under (x y)
  (aref (layer-under-player) y x))

(defun layer-value-under (layer x y)
  (aref (nth (1+ layer) (layers *world*)) y x))

(defun get-players-layer ()
  (nth (layer *player*) (layers *world*)))

(defun get-layer-under-player ()
  (nth (1+ (layer *player*)) (layers *world*)))

(defun get-layer-above-player ()
  (nth (1- (layer *player*)) (layers *world*)))