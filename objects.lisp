(in-package :clobber)

(defparameter *object-list*
  '(player
    earth
    stone
    water
    dirt
    grass
    fire
    clay
    air))

(defmacro make-objects ()
  (defobject unit (x y hp inventory))
  (defcontainer rucksack :slots 16)
  (dolist (i *object-list*)
    `(defobject ,i () :inherit (unit)))
  (defobject player (name direction str def) :inherit (unit))
  (setf *player* (make-instance 'player)))

(defun initialize-object-table ()
  (dolist (name *object-list*)
    `(defobject ,name () :inherit (earth)))
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


;; Imperative Object Creation
(initialize-object-table)
(make-objects)