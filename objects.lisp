(in-package :clobber)

;;; Sprite Functions
;(gethash name (sprites *world*))
(defun add-sprite (name)
  "Adds a sprite"
  (setf (sprite name)
        (sdl:load-image
         (asdf:system-relative-pathname
          :clobber
          (concatenate 'string
                       "images/"
                       (string-downcase (symbol-name name))
                       ".png")))))

(defun initialize-sprite-table ()
  "Initializes the starting sprites."
  (add-sprite 'empty)
  (add-sprite 'player)
  (add-sprite 'earth)
  (add-sprite 'stone)
  (add-sprite 'grass))

(defun lookup-sprite (object)
  (sprite object))


(let ((counter 0))
  (defun add-object (name)  
    (defobject name () :inherit (unit))
    (setf (gethash counter (objects *world*)) name)
    (push (make-instance name) (instances *world*))
    (incf counter)))

(defun initialize-object-table ()
  (add-object 'earth)
  (add-object 'stone)
  (add-object 'water)
  (add-object 'dirt)
  (add-object 'grass)
  (add-object 'fire)
  (add-object 'clay)
  (add-object 'air)
  (defplayer))

(defun lookup-object (id)
  (gethash id (objects *world*)))