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

;;; Unit Definition
(defobject unit (layer x y hp inventory))
(defcontainer rucksack :slots 16)
(defobject player (name direction str def) :inherit (unit))
(defobject earth () :inherit (unit))
(defobject stone () :inherit (earth))
(defobject water () :inherit (earth))
(defobject dirt () :inherit (earth))
(defobject grass () :inherit (earth))
(defobject fire () :inherit (earth))
(defobject clay () :inherit (earth))
(defobject air () :inherit (earth))


(defparameter *player* (make-instance 'player))
(defparameter *world* (make-instance 'world))

(defun lookup-sprite (object)
  (sprite object))

(defun lookup-object (id)
  (gethash id (objects *world*)))

;; (let ((counter 0))
;;   (defun add-object (name)  
;;     (defobject name () :inherit (unit))
;;     (setf (gethash counter (objects *world*)) name)
;;     (push (make-instance name) (instances *world*))
;;     (incf counter)))

;; (defun initialize-object-table ()
;;   (add-object 'earth)

;; (defun initialize-sprite-table ()
;;   "Initializes the starting sprites."
;;   (add-sprite 'empty)
;;   (add-sprite 'player)
;;   (add-sprite 'earth)
;;   (add-sprite 'stone)
;;   (add-sprite 'grass))
