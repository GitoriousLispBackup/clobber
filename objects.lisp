(in-package :clobber)

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