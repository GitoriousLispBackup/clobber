(in-package :clobber)

(defmacro defsprite (name)
  `(sdl:load-image ,(asdf:system-relative-pathname :clobber (concatenate 'string "images/" (string-downcase (string name)) ".png"))))

(defmacro defobject (name (&body slots) &key (inherit nil))
  (let ((slot-list `((sprite :accessor sprite) (top-sprite :accessor top-sprite))))
    (dolist (i slots) (push (list i :accessor i) slot-list))
    `(defclass ,name ,(if inherit inherit '())
       ,@(list slot-list))))

(defmacro defcontainer (name &key (slots 6))
  (let ((acc))
    (dotimes (i slots)
      (push
       (list (intern (concatenate 'string "slot-" (write-to-string (1+ i)))))
       acc))
    `(defclass ,name (unit)        
       ,(reverse acc))))

(defmacro deftable (table-name)
  (let ((slots))
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (push (list value
                             :type 'simple-vector
                             :initform 'nil
                             :accessor value) slots)) (objects *world*))
    `(defclass ,table-name ()
       ,slots)))

(defclass world ()
  ((layers :type vector :initform (list (make-array '(10 10)) (make-array '(10 10))) :accessor layers)
   (objects :type hash-table :initform (make-hash-table) :accessor objects)
   (instances :type list :initform '() :accessor instances)
   (sprites :type hash-table :initform (make-hash-table) :accessor sprites)))