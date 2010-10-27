(in-package :clobber)

(defparameter *current-layer* 0)

(defun concatenate-symbols (list-of-symbols &optional (package *package*))
  (intern (apply #'concatenate 'string (mapcar #'symbol-name list-of-symbols)) package))
(defun concatenate-symbols-to-key (list-of-symbols &optional (package 'keyword))
  (intern (apply #'concatenate 'string (mapcar #'symbol-name list-of-symbols)) package))

(defun defsprite (name)
  (sdl:load-image
   (asdf:system-relative-pathname :clobber (concatenate 'string
                                                        "images/"
                                                        (string-downcase (string name))
                                                        ".png"))))

(defmacro defobject (name (&body slots) &key (inherit nil))
  (let* ((slot-list '((sprite)
                      (top-sprite)
                      (x)
                      (y)
                      (hp)))
         (object-slots))
    (push `(layer :accessor layer :initarg :layer :initform ,*current-layer*) object-slots)
    (dolist (i (append slots slot-list))
      (push `(,@i :accessor ,@i :initarg ,(concatenate-symbols-to-key i)) object-slots))
    `(defclass ,name ,(if inherit inherit ())
       ,object-slots)))

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
   (instances :type list :initform () :accessor instances)
   (sprites :type hash-table :initform (make-hash-table) :accessor sprites)))