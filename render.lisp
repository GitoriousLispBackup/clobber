(in-package :clobber)

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