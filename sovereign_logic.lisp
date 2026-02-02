(defun find-color-rule (pairings grid-in grid-out)
  (remove-duplicates
   (mapcar (lambda (p) 
             (list (get-obj-color (getf p :input) grid-in)
                   (get-obj-color (getf p :output) grid-out)))
           pairings)
   :test #'equal))

(defun get-global-cipher (file-path)
  (let* ((j (cl-json:decode-json-from-source (pathname file-path)))
         (train (cdr (assoc :train j)))
         (full-mapping '()))
    (dolist (ex train)
      (let* ((in (cdr (assoc :input ex)))
             (out (cdr (assoc :output ex)))
             (pairs (match-objects (get-objs in) (get-objs out))))
        (setf full-mapping (append full-mapping (find-color-rule pairs in out)))))
    (remove-duplicates full-mapping :test #'equal)))

(defun apply-recolor-rule (grid mapping)
  (loop for row in grid
        collect (loop for cell in row
                      collect (let ((match (assoc cell mapping)))
                                (if match (cadr match) cell)))))

(defun solve-test-case-v2 (file-path)
  (let* ((j (cl-json:decode-json-from-source (pathname file-path)))
         (test-data (car (cdr (assoc :test j))))
         (test-in (cdr (assoc :input test-data)))
         (cipher (get-global-cipher file-path)))
    (format t "~%GLOBAL CIPHER: ~A~%" cipher)
    (let ((result (apply-recolor-rule test-in cipher)))
      (dolist (row result) (format t "~A~%" row))
      result)))
