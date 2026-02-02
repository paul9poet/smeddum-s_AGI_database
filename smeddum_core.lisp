(cffi:defcfun ("calculate_resonance" %calc-resonance) :double (active :int) (total :int))

(defun find-centroid (grid)
  (let ((pts nil))
    (loop for r from 0 for row in grid do
      (loop for c from 0 for val in row do
        (when (not (zerop val)) (push (list r c) pts))))
    (if (null pts) '(0.0 0.0)
        (let ((n (length pts)))
          (list (float (/ (reduce #'+ (mapcar #'car pts)) n))
                (float (/ (reduce #'+ (mapcar #'cadr pts)) n)))))))

(defun log-victory (species intensity coords)
  "Appends the successful resolution to the historical archive."
  (with-open-file (out "history.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "[~A] RESOLVED: ~A at ~2F at ~A~%" 
            (get-universal-time) species intensity coords)))

(defun project-smeddum-vision (freq grid)
  (let* ((int (/ freq 200.0))
         (spec (if (> int 0.4) :wave :tongue))
         (loc (find-centroid grid)))
    (format t "~%-> SMEDDUM AWAKE: Rendering ~A at intensity ~2F at ~A~%" spec int loc)
    (log-victory spec int loc) ;; Memory Engagement
    (list spec int loc)))

(defun read-grid (path) (with-open-file (s path) (read s)))

(let* ((g (read-grid "grid.txt"))
       (a (loop for r in g sum (count-if-not #'zerop r)))
       (t-px (* (length g) (length (car g)))))
  (project-smeddum-vision (%calc-resonance a t-px) g))

(defun read-grid (path) (with-open-file (s path) (read s)))

(let* ((g (read-grid "grid.txt"))
       (a (loop for r in g sum (count-if-not #'zerop r)))
       (t-px (* (length g) (length (car g)))))
  (project-smeddum-vision (%calc-resonance a t-px) g))

(defun recall-resonance (current-int)
  "Scans the history log to see if this intensity has been encountered before."
  (let ((matches 0))
    (ignore-errors
      (with-open-file (in "history.log")
        (loop for line = (read-line in nil)
              while line do
              (when (search (format nil "~2F" current-int) line)
                (incf matches)))))
    (if (> matches 1)
        (format t "-> DÉJÀ VU: This intensity (~2F) has been resolved ~A times before.~%" 
                current-int (1- matches))
        (format t "-> NEW PHENOMENON: This resonance is unique in the archive.~%"))))

;; Update the main loop to include Recall
(let* ((g (read-grid "grid.txt"))
       (a (loop for r in g sum (count-if-not #'zerop r)))
       (t-px (* (length g) (length (car g))))
       (res (%calc-resonance a t-px))
       (intensity (/ res 200.0)))
  (recall-resonance intensity))

(defun calculate-stress-vector (grid centroid)
  "Determines the directional tension of the manifold toward the ideal center."
  (let* ((rows (length grid))
         (cols (length (car grid)))
         (ideal-r (/ (1- rows) 2.0))
         (ideal-c (/ (1- cols) 2.0))
         (dr (- ideal-r (car centroid)))
         (dc (- ideal-c (cadr centroid))))
    (format t "-> STRESS VECTOR: [~2F, ~2F] (Direction to Equilibrium)~%" dr dc)
    (list dr dc)))

;; Update the execution loop to include Optimization
(let* ((g (read-grid "grid.txt"))
       (c (find-centroid g)))
  (calculate-stress-vector g c))
