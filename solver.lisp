;;; ===========================================================================
;;; SYMBOLIC-ARC: An Entity-Based Solver
;;; Developed for the Abstraction and Reasoning Corpus.
;;; ===========================================================================

(defstruct arc-object
  "Represents a discrete entity within the grid."
  pixels   ; List of (row . col) pairs
  color)   ; Integer color ID

;;; --- GEOMETRY EXPERTS ---

(defun get-bounding-box (pixels)
  "Returns (min-r min-c max-r max-c) for a coordinate set."
  (let ((min-r (caar pixels)) (min-c (cdar pixels))
        (max-r (caar pixels)) (max-c (cdar pixels)))
    (dolist (p pixels (list min-r min-c max-r max-c))
      (let ((r (car p)) (c (cdr p)))
        (setf min-r (min min-r r) min-c (min min-c c)
              max-r (max max-r r) max-c (max max-c c))))))

(defun rotate-object-90 (obj)
  "Rotates an object 90deg CW and normalizes to (0,0)."
  (let* ((pixels (arc-object-pixels obj))
         ;; Matrix Transform: (r, c) -> (c, -r)
         (new-pixels (loop for p in pixels 
                           collect (cons (cdr p) (- (car p)))))
         (bbox (get-bounding-box new-pixels))
         (min-r (first bbox))
         (min-c (second bbox))
         ;; Normalization (Translation Invariance)
         (normalized (loop for p in new-pixels
                           collect (cons (- (car p) min-r) 
                                         (- (cdr p) min-c)))))
    (make-arc-object :pixels normalized 
                     :color (arc-object-color obj))))

;;; --- INFERENCE HELPERS ---

(defun get-orientation (obj)
  "Returns :tall if height > width, else :wide."
  (destructuring-bind (r1 c1 r2 c2) (get-bounding-box (arc-object-pixels obj))
    (if (> (- r2 r1) (- c2 c1)) :tall :wide)))

(defun print-grid-coords (pixels width height)
  "Utility to render coordinate sets back to a visual grid."
  (loop for r from 0 below height do
    (format t "~&")
    (loop for c from 0 below width do
      (if (member (cons r c) pixels :test #'equal)
          (format t "X ")
          (format t ". "))))
  (values))

(defun get-orientation (obj)
  "Returns :tall if height > width, else :wide."
  (destructuring-bind (r1 c1 r2 c2) (get-bounding-box (arc-object-pixels obj))
    (if (> (- r2 r1) (- c2 c1)) :tall :wide)))

(defun print-grid-coords (pixels width height)
  "Utility to render coordinate sets back to a visual grid.
  Uses 'X' for pixels and '.' for empty space."
  (loop for r from 0 below height do
    (format t "~&")
    (loop for c from 0 below width do
      (if (member (cons r c) pixels :test #'equal)
          (format t "X ")
          (format t ". "))))
  (terpri)
  (values))

(defun flood-fill (grid r c target-color visited)
  "Explores a contiguous colored area and returns its coordinates."
  (let ((stack (list (cons r c)))
        (component nil))
    (loop while stack do
      (let* ((pos (pop stack))
             (curr-r (car pos))
             (curr-c (cdr pos)))
        (when (and (>= curr-r 0) (< curr-r (length grid))
                   (>= curr-c 0) (< curr-c (length (car grid)))
                   (= (nth curr-c (nth curr-r grid)) target-color)
                   (not (member pos visited :test #'equal))
                   (not (member pos component :test #'equal)))
          (push pos component)
          (push (cons (1+ curr-r) curr-c) stack)
          (push (cons (1- curr-r) curr-c) stack)
          (push (cons curr-r (1+ curr-c)) stack)
          (push (cons curr-r (1- curr-c)) stack))))
    component))

(defun get-objects (grid)
  "Scans the grid and returns a list of arc-object structs.
  Assumes color 0 is the background (noise)."
  (let ((visited nil)
        (objects nil))
    (loop for r from 0 below (length grid) do
      (loop for c from 0 below (length (car grid)) do
        (let ((val (nth c (nth r grid))))
          (unless (or (= 0 val) (member (cons r c) visited :test #'equal))
            (let ((comp (flood-fill grid r c val visited)))
              (setf visited (append comp visited))
              (push (make-arc-object :pixels comp :color val) objects))))))
    objects))

(defun apply-transformation (obj rule)
  "Applies a symbolic rule to an arc-object. 
  RULE can be :rotate-90, :identity, etc."
  (case rule
    (:rotate-90 (rotate-object-90 obj))
    (otherwise obj)))

(defun assemble-grid (objects width height)
  "Creates a fresh grid of WIDTH x HEIGHT and paints all objects onto it."
  (let ((grid (loop repeat height collect (make-list width :initial-element 0))))
    (dolist (obj objects)
      (dolist (p (arc-object-pixels obj))
        (let ((r (car p)) (c (cdr p)))
          (when (and (>= r 0) (< r height) (>= c 0) (< c width))
            (setf (nth c (nth r grid)) (arc-object-color obj))))))
    grid))

(defun flip-object-h (obj)
  "Reflects the object horizontally within its own bounding box."
  (let* ((pixels (arc-object-pixels obj))
         (bbox (get-bounding-box pixels))
         (min-c (second bbox))
         (max-c (fourth bbox)))
    (make-arc-object
     :color (arc-object-color obj)
     :pixels (mapcar (lambda (p)
                       (cons (car p) (- max-c (- (cdr p) min-c))))
                     pixels))))

(defun flip-object-v (obj)
  "Reflects the object vertically within its own bounding box."
  (let* ((pixels (arc-object-pixels obj))
         (bbox (get-bounding-box pixels))
         (min-r (first bbox))
         (max-r (third bbox)))
    (make-arc-object
     :color (arc-object-color obj)
     :pixels (mapcar (lambda (p)
                       (cons (- max-r (- (car p) min-r)) (cdr p)))
                     pixels))))

(defun apply-transformation (obj rule)
  "SYMLOGIC: Dispatches the correct geometric expert based on the rule.
  Supports the full D4 Symmetry Group (Identity, Rotations, Reflections)."
  (case rule
    (:rotate-90 (rotate-object-90 obj))
    (:flip-h    (flip-object-h obj))
    (:flip-v    (flip-object-v obj))
    (:identity  obj)
    (otherwise  obj)))

(defun solve-task (grid rule)
  "The Master Pipeline:
  1. PERCEPTION: Extract objects from the grid.
  2. REASONING: Apply the symbolic RULE to each object.
  3. ACTUATION: Reassemble the objects into a final grid."
  (let* ((height (length grid))
         (width (length (car grid)))
         (input-objects (get-objects grid))
         (output-objects (mapcar (lambda (obj) 
                                   (apply-transformation obj rule)) 
                                 input-objects)))
    (assemble-grid output-objects width height)))
