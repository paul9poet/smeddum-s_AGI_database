(in-package :cl-user)
(require :uiop)

(defun filter-to-pure-digits (str)
  (remove-if-not #'digit-char-p str))

(defun invoke-smeddum-primitive (primitive-name input-file)
  (let* ((bin-path (merge-pathnames (format nil "binaries/~A" primitive-name) (uiop:getcwd)))
         (command (format nil "~A ~A > output.txt" (namestring bin-path) input-file)))
    (uiop:run-program command :output t :error-output t)))

(defun compare-grids (file-a file-b)
  (let* ((digits-a (filter-to-pure-digits (uiop:read-file-string file-a)))
         (digits-b (filter-to-pure-digits (uiop:read-file-string file-b)))
         (final-a (if (>= (length digits-a) 9) (subseq digits-a (- (length digits-a) 9)) digits-a))
         (final-b (if (>= (length digits-b) 9) (subseq digits-b (- (length digits-b) 9)) digits-b)))
    (string= final-a final-b)))

(defun smeddum-evaluator (primitive-list input-file target-file)
  (dolist (prim primitive-list)
    (invoke-smeddum-primitive prim input-file)
    (if (compare-grids "output.txt" target-file)
        (progn
          (format t "~&[!] SOLUTION FOUND: ~A solves this task!~%" prim)
          (return prim))
        (format t "~&[?] ~A did not match.~%" prim))))
