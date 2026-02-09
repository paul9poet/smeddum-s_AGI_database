(in-package :smeddum-core)

(defparameter *max-recursive-depth* 3)

(defstruct arc-rule
  "A symbolic representation of an ARC transformation."
  (transformation nil)
  (complexity 0)  ; The 'Description Length'
  (verified-p nil))

(defun calculate-mdl (rule)
  "Calculates the complexity of a rule. Lower is better."
  ;; Simplest rules: Identity, 90-degree rotation, Reflection.
  ;; Complex rules: Conditional color changes, recursive nesting.
  (length (serialize-rule rule)))

(defun audit-task (task-json)
  "The primary entry point for the Tiny Recursive Machine."
  (let* ((demo-pairs (get-demo-pairs task-json))
         (candidates (generate-candidates demo-pairs)))
    ;; Sort by Minimum Description Length (MDL)
    (setf candidates (sort candidates #'< :key #'calculate-mdl))
    
    (loop for rule in candidates
          do (when (verify-rule-on-demos rule demo-pairs)
               (format t "~&[SUCCESS] Invariant found with MDL: ~A" (calculate-mdl rule))
               (return (apply-rule (get-test-input task-json) rule))))))

(defun verify-rule-on-demos (rule demos)
  "Strictly tied to the ARC project: the rule must work for ALL training pairs."
  (every #'(lambda (pair)
             (grid-equal (apply-rule (input pair) rule)
                         (output pair)))
         demos))
