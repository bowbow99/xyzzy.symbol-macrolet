;;;; -*- mode: lisp -*-
;;;;
;;;; symbol-macrolet --- common-lisp's symbol-macrolet for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-07-06 03:26:10
;;;; Updated: 2009-07-06 03:26:32
;;;;

(defmacro symbol-macrolet (defs &body body)
  (labels ((rplatom (old new forms)
             (cond ((null forms) nil)
                   ((atom forms)
                    (if (eq forms old) new forms))
                   (t (cons (rplatom old new (car forms))
                            (rplatom old new (cdr forms)))))))
    `(progn ,@(dolist (def (reverse defs) body)
                (setf body (rplatom (car def) (cadr def) body))))))

;;;; symbol-macrolet ends here
