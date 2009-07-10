;;;; -*- mode: lisp; package: lisp -*-
;;;;
;;;; symbol-macrolet --- common-lisp's symbol-macrolet for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-07-06 03:26:10
;;;; Updated: 2009-07-11 01:07:01
;;;;

(in-package "lisp")
(export '(symbol-macrolet))

(defmacro symbol-macrolet (defs &body body)
  (labels ((modify (old new forms)
             (cond ((eq forms old) new)
                   ((eq forms 'setq) 'setf)
                   ((eq forms 'psetq) 'psetf)
                   ((atom forms) forms)
                   (t (cons (modify old new (car forms))
                            (modify old new (cdr forms)))))))
    `(progn ,@(dolist (def (reverse defs) body)
                (setf body (modify (car def) (cadr def) body))))))

#+xyzzy
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1
      (get 'symbol-macrolet 'ed::lisp-indent-flet) t)

;;;; symbol-macrolet ends here
