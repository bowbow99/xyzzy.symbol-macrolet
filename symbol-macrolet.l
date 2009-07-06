;;;; -*- mode: lisp; package: lisp -*-
;;;;
;;;; symbol-macrolet --- common-lisp's symbol-macrolet for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-07-06 03:26:10
;;;; Updated: 2009-07-06 16:25:12
;;;;

(in-package "lisp")
(export '(symbol-macrolet))

(defmacro symbol-macrolet (defs &body body)
  (labels ((rplatom (old new forms)
             (cond ((eq forms old) new)
                   ((eq forms 'setq) 'setf)
                   ((eq forms 'psetq) 'psetf)
                   ((atom forms) forms)
                   (t (cons (rplatom old new (car forms))
                            (rplatom old new (cdr forms)))))))
    `(progn ,@(dolist (def (reverse defs) body)
                (setf body (rplatom (car def) (cadr def) body))))))

#+xyzzy
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1
      (get 'symbol-macrolet 'ed::lisp-indent-flet) t)

;;;; symbol-macrolet ends here
