;;;; -*- mode: lisp; package: lisp -*-
;;;;
;;;; symbol-macrolet --- common-lisp's symbol-macrolet for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-07-06 03:26:10
;;;; Updated: 2009-07-11 09:51:59
;;;;

(require "setf-values")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage "symbol-macrolet"
    (:use "lisp")))

(in-package "symbol-macrolet")

(defun shadowing (sym/macros shadowed)
  (remove-if (lambda (sym/macro)
               (find (car sym/macro) shadowed))
    sym/macros))

(defun expand-atom (form sym/macros &optional env)
  (let ((found (find form sym/macros :key #'car)))
    (if found (cadr found) form)))

(defun expand-form (form sym/macros &optional env)
  (setq form (macroexpand form env))
  (labels ((rec (form)
             (expand-form form sym/macros env))
           (pass (fn)
             (funcall fn form sym/macros env)))
    (if (atom form)
        (expand-atom form sym/macros env)
      (case (car form)
        (quote form)
        (setq `(setf ,@(mapcar #'rec (cdr form))))
        (multiple-value-setq (pass 'expand-mv-setq))
        (multiple-value-bind (pass 'expand-mv-bind))
        (let (pass 'expand-let))
        (let* (pass 'expand-let*))
        ((labels flet) (pass 'expand-flet))
        (function
         `(function ,(expand-lambda (cadr form) sym/macros env))) 
        (lambda (pass 'expand-lambda))
        (t (list* (car form)
                  (mapcar #'rec (cdr form))))))))

(defun expand-mv-setq (form sym/macros &optional env)
  `(setf (values ,@(mapcar (lambda (sym)
                             (expand-atom sym sym/macros env))
                     (cadr form)))
         ,(expand-form (caddr form) sym/macros env)))

(defun expand-mv-bind (form sym/macros &optional env)
  (let ((syms (cadr form))
        (values (caddr form))
        (body (cdddr form)))
    `(multiple-value-bind ,syms ,(expand-form values sym/macros env)
       ,@(expand-form body (shadowing sym/macros syms) env))))

(defun expand-let (form sym/macros &optional env)
  (let ((letted (mapcar (lambda (letting)
                          (if (symbolp letting) letting
                            (car letting)))
                  (cadr form))))
    `(let ,(mapcar (lambda (letting)
                     (list (car letting)
                           (expand-form (cadr letting) sym/macros env)))
             (cadr form))
       ,@(expand-form (cddr form) (shadowing sym/macros letted)))))

(defun expand-let* (form sym/macros &optional env)
  (let ((letted nil))
    (labels ((expand-with-shadow (form)
               (prog1
                   (expand-form (cadr form) (shadowing sym/macros letted) env)
                 (push (car form) letted))))
    `(let* ,(mapcar (lambda (pair)
                      (list (car pair)
                            (expand-with-shadow pair)))
              (cadr form))
       ,@(let ((shadowed (shadowing sym/macros letted)))
           (mapcar (lambda (form)
                     (expand-form form shadowed env))
             (cddr form)))))))

(defun expand-lambda (form sym/macros &optional env)
  (let ((vars (cadr form))
        (body (cddr form)))
    `(lambda ,vars
       ,@(expand-form body (shadowing sym/macros vars)))))


(defun expand-flet (form sym/macros &optional env)
  `(,(car form)
    ,(mapcar (lambda (def)
               (list* (car def) ;name
                      (cadr def) ;lambda-list
                      (mapcar (lambda (form)
                                (expand-form form (shadowing sym/macros (cadr def)) env))
                        (cddr def))))
       (cadr form))
    ,@(mapcar (lambda (form)
                (expand-form form sym/macros env))
        (cddr form))))





(in-package "lisp")
(export '(symbol-macrolet))

(defmacro symbol-macrolet (defs &body forms &environment env)
  (multiple-value-bind (decl forms)
      (find-declaration forms)
    (let ((em (and decl (eql (caadr decl) 'special)
                   (intersection (cdadr decl) (mapcar #'car defs)))))
      (when em (error "declaring symbol macros as special:~{ ~S~}" em)))
    (let ((constants (remove-if-not #'constantp
                       (mapcar #'car defs))))
      (when constants
        (error "constant variables cannot be a symbol macro:~{ ~S~}" constants)))
    `(progn ,@(mapcar (lambda (form)
                        (symbol-macrolet::expand-form form defs env))
                forms))))

#+xyzzy
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1
      (get 'symbol-macrolet 'ed::lisp-indent-flet) t)

;;;; symbol-macrolet ends here
