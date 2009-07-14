;;;; -*- mode: lisp; package: lisp -*-
;;;;
;;;; symbol-macrolet.l --- common-lisp's symbol-macrolet for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-07-06 03:26:10
;;;; Updated: 2009-07-14 13:41:57
;;;;
;;;; 

(require "setf-values")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage "symbol-macrolet"
    (:use "lisp")))

(in-package "symbol-macrolet")

(defconstant *version* "0.01.00")

;;;;
;;;; * utilities
;;;;

(defmacro progn? (&body body)
  (if (> (length body) 1) `(progn ,@body) (car body)))

(defun defs->env (defs &optional form)
  "Returns environment-object which contains local macros of <defs>."
  (cond ((null defs) (eval form))
        ((null form)
         (let ((env (gensym)))
           (defs->env defs
                      `(macrolet ((,env (&environment ,env) ,env)) (,env)))))
        (t (defs->env (cdr defs)
                      `(macrolet ,(car defs) ,form)))))

(defun macroexpand* (form &key defs env)
  "Returns expansion of <form> in gine <env> and local macro <defs>."
  (let ((global (macroexpand form))
        (outer (macroexpand form env))
        (inner (macroexpand form (defs->env defs))))
    (or (find-if (lambda (expansion)
                   (not (equal expansion global)))
          (list inner outer))
        global)))

(defun shadowing (sym/macros shadowed)
  (remove-if (lambda (sym/macro)
               (find (car sym/macro) shadowed))
    sym/macros))

(defun expand-atom (form sym/macros &optional env defs)
  (let ((found (find form sym/macros :key #'car)))
    (if found (cadr found) form)))

(defun expand-form (form sym/macros &optional env defs)
  (setq form (macroexpand* form :env env :defs defs))
  (labels ((rec (form)
             (expand-form form sym/macros env defs))
           (pass (fn)
             (funcall fn form sym/macros env defs)))
    (if (atom form)
        (expand-atom form sym/macros env defs)
      (case (car form)
        (quote form)
        (setq `(setf ,@(mapcar #'rec (cdr form))))
        (multiple-value-setq (pass 'expand-mv-setq))
        (multiple-value-bind (pass 'expand-mv-bind))
        (let (pass 'expand-let))
        (let* (pass 'expand-let*))
        (macrolet (pass 'expand-macrolet))
        ((labels flet) (pass 'expand-flet))
        (function
         `(function ,(expand-lambda (cadr form) sym/macros env))) 
        (lambda (pass 'expand-lambda))
        ((throw catch return-from go block)
         `(,(car form) ,(cadr form) ,@(mapcar #'rec (cddr form))))
        (tagbody (pass 'expand-tagbody))
        (t (list* (car form)
                  (mapcar #'rec (cdr form))))))))

(defun expand-tagbody (form sym/macros &optional env defs)
  `(tagbody
     ,@(mapcar (lambda (form)
                 (if (symbolp form) form
                   (expand-form form sym/macros env defs)))
         (cdr form))))

(defun expand-macrolet (form sym/macros &optional env defs)
  (labels ((expand-def (form)
             `(,(car form) ,#1=(cadr form) ;NAME ARGS
               ,@(let ((sym/macros (shadowing sym/macros #1#)))
                   (mapcar (lambda (form)
                             (expand-form form sym/macros env defs))
                     (cddr form))))))
    (let ((defs (cons (mapcar #'expand-def (cadr form)) defs)))
      `(progn? ,@(mapcar (lambda (form)
                           (expand-form form sym/macros env defs))
                   (cddr form))))))

(defun expand-mv-setq (form sym/macros &optional env defs)
  `(setf (values ,@(mapcar (lambda (sym)
                             (expand-atom sym sym/macros env defs))
                     (cadr form)))
         ,(expand-form (caddr form) sym/macros env defs)))

(defun expand-mv-bind (form sym/macros &optional env defs)
  (let ((syms (cadr form))
        (values (caddr form))
        (body (cdddr form)))
    `(multiple-value-bind ,syms ,(expand-form values sym/macros env defs)
       ,@(expand-form body (shadowing sym/macros syms) env defs))))

(defun expand-let (form sym/macros &optional env defs)
  (let ((letted (mapcar (lambda (letting)
                          (if (symbolp letting) letting
                            (car letting)))
                  (cadr form))))
    `(let ,(mapcar (lambda (letting)
                     (list (car letting)
                           (expand-form (cadr letting) sym/macros env defs)))
             (cadr form))
       ,@(mapcar (lambda (form)
                   (expand-form form (shadowing sym/macros letted) env defs))
           (cddr form)))))

(defun expand-let* (form sym/macros &optional env defs)
  (let ((letted nil))
    (labels ((expand-with-shadow (form)
               (prog1
                   (expand-form (cadr form) (shadowing sym/macros letted)
                                env defs)
                 (push (car form) letted))))
    `(let* ,(mapcar (lambda (pair)
                      (list (car pair)
                            (expand-with-shadow pair)))
              (cadr form))
       ,@(let ((shadowed (shadowing sym/macros letted)))
           (mapcar (lambda (form)
                     (expand-form form shadowed env defs))
             (cddr form)))))))

(defun expand-lambda (form sym/macros &optional env defs)
  (let ((vars (cadr form))
        (body (cddr form)))
    `(lambda ,vars
       ,@(expand-form body (shadowing sym/macros vars) env defs))))


(defun expand-flet (form sym/macros &optional env defs)
  `(,(car form)
    ,(mapcar (lambda (def)
               (list* (car def) ;name
                      (cadr def) ;lambda-list
                      (mapcar (lambda (form)
                                (expand-form form (shadowing sym/macros (cadr def))
                                             env defs))
                        (cddr def))))
       (cadr form))
    ,@(mapcar (lambda (form)
                (expand-form form sym/macros env defs))
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
    `(symbol-macrolet::progn?
      ,@(mapcar (lambda (form)
                  (symbol-macrolet::expand-form form defs env))
          forms))))

#+xyzzy
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1
      (get 'symbol-macrolet 'ed::lisp-indent-flet) t)

(provide "symbol-macrolet")
;;;; symbol-macrolet.l ends here
