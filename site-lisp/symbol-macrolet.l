;;;; -*- mode: lisp; package: lisp -*-
;;;;
;;;; symbol-macrolet.l --- common-lisp's symbol-macrolet for xyzzy-lisp.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;;
;;;; License: The MIT License (http://www.opensource.org/licenses/mit-license.php)
#|
Copyright (c) 2009 bowbow99

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(require :setf-values)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :symbol-macrolet
    (:use :lisp)))

(in-package :symbol-macrolet)

(defconstant *version* "0.01.04")

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
    (if found
        (identity (cadr found))
      (identity form))))

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
         (if (consp (cadr form))
             `(function ,(expand-lambda (cadr form) sym/macros env))
           form))
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
                     (cond ((consp letting)
                            (list (car letting)
                                  (expand-form (cadr letting) sym/macros env defs)))
                           ((symbolp letting) letting)
                           (t (error "不正な let フォームです:~%~S" form))))
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
                        (cond ((consp pair)
                                 (list (car pair)
                                       (expand-with-shadow pair)))
                              ((symbolp pair) pair)
                              (t (error "不正な let* フォームです: ~S" form))))
                (cadr form))
         ,@(let ((shadowed (shadowing sym/macros letted)))
             (mapcar (lambda (form)
                       (expand-form form shadowed env defs))
               (cddr form)))))))

(defun expand-lambda (form sym/macros &optional env defs)
  (let ((vars (cadr form))
        (body (cddr form)))
    `(lambda ,vars
       ,@(mapcar (lambda (form)
                   (expand-form form (shadowing sym/macros vars) env defs))
           body))))


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
    ;(ed::msgbox "~{~S~%~}" defs)
    `(symbol-macrolet::progn?
      ,@(mapcar (lambda (form)
                  (symbol-macrolet::expand-form form defs env))
          forms))))

#+xyzzy
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1
      (get 'symbol-macrolet 'ed::lisp-indent-flet) t)

(provide "symbol-macrolet")
;;;; symbol-macrolet.l ends here
