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

(require "setf-values")

(defpackage :symbol-macrolet
  (:use :lisp))

(in-package :symbol-macrolet)

(defconstant *version* "0.01.04")

(export '(symbol-macrolet))

;;;;
;;;; * utilities
;;;;

(defmacro progn? (&body body)
  (if (> (length body) 1) `(progn ,@body) (car body)))

(defun merge-env (env type local-defs)
  "Return an environment object which contains everything in ENV plus
 local function/macros from LOCAL-DEFS."
  (evalhook (let ((env (gensym)))
              `(,type ,local-defs
                (macrolet ((,env (&environment ,env) ,env)) (,env))))
            nil nil env))

(defun shadowing (sym/macros shadowed)
  (remove-if (lambda (sym/macro)
               (find (car sym/macro) shadowed))
    sym/macros))

;;;;
;;;; * Expand Forms

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
        (macrolet (pass 'expand-macrolet))
        (flet (pass 'expand-flet))
        (labels (pass 'expand-labels))
        (function
         (if (consp (cadr form))
           `(function ,(expand-lambda (cadr form) sym/macros env))
           form))
        (lambda (pass 'expand-lambda))
        ((throw catch return-from go block)
         `(,(car form) ,(cadr form) ,@(mapcar #'rec (cddr form))))
        (tagbody (pass 'expand-tagbody))
        (t
         `(,(if (and (consp (car form))
                     (eql (caar form) 'lambda))
              (expand-lambda (car form) sym/macros env)
              (car form))
           ,@(mapcar #'rec (cdr form))))))))


(defun expand-atom (form sym/macros &optional env)
  (let ((found (find form sym/macros :key #'car)))
    (if found
      (identity (cadr found))
      (identity form))))

(defun expand-tagbody (form sym/macros &optional env)
  `(tagbody
     ,@(mapcar (lambda (form)
                 (if (symbolp form) form
                   (expand-form form sym/macros env)))
         (cdr form))))

(defun expand-macrolet (form sym/macros &optional env)
  (labels ((expand-def (form)
             `(,(car form) ,#1=(cadr form) ;NAME ARGS
               ,@(let ((sym/macros (shadowing sym/macros #1#)))
                   (mapcar (lambda (form)
                             (expand-form form sym/macros env))
                     (cddr form))))))
    (let* ((defs (mapcar #'expand-def (cadr form)))
           (env (merge-env env 'macrolet defs)))
      `(macrolet (,@defs)
         ,@(mapcar (lambda (form)
                     (expand-form form sym/macros env))
             (cddr form))))))

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
                     (cond ((consp letting)
                            (list (car letting)
                                  (expand-form (cadr letting) sym/macros env)))
                           ((symbolp letting) letting)
                           (t (error "不正な let フォームです:~%~S" form))))
             (cadr form))
       ,@(mapcar (lambda (form)
                   (expand-form form (shadowing sym/macros letted) env))
           (cddr form)))))

(defun expand-let* (form sym/macros &optional env)
  (let ((letted nil))
    (labels ((expand-with-shadow (form)
               (prog1
                   (expand-form (cadr form) (shadowing sym/macros letted) env)
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
                       (expand-form form shadowed env))
               (cddr form)))))))


(defun expand-lambda-list (lambda-list sym/macros &optional env)
  (macrolet ((shadow-sym/macros (sym)
               `(setf sym/macros (shadowing sym/macros (list ,sym)))))
    (labels ((*expand (*lambda-list &optional (state '&required))
               (cond ((null *lambda-list) nil)
                     ((symbolp *lambda-list)
                      (shadow-sym/macros *lambda-list)
                      *lambda-list)
                     ((member (first *lambda-list) lambda-list-keywords :test #'eq)
                      (let ((new-state (first *lambda-list)))
                        (case new-state
                          ((&whole &environment)
                           (shadow-sym/macros (second *lambda-list))
                           `(,new-state
                             ,(second *lambda-list)
                             ,@(*expand (cddr *lambda-list) state)))
                          (&rest
                           `(,new-state
                             ,(*expand (second *lambda-list)) ; possibly destructuring
                             ,@(*expand (cddr *lambda-list))))
                          (t
                           `(,new-state ,@(*expand (cdr *lambda-list) new-state))))))
                     (t
                      (let ((param (first *lambda-list)))
                        (case state
                          (&required
                           (shadow-sym/macros param)
                           `(,(*expand param) ; possibly destructuring
                             ,@(*expand (cdr *lambda-list) state)))
                          ((&optional &key &aux)
                           `(,(cond ((symbolp param)
                                     (shadow-sym/macros param)
                                     param)
                                    ((consp param)
                                     (let* ((len (length param))
                                            (init-form (if (>= len 2)
                                                         (expand-form (second param) sym/macros env))))
                                       (shadow-sym/macros (if (consp (first param))
                                                            ;; ((keysym var) ..) in &key parameters
                                                            (second (first param))
                                                            (first param)))
                                       (when (= len 3)
                                         (shadow-sym/macros (third param)))
                                       `(,(first param)
                                         ,@(when (>= len 2) (list init-form))
                                         ,@(when (>= len 3) (list (third param)))))))
                             ,@(*expand (cdr *lambda-list) state)))))))))
      (values (*expand lambda-list)
              sym/macros))))

(defun expand-lambda (form sym/macros &optional env)
  (multiple-value-bind (lambda-list% sym/macros%)
      (expand-lambda-list (cadr form) sym/macros env)
    `(lambda ,lambda-list%
       ,@(mapcar (lambda (form)
                   (expand-form form sym/macros% env))
           (cddr form)))))

(defun expand-flet (form sym/macros &optional env)
  `(flet
       ;; DEFINITIONS of local functions
       ,(mapcar (lambda (def)
                  (multiple-value-bind (lambda-list% sym/macros%)
                      (expand-lambda-list (cadr def) sym/macros env)
                    `(,(car def) ; NAME
                      ,lambda-list%
                      ,@(mapcar (lambda (form)
                                  (expand-form form sym/macros% env))
                          (cddr def)))))
          (cadr form))
     ;; BODY of `flet' form
     ,@(let ((new-env (merge-env env 'flet (cadr form))))
         (mapcar (lambda (form)
                   (expand-form form sym/macros new-env))
           (cddr form)))))

(defun expand-labels (form sym/macros &optional env)
  (let ((new-env env))
    `(labels
         ;; DEFINITIONS of local function
         ,(mapcar (lambda (def)
                    (setf new-env (merge-env new-env 'labels (list def)))
                    (multiple-value-bind (lambda-list% sym/macros%)
                        (expand-lambda-list (cadr def) sym/macros env)
                      `(,(car def) ; NAME
                        ,lambda-list%
                        ,@(mapcar (lambda (form)
                                    (expand-form form sym/macros% new-env))
                            (cddr def)))))
            (cadr form))
       ;; BODY of `labels' form
       ,@(mapcar (lambda (form)
                   (expand-form form sym/macros new-env))
           (cddr form)))))


(defmacro symbol-macrolet (sym/macros &body forms &environment env)
  (multiple-value-bind (decl forms)
      (lisp::find-declaration forms)
    (let ((em (and decl (eql (caadr decl) 'special)
                   (intersection (cdadr decl) (mapcar #'car sym/macros)))))
      (when em (error "declaring symbol macros as special:~{ ~S~}" em)))
    (let ((constants (remove-if-not #'constantp
                       (mapcar #'car sym/macros))))
      (when constants
        (error "constant variables cannot be a symbol macro:~{ ~S~}" constants)))
    `(symbol-macrolet::progn?
      ,@(mapcar (lambda (form)
                  (symbol-macrolet::expand-form form sym/macros env))
          forms))))

#+xyzzy
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1
      (get 'symbol-macrolet 'ed::lisp-indent-flet) t)

(provide "symbol-macrolet")
;;;; symbol-macrolet.l ends here
