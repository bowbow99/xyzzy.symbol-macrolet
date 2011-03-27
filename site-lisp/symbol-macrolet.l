;;; -*- mode: lisp; package: lisp -*-
;;;
;;; symbol-macrolet.l --- マクロで実装した Common Lisp の symbol-macrolet
;;;
;;; Author: bowbow99 <bowbow99@gmail.com>
;;; Version: 0.01.05
;;; Category: Lisp Libraries
;;; Depends: setf-values
;;; License: MIT (see COPYING)
;;; Commentary: (see README.md)
;;; Change Log: (see ChangeLog)

;;; Code:

(require "setf-values")

(defpackage :symbol-macrolet
  (:use :lisp))

(in-package :symbol-macrolet)

(defconstant *version* "0.01.05")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export (mapcar (lambda (name) (intern (string name) :lisp))
            '(#:symbol-macrolet))
          :lisp))

;;;;
;;;; * utilities
;;;;

(defun merge-env (env type local-defs)
  "Return an environment object which contains everything in ENV plus
 local function/macros from LOCAL-DEFS."
  (evalhook (let ((env (gensym)))
              `(,type ,local-defs
                (macrolet ((,env (&environment ,env) ,env)) (,env))))
            nil nil env))

(defun shadow-symbol (sym/expansions shadowed)
  (remove-if (lambda (sym/expansion)
               (find (car sym/expansion) shadowed))
    sym/expansions))

;;;;
;;;; * Expand Forms

(defun expand-form (form sym/expansions &optional env)
  (setq form (macroexpand form env))
  (labels ((rec (form)
             (expand-form form sym/expansions env))
           (pass (fn)
             (funcall fn form sym/expansions env)))
    (if (atom form)
      (expand-atom form sym/expansions env)
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
           `(function ,(expand-lambda (cadr form) sym/expansions env))
           form))
        (lambda (pass 'expand-lambda))
        ((throw catch return-from go block)
         `(,(car form) ,(cadr form) ,@(mapcar #'rec (cddr form))))
        (tagbody (pass 'expand-tagbody))
        (t
         `(,(if (and (consp (car form))
                     (eql (caar form) 'lambda))
              (expand-lambda (car form) sym/expansions env)
              (car form))
           ,@(mapcar #'rec (cdr form))))))))


(defun expand-atom (form sym/expansions &optional env)
  (let ((found (find form sym/expansions :key #'car)))
    (if found
      (identity (cadr found))
      (identity form))))

(defun expand-body (body sym/expansions &optional env)
  (mapcar (lambda (form)
            (expand-form form sym/expansions env))
    body))

(defun expand-tagbody (form sym/expansions &optional env)
  `(tagbody
     ,@(mapcar (lambda (form)
                 (if (symbolp form) form
                   (expand-form form sym/expansions env)))
         (cdr form))))

(defun expand-macrolet (form sym/expansions &optional env)
  (labels ((expand-def (def)
             (multiple-value-bind (lambda-list% sym/expansions%)
                 (expand-lambda-list (cadr def) sym/expansions env)
               `(,(car def) ; NAME
                 ,lambda-list%
                 ,@(mapcar (lambda (form)
                             (expand-form form sym/expansions% env))
                     (cddr def))))))
    (let* ((defs (mapcar #'expand-def (cadr form)))
           (env (merge-env env 'macrolet defs)))
      `(macrolet (,@defs)
         ,@(mapcar (lambda (form)
                     (expand-form form sym/expansions env))
             (cddr form))))))

(defun expand-mv-setq (form sym/expansions &optional env)
  `(setf (values ,@(mapcar (lambda (sym)
                             (expand-atom sym sym/expansions env))
                     (cadr form)))
         ,(expand-form (caddr form) sym/expansions env)))

(defun expand-mv-bind (form sym/expansions &optional env)
  (let ((syms (cadr form))
        (values (caddr form))
        (body (cdddr form)))
    `(multiple-value-bind ,syms ,(expand-form values sym/expansions env)
       ,@(expand-form body (shadow-symbol sym/expansions syms) env))))

(defun expand-let (form sym/expansions &optional env)
  (let ((letted (mapcar (lambda (letting)
                          (if (symbolp letting) letting
                            (car letting)))
                  (cadr form))))
    `(let ,(mapcar (lambda (letting)
                     (cond ((consp letting)
                            (list (car letting)
                                  (expand-form (cadr letting) sym/expansions env)))
                           ((symbolp letting) letting)
                           (t (error "不正な let フォームです:~%~S" form))))
             (cadr form))
       ,@(mapcar (lambda (form)
                   (expand-form form (shadow-symbol sym/expansions letted) env))
           (cddr form)))))

(defun expand-let* (form sym/expansions &optional env)
  (let ((letted nil))
    (labels ((expand-with-shadow (form)
               (prog1
                   (expand-form (cadr form) (shadow-symbol sym/expansions letted) env)
                 (push (car form) letted))))
      `(let* ,(mapcar (lambda (pair)
                        (cond ((consp pair)
                               (list (car pair)
                                     (expand-with-shadow pair)))
                              ((symbolp pair) pair)
                              (t (error "不正な let* フォームです: ~S" form))))
                (cadr form))
         ,@(let ((shadowed (shadow-symbol sym/expansions letted)))
             (mapcar (lambda (form)
                       (expand-form form shadowed env))
               (cddr form)))))))


(defun expand-lambda-list (lambda-list sym/expansions &optional env)
  (macrolet ((shadow-sym/expansions (sym)
               `(setf sym/expansions (shadow-symbol sym/expansions (list ,sym)))))
    (labels ((*expand (*lambda-list &optional (state '&required))
               (cond ((null *lambda-list) nil)
                     ((symbolp *lambda-list)
                      (shadow-sym/expansions *lambda-list)
                      *lambda-list)
                     ((member (first *lambda-list) lambda-list-keywords :test #'eq)
                      (let ((new-state (first *lambda-list)))
                        (case new-state
                          ((&whole &environment)
                           (shadow-sym/expansions (second *lambda-list))
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
                           (shadow-sym/expansions param)
                           `(,(*expand param) ; possibly destructuring
                             ,@(*expand (cdr *lambda-list) state)))
                          ((&optional &key &aux)
                           `(,(cond ((symbolp param)
                                     (shadow-sym/expansions param)
                                     param)
                                    ((consp param)
                                     (let* ((len (length param))
                                            (init-form (if (>= len 2)
                                                         (expand-form (second param) sym/expansions env))))
                                       (shadow-sym/expansions (if (consp (first param))
                                                                ;; ((keysym var) ..) in &key parameters
                                                                (second (first param))
                                                                (first param)))
                                       (when (= len 3)
                                         (shadow-sym/expansions (third param)))
                                       `(,(first param)
                                         ,@(when (>= len 2) (list init-form))
                                         ,@(when (>= len 3) (list (third param)))))))
                             ,@(*expand (cdr *lambda-list) state)))))))))
      (values (*expand lambda-list)
              sym/expansions))))

(defun expand-lambda (form sym/expansions &optional env)
  (multiple-value-bind (lambda-list% sym/expansions%)
      (expand-lambda-list (cadr form) sym/expansions env)
    `(lambda ,lambda-list%
       ,@(mapcar (lambda (form)
                   (expand-form form sym/expansions% env))
           (cddr form)))))

(defun expand-flet (form sym/expansions &optional env)
  `(flet
       ;; DEFINITIONS of local functions
       ,(mapcar (lambda (def)
                  (multiple-value-bind (lambda-list% sym/expansions%)
                      (expand-lambda-list (cadr def) sym/expansions env)
                    `(,(car def) ; NAME
                      ,lambda-list%
                      ,@(mapcar (lambda (form)
                                  (expand-form form sym/expansions% env))
                          (cddr def)))))
          (cadr form))
     ;; BODY of `flet' form
     ,@(let ((new-env (merge-env env 'flet (cadr form))))
         (mapcar (lambda (form)
                   (expand-form form sym/expansions new-env))
           (cddr form)))))

(defun expand-labels (form sym/expansions &optional env)
  (let ((new-env env))
    `(labels
         ;; DEFINITIONS of local function
         ,(mapcar (lambda (def)
                    (setf new-env (merge-env new-env 'labels (list def)))
                    (multiple-value-bind (lambda-list% sym/expansions%)
                        (expand-lambda-list (cadr def) sym/expansions env)
                      `(,(car def) ; NAME
                        ,lambda-list%
                        ,@(mapcar (lambda (form)
                                    (expand-form form sym/expansions% new-env))
                            (cddr def)))))
            (cadr form))
       ;; BODY of `labels' form
       ,@(mapcar (lambda (form)
                   (expand-form form sym/expansions new-env))
           (cddr form)))))


(defmacro lisp:symbol-macrolet (sym/expansions &body forms &environment env)
  (multiple-value-bind (decl forms)
      (lisp::find-declaration forms)
    (let ((em (and decl (eql (caadr decl) 'special)
                   (intersection (cdadr decl) (mapcar #'car sym/expansions)))))
      (when em (error "declaring symbol macros as special:~{ ~S~}" em)))
    (let ((constants (remove-if-not #'constantp
                       (mapcar #'car sym/expansions))))
      (when constants
        (error "constant variables cannot be a symbol macro:~{ ~S~}" constants)))
    `(progn
      ,@(mapcar (lambda (form)
                  (symbol-macrolet::expand-form form sym/expansions env))
          forms))))
#+xyzzy
(setf (get 'symbol-macrolet 'ed:lisp-indent-hook) 1
      (get 'symbol-macrolet 'ed::lisp-indent-flet) t)

(provide "symbol-macrolet")

;;; symbol-macrolet.l ends here
