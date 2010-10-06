; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/base/keymap.l
;;
;; Copyright (c) 2010 MIYAMUKO Katsuyuki.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mc-require "gist-mode/package")
  (mc-require "gist-mode/base/util")
  )

(in-package :gist-mode)

(defmacro with-keymap (keymap &body body)
  (with-gensyms ($keymap)
    `(let ((,$keymap ,keymap))
       ,@(mapcar #'(lambda (form)
                     (if (cadr form)
                         `(define-key ,$keymap ,@form)
                       `(undefine-key ,$keymap ,(car form))))
                 body)
       ,$keymap)))
(setf (get 'with-keymap 'ed:lisp-indent-hook) 'let)

(defmacro define-keymap (name base &body body)
  `(progn
     (defvar ,name nil)
     (setf ,name
           (with-keymap ,base
             ,@body))))
(setf (get 'define-keymap 'ed::lisp-indent-hook) 'defun)


(provide "gist-mode/base/keymap")

;;; End
