; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/base/util.l
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
  )

(in-package :gist-mode)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (sym)
                     `(,sym (gensym)))
                 syms)
     ,@body))
(setf (get 'with-gensyms 'ed:lisp-indent-hook) 'let)

(defmacro invoke-later (dt &body body)
  `(start-timer ,dt #'(lambda () ,@body) t))
(setf (get 'invoke-later 'ed::lisp-indent-hook) '1)

(defun set-local-window-flag-alist (flags-alist &optional (window-or-buffer (selected-buffer)))
  (dolist (kv flags-alist)
    (set-local-window-flags window-or-buffer (car kv) (cdr kv))))


(provide "gist-mode/base/util")

;;; End
