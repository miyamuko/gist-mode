; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/base/render.l
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
  (mc-require "gist-mode/base/buffer")
  )

(in-package :gist-mode)

(defstruct face
  id value text-attr)

(defparameter *in-with-output-to-view* nil)
(defmacro with-output-to-view ((view) &body body)
  (with-gensyms ($buffer)
    `(if *in-with-output-to-view*
         (progn ,@body)
       (let ((*in-with-output-to-view* t)
             (,$buffer (view-buffer ,view)))
         (with-temporary-writable (,$buffer)
           (with-output-to-buffer (,$buffer (point-max))
             ,@body))))))
(setf (get 'with-output-to-view 'lisp-indent-hook) 'with-output-to-buffer)

(defun view-output-point ()
  (buffer-stream-point *standard-output*))

(defun beginning-of-view-p ()
  (zerop (view-output-point)))

(defun render (face fmt &rest args)
  (check-type face (or face symbol))
  (when (and face (symbolp face))
    (setf face (make-face :id face)))
  (let ((s (view-output-point)))
    (apply 'format t fmt args)
    (when face
      (apply 'set-text-attribute s (view-output-point) face (face-text-attr face)))))


(provide "gist-mode/base/render")

;;; End
