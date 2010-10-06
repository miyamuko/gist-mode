; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/base/buffer.l
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

(defmacro with-selected-buffer ((buffer) &body body)
  (with-gensyms ($original $buffer)
    `(let ((,$original (selected-buffer))
           (,$buffer ,buffer))
       (with-set-buffer
         (if (eq ,$original ,$buffer)
             (progn ,@body)
           (unwind-protect
               (progn
                 (set-buffer ,$buffer)
                 ,@body)
             (set-buffer ,$original)))))))
(setf (get 'with-selected-buffer 'ed:lisp-indent-hook) 'with-selected-window)

(defmacro with-temporary-writable ((buffer) &body body)
  `(with-selected-buffer (,buffer)
     (let ((read-only buffer-read-only))
       (setf buffer-read-only nil)
       (unwind-protect
           (progn ,@body)
         (setf buffer-read-only read-only)))))
(setf (get 'with-temporary-writable 'lisp-indent-hook) 'with-selected-window)


(provide "gist-mode/base/buffer")

;;; End
