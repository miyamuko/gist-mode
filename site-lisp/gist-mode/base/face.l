; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/base/face.l
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

(defstruct face
  id value text-attr)


(defmacro with-face ((face) &body body)
  (with-gensyms ($face $start)
    `(let ((,$face ,face))
       (check-type ,$face (or face symbol))
       (when (and ,$face (symbolp ,$face))
         (setf ,$face (make-face :id ,$face)))
       (let ((,$start (view-point)))
         (progn ,@body)
         (when ,$face
           (apply #'set-text-attribute ,$start (view-point)
                  ,$face (face-text-attr ,$face)))))))
(setf (get 'with-face 'lisp-indent-hook) 'with-output-to-buffer)

(defun current-face ()
  (multiple-value-bind (from to face)
      (find-face #'(lambda (from to _)
                     (<= from (point) to))
                 :reverse t)
    face))

(defun find-face-forward (face &key (key 'identity) (test 'eql) no-dup)
  (let ((start (point)))
    (find-face #'(lambda (from to found-face)
                   (and (funcall test (funcall key found-face) face)
                        (if no-dup
                            (< start from)
                          (<= start to))))
               :reverse nil)))

(defun find-face-backward (face &key (key 'identity) (test 'eql) no-dup)
  (let ((end (point)))
    (find-face #'(lambda (from to found-face)
                   (and (funcall test (funcall key found-face) face)
                        (if no-dup
                            (< to end)
                          (<= from end))))
               :reverse t)))

(defun find-face (test &key reverse)
  (let ((r (find-if #'(lambda (attr)
                        (multiple-value-bind (from to found-face)
                            (values-list attr)
                          (and (face-p found-face)
                               (funcall test from to found-face))))
                    (if reverse
                        (nreverse (list-text-attributes (point-min) (+ (point) 1000)))
                      (list-text-attributes (- (point) 1000) (point-max))))))
    (when r
      (values-list r))))


(provide "gist-mode/base/face")

;;; End
