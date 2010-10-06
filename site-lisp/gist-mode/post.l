; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/post.l
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

(export '(gist-region
          gist-region-private
          gist-buffer
          gist-buffer-private
          ))

(defun gist-region (s e)
  (interactive "r")
  (gist-post s e :private nil))

(defun gist-region-private (s e)
  (interactive "r")
  (gist-post s e :private t))

(defun gist-buffer ()
  (interactive "r")
  (gist-post (point-min) (point-max) :private nil))

(defun gist-buffer-private ()
  (interactive "r")
  (gist-post (point-min) (point-max) :private t))


(defun gist-post (s e &key private)
  (if (< e s) (rotatef s e))
  (let ((contents (buffer-substring s e))
        (filetype (gh:guess-gist-filetype (selected-buffer))))
    (message "e...")
    (gh:create-gist contents :lang (car filetype) :private private
                    :callback #'gist-post-callback)))

(defun gist-post-callback (id)
  (if id
      (let ((url (gh:gist-home-url id)))
        (copy-to-clipboard url)
        (message "投稿しました: ~A" url))
    (message "投稿失敗")))


(provide "gist-mode/post")

;;; End
