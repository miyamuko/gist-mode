; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/edit.l
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
  (mc-require "gist-mode/base/view")
  (mc-require "gist-mode/base/render")
  (mc-require "gist-mode/base/keymap")
  (mc-require "gist-mode/common")
  )

(in-package :gist-mode)

(export '(gist-edit
          ))

(defvar *gist-edit-buffer-name* "*Gist ~A*")

(defvar *gist-edit-mode-hook* nil)

(defparameter *gist-edit-mode-name* "Gist Edit")

(defstruct (edit-view (:include view))
  gist-id
  gist-meta
  gist-contents
  read-only
  )

(defun gist-edit (gist)
  (interactive)
  (check-type gist (or gh:gist string integer))
  (let ((id (if (gh:gist-p gist)
                (gh:gist-id gist)
              (string gist))))
    (multiple-value-bind (view created-p)
        (find-or-create-view 'edit-view (format nil *gist-edit-buffer-name* id)
                             :mode 'gist-edit-mode)
      (pop-to-view view)
      (when created-p
        (setf (edit-view-gist-id view) id)
        (when (gh:gist-p gist)
          (setf (edit-view-gist-meta view) gist)))
      (do-fetch-contents view))))

(defun do-fetch-contents (view)
  (gh:show-gist (edit-view-gist-id view)
                :callback #'(lambda (contents)
                              (do-fetch-contents-callback view contents))))

(defun do-fetch-contents-callback (view contents)
  (setf (edit-view-gist-contents view) contents)
  (with-output-to-view (view)
    (render contents)))

(defun gist-edit-open-home-by-browser ()
  (interactive)
  (gist-edit-open-browser 'gh:gist-home-url))

(defun gist-edit-open-edit-by-edit ()
  (interactive)
  (gist-edit-open-browser 'gh:gist-edit-url))

(defun gist-edit-open-browser (fn)
  (let* ((view (selected-view))
         (url (funcall fn (or (edit-view-gist-meta view)
                              (edit-view-gist-id view)))))
    (message "Open ~A" url)
    (shell-execute url t)))

(define-keymap *gist-edit-keymap* (make-sparse-keymap)
  ('(#\C-c #\u) 'gist-edit-upload)
  ('(#\C-c #\v) 'gist-edit-open-home-by-browser)
  ('(#\C-c #\e) 'gist-edit-open-edit-by-edit)
  (#\F5 'gist-edit-reload)
  )

(defun gist-edit-mode ()
  (kill-all-local-variables)
  (setf buffer-mode 'gist-edit-mode)
  (setf mode-name *gist-edit-mode-name*)
  (use-keymap *gist-edit-keymap*)
  (run-hooks '*gist-edit-mode-hook*)
  )


(provide "gist-mode/edit")

;;; End
