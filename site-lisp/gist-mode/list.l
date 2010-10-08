; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/list.l
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
  )

(in-package :gist-mode)

(export '(gist-list
          gist-list-all
          gist-list-starred
          ))


(defvar *gist-my-list-buffer-name* "*My Gists*")
(defvar *gist-all-list-buffer-name* "*All Gists*")
(defvar *gist-starred-list-buffer-name* "*Starred Gists*")

(defvar *gist-list-mode-hook* nil)

(defvar *gist-list-header-foreground-color* 8)
(defvar *gist-list-header-background-color* 15)
(defvar *gist-list-refresh-per-render* 3)

(defconstant +gist-max-parallel-requests+ 6)
(defvar *gist-number-of-parallel-requests* 3)

(defparameter *gist-list-format* "~@10A  ~@30A  ~@10A  ~A~%")
(defparameter *gist-list-mode-name* "Gist List")
(defparameter *gist-list-window-flag-alist*
  `((,*window-flag-line-number* . nil)
    (,*window-flag-newline* . nil)
    (,*window-flag-eof* . nil)
    (,*window-flag-ruler* . nil)
    (,*window-flag-just-inverse* . nil)
    (,*window-flag-cursor-line* . t)
    ))


(defstruct (list-view (:include view))
  fetcher             ; リスト取得関数
  requested-page      ; リクエストした最大のページ番号
  rendered-page       ; 描画した最大のページ番号
  end-p               ; リストの最後に到達した場合 t
  gists/page          ; 取得したすべての gist (ページ番号と gists の hashtable)
  )


(defun gist-list ()
  (interactive)
  (fetch-gists *gist-my-list-buffer-name* 'gh:list-my-gists))

(defun gist-list-all ()
  (interactive)
  (fetch-gists *gist-all-list-buffer-name* 'gh:list-all-gists))

(defun gist-list-starred ()
  (interactive)
  (fetch-gists *gist-starred-list-buffer-name* 'gh:list-starred-gists))


(defun fetch-gists (name fetcher)
  (multiple-value-bind (view created-p)
      (find-or-create-view 'list-view name :mode 'gist-list-mode)
    (pop-to-view view)
    (when created-p
      (setf (list-view-fetcher view) fetcher
            (list-view-requested-page view) 1
            (list-view-rendered-page view) 1
            (list-view-end-p view) nil
            (list-view-gists/page view) (make-hash-table :test #'eql)
            (list-view-ontimer view) #'(lambda ()
                                         (do-fetch-if-need view))
            )
      (render-list-header view))
    (do-fetch-if-need view)
    (do-fetch-if-need view)
    (do-fetch-if-need view)))

(defun do-fetch-if-need (view)
  (when (and (not (list-view-end-p view))
             (not (deleted-view-p view))
             (eql (selected-window) (get-view-window view))
             (refresh-screen)
             (pos-visible-in-window-p (point-max) (get-view-window view)))
    (let* ((max-para (min +gist-max-parallel-requests+
                          *gist-number-of-parallel-requests*))
           (num-req (- max-para (count-pending-requests view))))
      (dotimes (_ num-req)
        (do-fetch view (list-view-requested-page view))
        (incf (list-view-requested-page view)))
      )))

(defun do-fetch (view page)
  (let ((fetcher (list-view-fetcher view)))
    (do-request view fetcher
                :page page
                :callback #'(lambda (gists next)
                              (do-fetch-callback view gists page next)))))

(defun do-fetch-callback (view gists page next)
  (remove-completed-requests view)
  (unless next
    (setf (list-view-end-p view) t))
  (when gists
    (add-gists view page gists))
  (when (view-buffer-available-p view)
    (invoke-later 0.01
      (render-list-items view))))

(defun add-gists (view page gists)
  (setf (gethash page (list-view-gists/page view)) gists))


(defun list-header-face ()
  (make-face :id 'gist-list-header
             :text-attr `(:foreground ,*gist-list-header-foreground-color*
                          :background ,*gist-list-header-background-color*
                          :extend t)))

(defun list-item-face (gist)
  (make-face :id 'gist-list-item
             :value gist))

(defun render-list-header (view)
  (with-output-to-view (view)
    (when (beginning-of-view-p)
      (render (list-header-face) *gist-list-format*
              "ID" "Created" "Visibility" "Description")))
  (refresh-screen))

(defun render-list-items (view)
  (dolist (gists (group (get-render-pending-gists view)
                        *gist-list-refresh-per-render*))
    (with-output-to-view (view)
      (dolist (gist gists)
        (render (list-item-face gist) *gist-list-format*
                (gh:gist-id gist)
                (gh:gist-created-at gist)
                (if (gh:gist-public-p gist) "public" "private")
                (or (gh:gist-description gist) ""))))
    (refresh-screen)))

(defun get-render-pending-gists (view)
  (let ((r nil)
        (hash (list-view-gists/page view)))
    (loop
      (multiple-value-bind (gists found)
          (gethash (list-view-rendered-page view) hash)
        (unless found

          (return))
        (when gists
          (push gists r))
        (incf (list-view-rendered-page view))))
    (apply 'append (nreverse r))))


(defun gist-list-open-browser ()
  (interactive)
  )

(defun gist-list-show ()
  (interactive)
  )

(defun gist-list-quit ()
  (interactive)
  (delete-view (selected-view)))

(define-keymap *gist-list-keymap* (make-sparse-keymap)
  (#\j 'next-line)
  (#\k 'previous-line)
  (#\v 'gist-list-open-browser)
  (#\q 'gist-list-quit)

  (#\RET 'gist-list-show)
  (#\SPC 'next-page)
  )

(defun gist-list-mode ()
  (kill-all-local-variables)
  (setf buffer-mode 'gist-list-mode)
  (setf mode-name *gist-list-mode-name*)
  (set-local-window-flag-alist *gist-list-window-flag-alist* (selected-buffer))
  (use-keymap *gist-list-keymap*)
  (run-hooks '*gist-list-mode-hook*)
  )


(provide "gist-mode/list")

;;; End
