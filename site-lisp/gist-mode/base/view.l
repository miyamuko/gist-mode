; -*- mode: lisp; package: gist-mode; encoding: utf-8 -*-

;;; gist-mode/base/view.l
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

(defvar *view-id* 0)
(defparameter *view-timer-interval* 0.5)
(defvar *view-timer-started-p* nil)

(defvar-local @view nil)

;; view とはバッファを一段抽象化したようなもの。
;; view はバッファローカル変数でバッファごとに作成する。
;; 処理に必要な情報はすべて view 構造体に詰め込んでいるので、
;; view ひとつのみ引数として指定すれば OK

(defstruct view
  (id (incf *view-id*))      ; id
  buffer                     ; view に関連づいたバッファ
  mode                       ; バッファのモード
  requests                   ; 処理中の xml-http-request
  ontimer                    ; 定期的に実行する関数
  )


(defun find-or-create-view (view-type name &key (mode 'fundamental-mode))
  "指定した名前の view を取得。存在しないなら新規に作成する。
view と新規に作成したかどうかを多値で返す"
  (let ((view (find-view name)))
    (if (and view (typep view view-type))
        (values view nil)
      (values (create-view view-type name mode) t))))

(defun find-view (name)
  "名前から view を探す"
  (let ((buffer (find-buffer name)))
    (get-view-by-buffer buffer)))

(defun create-view (view-type name mode)
  "view を作成"
  (let* ((buffer (create-new-buffer name))
         (view (github::make-struct view-type :buffer buffer :mode mode)))
    (setup-buffer buffer view)
    (start-view-timer-if-need)
    view))

(defun setup-buffer (buffer view)
  "バッファを初期化して buffer local な変数に view を設定"
  (with-selected-buffer (buffer)
    (setup-temp-buffer buffer)
    (setf buffer-read-only t)
    (when (view-mode view)
      (funcall (view-mode view)))
    ;; foo-mode を実行するとだいたい kill-all-local-variables されるから
    ;; 最後に @view を設定
    (setf @view view)
    ))

(defun selected-view ()
  "現在のバッファに関連付けられている view を取得"
  (get-view-by-buffer (selected-buffer)))

(defun view-buffer-p (buffer)
  "view のバッファなら t"
  (local-variable-p '@view buffer))

(defun view-buffer-available-p (view)
  "view のバッファが削除されていないなら t"
  (let ((buffer (view-buffer view)))
    (and buffer
         (bufferp buffer)
         (not (deleted-buffer-p buffer))
         (view-buffer-p buffer))))

(defun deleted-view-p (view)
  "削除された view なら t"
  (not (view-buffer-available-p view)))

(defun get-view-by-buffer (buffer)
  "バッファに関連付けられている view を取得"
  (when (view-buffer-p buffer)
    (buffer-local-value buffer '@view)))

(defun get-view-window (view)
  "view のバッファを表示している Window を返す"
  (when (view-buffer-available-p view)
    (get-buffer-window (view-buffer view))))

(defun count-views ()
  "view の数を返す"
  (count-if #'view-buffer-p (buffer-list)))

(defun list-all-views ()
  "すべての view を返す"
  (mapcar #'get-view-by-buffer (remove-if-not #'view-buffer-p (buffer-list))))

(defun delete-all-views ()
  "すべての view を削除"
  (mapc #'delete-view (list-all-views)))

(defun delete-view (view)
  "view を削除"
  (abort-pending-requests view)
  (let ((buffer (view-buffer view)))
    (when (and view (view-buffer-available-p view))
      (delete-buffer buffer)))
  (stop-view-timer-if-need))

(defun erase-view (view)
  "view の内容を削除"
  (erase-buffer (view-buffer view)))


(defun start-view-timer-if-need ()
  "タイマーが動作していなくて、view が一つ以上あるならタイマーを開始"
  (when (and (not *view-timer-started-p*)
             (< 0 (count-views)))
    (start-timer *view-timer-interval* 'view-dispatch-timer)
    (setf *view-timer-started-p* t)))

(defun stop-view-timer-if-need ()
  "タイマーが動作していて、view が一つもないならタイマーを止める"
  (when (and *view-timer-started-p*
             (zerop (count-views)))
    (stop-timer 'view-dispatch-timer)
    (setf *view-timer-started-p* nil)))

(defun view-dispatch-timer ()
  "各 view の ontimer を呼び出す"
  (dolist (view (list-all-views))
    (let ((fn (view-ontimer view)))
      (when (and fn (functionp fn))
        (handler-case
            (funcall fn)
          (error (c)
            (errbox "~S" c)))))))

(defun do-request (view fn &rest args)
  "処理中のリクエストを追加する"
  (unless (string= (package-name (symbol-package fn)) :github)
    (error "github パッケージの関数じゃない: ~S" fn))
  (unless (member :callback args)
    (error ":callback 引数がない: ~S" args))
  (let ((req (apply fn args)))
    (push req (list-view-requests view))))

(defun remove-completed-requests (view)
  "処理が完了したリクエストを削除する"
  (setf (view-requests view) (get-pending-requests view)))

(defun request-pending-p (view)
  "バックグラウンドで処理中のリクエストがあるなら t"
  (some 'xhr:xhr-request-waiting-p (view-requests view)))

(defun abort-pending-requests (view)
  "バックグラウンドで処理中のリクエストを中断"
  (dolist (req (get-pending-requests view))
    (xhr:xhr-abort req)))

(defun count-pending-requests (view)
  "バックグラウンドで処理中のリクエスト数を返す"
  (count-if 'xhr:xhr-request-waiting-p (view-requests view)))

(defun count-completed-requests (view)
  "バックグラウンドで処理中のリクエスト数を返す"
  (count-if 'xhr:xhr-request-completed-p (view-requests view)))

(defun get-pending-requests (view)
  "バックグラウンドで処理中のリクエストを返す"
  (remove-if-not 'xhr:xhr-request-waiting-p (view-requests view)))

(defun get-completed-requests (view)
  "処理が完了したリクエストを返す"
  (remove-if-not 'xhr:xhr-request-completed-p (view-requests view)))


(defun pop-to-view (view)
  "view のバッファをウィンドウに表示する"
  (when (view-buffer-available-p view)
    (pop-to-buffer (view-buffer view) t)))


(provide "gist-mode/base/view")

;;; End
