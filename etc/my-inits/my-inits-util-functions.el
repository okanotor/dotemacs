;;; my-inits-util-functions.el --- utility functions -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; ユーティリティ関数群
;; 特定の機能に依らない、汎用的なものを記載
;; ちらばってるので、このファイルに集めたい

;;; Code:

;; 述語
;; Emacs バージョン判定
;; @see http://d.hatena.ne.jp/tomoya/20090807/1249601308
(dolist (ver '("22" "23" "23.0" "23.1" "23.2" "23.3" "23.4" "24" "24.0" "24.1" "24.2" "24.3" "24.4" "24.5" "25" "25.1" "25.2"))
  (set (intern (concat "emacs" ver "-p"))
       (if (string-match (concat "^" ver) emacs-version)
           t
         nil)))

;; 真偽値変換
;; @see http://github.com/elim/dotemacs/blob/master/init.el
(defun x->bool (elt) (not (not elt)))

;; システム判定
;; @see http://d.hatena.ne.jp/tomoya/20090807/1249601308
(setq
 ;; from system-type
 hurd-p    (eq system-type 'gnu)
 linux-p   (eq system-type 'gnu/linux)
 freebsd-p (eq system-type 'gnu/kfreebsd)
 darwin-p  (eq system-type 'darwin)
 dos-p     (eq system-type 'ms-dos)
 nt-p      (eq system-type 'windows-nt)
 cygwin-p  (eq system-type 'cygwin)
 
 ;; from window-system
 ws-x-p    (eq window-system 'x)
 ws-w32-p  (eq window-system 'w32)
 ws-mac-p  (eq window-system 'mac)
 ws-ns-p   (eq window-system 'ns)
 ws-pc-p   (eq window-system 'pc)

 ;; special
 colinux-p (when linux-p
             (let ((file "/proc/modules"))
               (and
                (file-readable-p file)
                ((lambda (elt) (not (not elt)))
                 (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (re-search-forward "^cofuse\.+" nil t)))
                )))
 meadow-p (featurep 'meadow)
 windows-p (or cygwin-p nt-p meadow-p)
 carbon-p (and darwin-p ws-mac-p emacs22-p)
 cocoa-p (and darwin-p ws-mac-p (or emacs23-p emacs24-p emacs25-p))
)


;; ウィンドウ関連

;; other-window-backward
;; ウィンドウの逆方向への巡回
(defun other-window-backward (&optional n)
  "Select the previous window"
  (interactive "p")
  (if n
      (other-window (- n))
    (other-window -1)))

(global-set-key (kbd "C-x O") 'other-window-backward)

;; swap-screen
;; スクリーンの入れ替え
;; @see http://www.bookshelf.jp/soft/meadow_30.html
(defun swap-screen ()
  "Swap two screen, leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))

;; swap-screen-with-cursor
;; スクリーンの入れ替え カーソルは移動させない
;; @see http://www.bookshelf.jp/soft/meadow_30.html
(defun swap-screen-with-cursor ()
  "Swap two screen, with cursor in same buffer"
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))

;; window-toggle-division
;; 横分割と縦分割の入れ替え
;; (他の関数の影響か、記載の内容ではうまく動かなかったため、新たに自作)
;; @see http://www.bookshelf.jp/soft/meadow_30.html
(defun window-toggle-division ()
  "toggle window division between horizontal and vertical"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "not splitted two windows in this frame."))

  ;; get old division information
  (let* ((old-window-list (window-list nil nil (frame-first-window)))
         (buf1 (window-buffer (nth 0 old-window-list)))
         (buf2 (window-buffer (nth 1 old-window-list)))
         (first-window-p (eq (selected-window) (frame-first-window)))
         before-height)
    (setq before-height (window-height))
    (delete-other-windows)

    ;; toggle split
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))

    ;; set new window to buffers
    (let ((new-window-list (window-list nil nil (frame-first-window))))
      (set-window-buffer (nth 0 new-window-list) buf1)
      (set-window-buffer (nth 1 new-window-list) buf2))

    ;; select window
    (unless first-window-p (other-window 1))
    ))

(global-set-key (kbd "<f2>") 'window-toggle-division)


;; rotate-screen
;; 複数分割したスクリーンの内容をローテーションする
(defun rotate-screen ()
  "rotate multi screen"
  (interactive)
  (when (= (count-windows 1) 1)
    (error "single window in this frame."))
  (let* ((win-list (window-list nil nil (frame-first-window)))
         (buf-list (mapcar 'window-buffer win-list))
         (max-size (length win-list))
         )
    (dotimes (n max-size)
      (set-window-buffer (nth n win-list) (nth (% (+ n 1) max-size) buf-list)))))

(global-set-key (kbd "<f3>") 'rotate-screen)

;; toggle-truncate-lines
;; @see http://ubulog.blogspot.com/2007/09/emacsonoff.html
(defun toggle-truncate-lines ()
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))


;; sort buffer list
;; @see http://www.bookshelf.jp/soft/meadow_28.html
(defun my-sort-buffer-by (cbuf slst)
  ;; sort using bury-buffer
  (cond ((eq slst '()) '())
        ((eq cbuf (car slst)) (my-sort-buffer-by cbuf (reverse (cdr slst))))
        (t (bury-buffer (car slst))
           (my-sort-buffer-by cbuf (cdr slst)))))

(defun my-sort-buffer-by-name (x y)
  (if (or (buffer-file-name y) (buffer-file-name x))
      (string< (buffer-file-name y) (buffer-file-name x))
    (string< (buffer-name y) (buffer-name x))))

(defun my-sort-buffer ()
  "Sort buffer list by name"
  (interactive)
  (let* ((cbuf (current-buffer))
         (blst (copy-sequence (buffer-list)))
         (slst (sort blst 'my-sort-buffer-by-name)))
    (my-sort-buffer-by cbuf slst)))

;;; my-inits-util-functions.el ends here
