;;; my-inits-nethack.el --- setting for nethack -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; ローグライクゲーム nethack のための設定
;;
;; 最近全然やってない

;;; Code:

;;(setq process-coding-system-alist
;;      (cons
;;       '("nethack" euc-jp . euc-jp)
;;       process-coding-system-alist))
(autoload 'nethack "nethack" "Play Nethack." t)
;;(setq nethack-program "/usr/local/games/jnethack_emacs")
(setq nethack-program "/Users/okanotor/tmp/nethack_el/opt/nethack/usr/games/nethack")

(defun nethack-x-timestamp-message ()
  "Add a time-stamp to every message.

Add the following to your ~/.emacs

  (add-hook 'nethack-before-print-message-hook 
	    'nethack-x-timestamp-message)"
  (insert (format "(%d) " (elt nh-status-attribute-T 0))))
(add-hook 'nethack-before-print-message-hook 'nethack-x-timestamp-message) 

(set-face-font 'nethack-map-tile-face "Ricty")

;;; my-inits-nethack.el ends here
