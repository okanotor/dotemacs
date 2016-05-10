;;; my-inits-recentf.el --- setting for recentf -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; recentf まわりの設定

;;; Code:

;; 最近使ったファイルの設定
;; @see http://maruta.be/intfloat_staff/101
(custom-set-variables
 '(recentf-save-file (locate-user-emacs-file "var/recentf/recentf"))
 '(recentf-max-saved-items 50)
 )

;; recentf-ext を利用する
;; @see http://d.hatena.ne.jp/rubikitch/20091224/recentf
;; @see http://masutaka.net/chalow/2011-10-30-2.html
(require 'recentf-ext nil t)

;;; my-inits-recentf.el ends here
