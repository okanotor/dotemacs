;;; my-inits-browse-kill-ring.el --- setting for browse-kill-ring -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; kill ring の内容を確認できる browse-kill-ring の設定
;; browse-kill-ring は ELPA からインストールする
;; anything にも kill-ring を扱う sources がある(※)ようだが
;; こちらの方が使いやすそう。
;; (※) anything-config.el 内に "anything-c-source-kill-ring" で定義されている
;; @see http://www.emacswiki.org/emacs/BrowseKillRing

;;; Code:

(when (require 'browse-kill-ring nil t)
;  (global-set-key (kbd "M-y") 'browse-kill-ring)
  (browse-kill-ring-default-keybindings))

;; kill-ring に同じ内容の文字列を複数入れない
;; @see http://www.fan.gr.jp/~ring/Meadow/meadow.html
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;;; my-inits-browse-kill-ring.el ends here
