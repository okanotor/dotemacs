;;; my-inits-yasnippet.el --- setting for yasnippet -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; 入力補助機能 Yasnippet の設定
;; ELPA からインストールする
;; 
;; @see http://fukuyama.co/yasnippet

;;; Code:

(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs
        (list
         (concat (latest-module-path 'yasnippet) "/snippets")
         (locate-user-emacs-file "usr/share/snippets")
         ))
  (yas-global-mode 1)
  (custom-set-variables '(yas-trigger-key "TAB"))
)

;;; my-inits-yasnippet.el ends here
