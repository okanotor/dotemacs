;;; my-inits-tempbuf.el --- setting for tempbuf -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; 使わなくなったバッファを自動的に閉じる tempbuf のための設定
;; el-get 経由でインストールする。

;;; Code:

(when (require 'tempbuf nil t)
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
)

;;; my-inits-tempbuf.el ends here
