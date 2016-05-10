;;; my-inits-ibuffer.el --- setting for ibuffer -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; ibuffer の設定
;; ibuffer は標準でインストール済。

;;; Code:
(when (require 'ibuffer nil t)
  ;;(global-set-key "\C-x\C-b" 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  )

;;; my-inits-ibuffer.el ends here
