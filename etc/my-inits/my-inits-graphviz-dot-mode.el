;;; my-inits-graphviz-dot-mode.el --- setting for dot-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Graphviz のソースコードを書くモードの設定
;; graphviz-dot-mode は ELPA からダウンロードする

;;; Code:
(when (require 'org-src nil t)
  (load "graphviz-dot-mode.el"))

;;; my-inits-graphviz-dot-mode.el ends here
