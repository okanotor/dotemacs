;;; my-inits-markdown-mode.el --- setting for markdown-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Markdown 形式ファイルの編集用モード。
;; markdonw-mode は ELPA からダウンロードする。

;;; Code:
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;; my-inits-markdown-mode.el ends here
