;;; my-inits-markdown-mode.el --- setting for markdown-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Markdown 形式ファイルの編集用モード。
;; markdonw-mode は ELPA からダウンロードする。

;;; Code:
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; @ref http://qiita.com/nanasess/items/c9342c06a3e28e64aeb8
(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))
(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook
          #'(lambda()
          (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))

;;; my-inits-markdown-mode.el ends here
