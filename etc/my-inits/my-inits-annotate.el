;;; my-inits-annotate.el --- setting for annotate -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; ファイルを修正することなく行に注釈をつけることができる annotate の設定
;; @see https://github.com/bastibe/annotate.el
;; @see http://rubikitch.com/2015/08/16/annotate/

;;; Code:
(when (require 'annotate nil t)
  (setq annotate-file (locate-user-emacs-file "var/annotate/annotate.txt"))
  (define-key annotate-mode-map (kbd "C-c C-a") nil)
  (define-key annotate-mode-map (kbd "C-c a") 'annotate-annotate)
  (add-hook 'find-file-hook 'annotate-mode)
  )

;;; my-inits-annotate.el ends here
