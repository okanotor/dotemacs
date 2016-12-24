;;; my-inits-hiwin.el --- setting for hiwin -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:

;;; Code:

(when (require 'hiwin nil t)
  (hiwin-activate)
  (set-face-background 'hiwin-face "gray80")
  )

;;; my-inits-hiwin.el ends here
