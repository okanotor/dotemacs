;;; my-inits-irfc.el --- setting for irfc -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; RFC を読むための irfc の設定

;;; Code:
(when (require 'irfc nil t)
  (setq irfc-directory (locate-user-emacs-file "var/rfc"))
  (setq irfc-assoc-mode t)
  )

;;; my-inits-irfc.el ends here
