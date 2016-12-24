;;; my-inits-sicp.el --- setting for sicp -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; SICP を info で読めるようにするための設定

;;; Code:
(when (require 'sicp nil t)
  (let ((latest-sicp-module (nth 0 (reverse (directory-files (locate-user-emacs-file "lib/elisp/elpa") t "sicp.*")))))
    (add-to-list 'Info-directory-list latest-sicp-module)
    )
  )

;;; my-inits-sicp.el ends here

