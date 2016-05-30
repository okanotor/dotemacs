;;; my-inits-helm.el --- setting form helm -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Emacs-helm を使うための設定

;;; Code:
(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

  (when (require 'helm-descbinds nil t)
    (helm-descbinds-mode)
    )
  )

;;; my-inits-helm.el ends here
