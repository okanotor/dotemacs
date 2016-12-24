;;; my-inits-rfc.el --- setting for RFC -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; RFC ファイルを読むための設定ファイル。
;; rfc.el を使用する。
;; 2014-02-08 現在、ELPA に未登録のため、EmacsWikiよりダウンロードする。
;; (install-elisp-from-emacs-wiki "rfc.el")

;;; Code:

(when (require 'rfc nil t)
  ;;  (setq rfc-url-save-directory "~/Documents/rfc")
  (setq rfc-url-save-directory (locate-user-emacs-file "var/rfc"))
  (setq rfc-index-url "http://www.ietf.org/rfc/rfc-index.txt")
  (setq rfc-archive-alist (list 
                           rfc-url-save-directory
                           "http://www.ietf.org/rfc/"))
  (setq rfc-insert-content-url-hook '(rfc-url-save))
)

;;; my-inits-rfc.el ends here
