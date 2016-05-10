;;; my-inits-conf-windows.el --- setting for windows -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Windows 向け設定

;;; Code:

;; set default frame
(setq default-frame-alist
      (append (list
               '(top . 0)
               '(left . 0)
               '(width . 219)
               '(height . 57)
               '(font . "-*-*-normal-r-normal-normal-12-*-*-*-*-*-fontest-msgothic")
               )
              default-frame-alist))

;;; my-inits-conf-windows.el ends here
