;;; my-inits-el-get.el --- setting for el-get -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:

;;; el-get を使用するための設定
;;;
;;; 基本的には、パッケージ管理は ELPA で行いたいが、いくつかのパッケー
;;; ジが ELPA に登録されていないため、そのようなパッケージは el-get を
;;; 使ってインストールする。
;;;
;;; master ブランチをインストールしようとしたが、
;;; `flet' is an obsolete macro (as of 24.3); use either `cl-flet' or `cl-letf'
;;; と言われてしまうため、現状 4.stable にしておく


;;; Code:

(setq el-get-dir (locate-user-emacs-file "lib/elisp/el-get/"))

;; el-get 未インストールの場合、インストールを行う
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

(el-get 'sync)

;;; my-inits-el-get.el ends here
