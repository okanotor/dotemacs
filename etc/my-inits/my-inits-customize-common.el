;;; my-inits-customize-common.el --- setting for customize for all environments -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; 全環境で使用する customize 修正
;; 各環境毎のは別ファイルで管理

;;; Code:

(custom-set-variables
 '(large-file-warning-threshold 20000000)
 '(quack-programs (quote ("/usr/local/bin/scheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((clmemo-mode . t))))
 '(yas-trigger-key "TAB"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; my-inits-customize-common.el ends here
