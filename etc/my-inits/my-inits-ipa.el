;;; my-inits-ipa.el --- setting for ipa -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; ソースコード等に簡易コメントを記述できる ipa の設定
;; ipa は ELPA に入ってないので、el-get 経由でダウンロードする。

;;; Code:

(when (require 'ipa nil t)
  (setq ipa-file (expand-file-name (locate-user-emacs-file "var/ipa/ipa.txt"))))

;;; my-inits-ipa.el ends here
