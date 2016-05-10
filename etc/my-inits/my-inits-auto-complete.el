;;; my-inits-auto-complete.el --- setting for auto-complete -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; コード支援機能「Auto Complete」の設定
;; auto-complete は ELPA からインストールする
;; ※auto-complete-config も同時にインストールされる
;; @see http://dev.ariel-networks.com/wp/documents/aritcles/emacs/part9
;; @see http://cx4a.org/software/auto-complete/manual.ja.html

;;; Code:

(when (require 'auto-complete-config nil t)

  ;; おすすめ設定を適用
  (ac-config-default)

  ;; 補完履歴の格納場所を変更
  (custom-set-variables
   '(ac-comphist-file (locate-user-emacs-file "var/auto-complete/ac-comphist.dat"))
   )

  ;; キーマップ変更
  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)

  ;; TAB キーで補完
  (setq ac-dwim t)
)

;;; my-inits-auto-complete.el ends here
