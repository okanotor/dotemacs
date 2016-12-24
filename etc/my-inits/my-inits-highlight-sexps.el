;;; my-inits-highlight-sexps.el --- setting for highlight-sexp -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; かっこの対応関係を強調表示する highlight-sexps の設定
;; ELPA 未登録なので、独自に拾ってくる。
;; @see http://david.rysdam.org/src/emacs/emacs.xhtml#highlight-sexps
;; オリジナル版は開始かっこ部の取り扱いが気に入らなかったので、パッチをあてて使用。

;;; Code:

(when (require 'highlight-sexps nil t)
  ;; 色の設定
  (custom-set-variables
   '(hl-sexp-background-colors
     '("#E1FFCD" "#D1F3B1" "#C6E797" "#BDDC7F"
       "#B7D069" "#B3C454" "#B1B942" "#ADAA30"
       "#A29321" "#967C13" "#8B6507")
     ))
  
  ;; 当分はlisp, emacs-lisp の場合のみ設定
  (add-hook 'lisp-mode-hook 'highlight-sexps-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-sexps-mode))

;;; my-inits-highlight-sexps.el ends here
