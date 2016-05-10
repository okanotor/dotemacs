;;; my-inits-elscreen.el --- setting for elscreen -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Emacs のフレームをタブ切り替えっぽくする elscreen の設定
;; ELPA のが最新 (Emacs 24 での不具合対処済) なので、ELPA からダウンロードする。
;; ELPA 版は apel 不要。
;; @see http://www.morishima.net/~naoto/software/elscreen/
;; @see http://www.emacswiki.org/emacs/EmacsLispScreen

;; TODO replace load to require or autoload

;;; Code:

;; elscreen の呼び出し
(cond (emacs23-p (require 'elscreen))
      (emacs24-p (elscreen-start)))

;; キーバインディングの設定
(when (featurep 'elscreen)
  (when (fboundp 'elscreen-next)
    (global-set-key (kbd "C-t") 'elscreen-next))
  (when (fboundp 'elscreen-previous)
    (global-set-key (kbd "C-S-t") 'elscreen-previous))
  )

;;; my-inits-elscreen.el ends here
