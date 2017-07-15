;;; my-inits-elixir-mode.el --- setting for elixir-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Elixir 向け設定
;;
;; @see https://github.com/elixir-lang/emacs-elixir
;; @see http://qiita.com/niwaken/items/620056c05a767172ef2f
;; @see http://cortyuming.hateblo.jp/entry/2016/05/25/205714

;;; Code:

(when (require 'elixir-mode nil t)
  (add-to-list 'elixir-mode-hook
               (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                 (require 'alchemist nil t)
                 (require 'flycheck-elixir nil t)
                 (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re) "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                 (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                 (ruby-end-mode +1)
                 (ac-alchemist-setup))))
