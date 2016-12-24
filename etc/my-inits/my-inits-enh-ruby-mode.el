;;; my-inits-enh-ruby-mode.el --- setting for enh-ruby-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Ruby 向け設定 (Enhanced Ruby Mode 利用)
;;

;;; Code:

(when (require 'enh-ruby-mode nil t)
  
  ;; マジックコメントの自動挿入を抑止
  (setq enh-ruby-add-encoding-comment-on-save nil)

  ;; auto-mode-alist で ruby-mode で開くよう設定されている定義を使って
  ;; enh-ruby-mode で開くよう追加
  (dolist (elem auto-mode-alist)
    (if (eq (cdr elem) 'ruby-mode)
        (add-to-list 'auto-mode-alist `(,(car elem) . enh-ruby-mode))))

  ;; ruby-electric を導入する
  (when (require 'ruby-electric nil t)
    (add-hook 'enh-ruby-mode-hook
              '(lambda ()
                 (ruby-electric-mode t)
                 )
              )
    )

  ;; enh-ruby-block を導入する
  ;; ruby-block の内容を、enh-ruby-mode を使用するよう修正したもの
  (when (require 'enh-ruby-block nil t)
    (enh-ruby-block-mode t)
    (setq enh-ruby-block-highlight-toggle t)
    )

  ;; rbenv を導入する
  (when (require 'rbenv nil t)
    )

  ;; rcodetools を導入する
  (when (require 'rcodetools nil t)
    (setq rct-find-tag-if-available nil)
    
    (eval-after-load "enh-ruby-mode"
      '(progn
         (defun enh-ruby-mode-hook-rcodetools ()
           (define-key enh-ruby-mode-map (kbd "<C-tab>") 'rct-complete-symbol)
           (define-key enh-ruby-mode-map (kbd "<C-return>") 'xmp)
           )
         (add-hook 'enh-ruby-mode-hook 'enh-ruby-mode-hook-rcodetools)))

    (defun make-enh-ruby-scratch-buffer ()
      (with-current-buffer (get-buffer-create "*enhanced ruby scratch*")
        (if (fboundp 'enh-ruby-mode)
            (enh-ruby-mode))
        (current-buffer)))

    (defun enh-ruby-scratch ()
      (interactive)
      (pop-to-buffer (make-enh-ruby-scratch-buffer)))
    )

  ;; inf-ruby 使用時に pry を使うようにする
  (when (require 'inf-ruby nil t)
    (setq inf-ruby-default-implementation "pry")
    (setq inf-ruby-eval-binding "Pry.toplevel_binding")
    (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
    )
  )

;;; my-inits-enh-ruby-mode.el end here
