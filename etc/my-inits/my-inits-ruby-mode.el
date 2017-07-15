;;; my-inits-ruby-mode.el --- setting for ruby-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Ruby 向け設定
;;
;; 使用前に、以下の gem を事前にインストールしておく
;; - rcodetools
;; - pry
;; - pry-doc
;;
;; @see http://blog.shibayu36.org/entry/2013/03/18/192651

;;; Code:

(when (require 'ruby-mode nil t)
  
  ;; マジックコメントの自動挿入を抑止
  ;; @see http://qiita.com/vzvu3k6k/items/acec84d829a3dbe1427a
  (setq ruby-insert-encoding-magic-comment nil)

  ;; ruby-electric を導入する
  ;; (when (require 'ruby-electric nil t)
  ;;   (add-hook 'ruby-mode-hook
  ;;             '(lambda ()
  ;;                (ruby-electric-mode t)
  ;;                )
  ;;             )
  ;;   )

  ;; ruby-block を導入する
  ;; end に対応する行をハイライトする
  (when (require 'ruby-block nil t)
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t)
    )
  
  ;; rbenv を導入する
  ;; (when (require 'rbenv nil t)
  ;;   )

  ;; rcodetools を導入する
  ;; @see http://qiita.com/ironsand/items/ce7c02eb46fcc25a438b
  (when (require 'rcodetools nil t)
    (setq rct-find-tag-if-available nil)
    
    (eval-after-load "ruby-mode"
      '(progn
         (defun ruby-mode-hook-rcodetools ()
           (define-key ruby-mode-map (kbd "<C-tab>") 'rct-complete-symbol)
           (define-key ruby-mode-map (kbd "<C-return>") 'xmp)
           )
         (add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)))

    (defun make-ruby-scratch-buffer ()
      (with-current-buffer (get-buffer-create "*ruby scratch*")
        (if (fboundp 'enh-ruby-mode)
            (enh-ruby-mode)
          (ruby-mode))
        (current-buffer)))

    (defun ruby-scratch ()
      (interactive)
      (pop-to-buffer (make-ruby-scratch-buffer)))
    )
  
  ;; inf-ruby 使用時に pry を使うようにする
  ;; @see http://d.hatena.ne.jp/rubikitch/20140627/pry
  (when (require 'inf-ruby nil t)
    (setq inf-ruby-default-implementation "pry")
    (setq inf-ruby-eval-binding "Pry.toplevel_binding")
    ;; ri などのエスケープシーケンスを処理し、色付けする
    (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
    )

  ;; robe を導入する
  (when (require 'robe nil t)
    (define-key robe-mode-map (kbd "M-.") nil)
    (add-hook 'ruby-mode-hook
              '(lambda ()
                 (robe-mode)
                 (ac-robe-setup)))
    )

  ;; smart-compile を導入する
  (when (require 'smart-compile nil t)
    (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
    (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))
    (setq compilation-window-height 15)
    )
)

;;; my-inits-ruby-mode.el ends here
