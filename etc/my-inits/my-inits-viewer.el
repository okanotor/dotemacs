;;; my-inits-viewer.el --- setting for viewer -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; View Mode 用支援機能の設定
;; ELPA よりインストールする。

;;; Code:

(setq view-read-only t)
(when (require 'view nil t)
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
  (define-key view-mode-map (kbd "?") 'View-search-regexp-backward)
  (define-key view-mode-map (kbd "G") 'View-goto-line-last)
  (define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
  (define-key view-mode-map (kbd "f") 'View-scroll-page-forward)

  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-char)
  (define-key view-mode-map (kbd "J") 'View-scroll-line-forward)
  (define-key view-mode-map (kbd "K") 'View-scroll-line-backward)

  ;; 編集可能行を水色にする
  ;; viewer.el が mode-line の値を保持するようになっているため、
  ;; このタイミングで設定しておく
  (custom-set-faces
   '(mode-line
     ((((class color) (min-colors 88))
       (:background "turquoise1")))))

  (when (require 'viewer nil t)
    (viewer-stay-in-setup)
    (setq viewer-modeline-color-unwritable "deep pink")
    (setq viewer-modeline-color-view "orange")
    (viewer-change-modeline-color-setup)
    (setq view-mode-by-default-regexp "\\.log$")

    ;; 自作関数
    ;; ファイル名が特定の条件にマッチする場合、view-mode で開く
    (setq view-mode-by-default-regexp-list
          `(
            ;; elpa 配下の xx-autoloads.el は、ダウンロード時に elpa が自動生成するファイルのため、
            ;; 読み取り専用にするとインストールに失敗する。
            ;; このため、xx-autoloads.el は除くよう正規表現を設定
            ,(concat (regexp-quote (expand-file-name user-emacs-directory)) "lib/elisp/elpa/.*[^\\(\\-autoloads\\)]\\.el$")
            ".*\\.el\\.gz$"
            ))
    (defun view-mode-by-default (list)
      (cond ((null list) nil)
            ((string-match (car list) buffer-file-name) t)
            (t (view-mode-by-default (cdr list)))))
    (defun view-mode-by-default-setup-with-regexp-list ()
      (when (and buffer-file-name view-mode-by-default-regexp-list
                 (view-mode-by-default view-mode-by-default-regexp-list))
        (view-mode 1)
        (message "view-mode by view-mode-by-default-regexp.")))
    (add-hook 'find-file-hook 'view-mode-by-default-setup-with-regexp-list)
    ))

;;; my-inits-viewer.el ends here
