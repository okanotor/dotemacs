;;; my-inits-eww.el --- setting for eww -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; EWW の設定
;; @see http://futurismo.biz/archives/2950

;;; Code:
(when (require 'eww nil t)
  ;;================================================================================
  ;; キーマップの設定
  ;;================================================================================
  (define-key eww-mode-map "r" 'eww-reload)
  (define-key eww-mode-map "c 0" 'eww-copy-page-url)
  (define-key eww-mode-map "p" 'scroll-down)
  (define-key eww-mode-map "n" 'scroll-up)

  ;;================================================================================
  ;; バッファ名のリネーム
  ;;================================================================================
  (defun eww-mode-hook--rename-buffer ()
    "Rename eww browser's buffer so sites open in new page."
    (rename-buffer "eww" t))
  (add-hook 'eww-mode-hook 'eww-mode-hook--rename-buffer)

  ;;================================================================================
  ;; 検索関連の設定
  ;;================================================================================
  (setq eww-search-prefix "http://www.google.co.jp/search?q=")

  ;; 25 or later
  (when (boundp 'eww-after-render-hook)
    (defvar eww-hl-search-word nil)
    (defun eww-search (term)
      (interactive "sSearch terms: ")
      (setq eww-hl-search-word term)
      (eww-browse-url (concat eww-search-prefix term)))
    (add-hook 'eww-after-render-hook
              (lambda ()
                (highlight-regexp eww-hl-search-word)
                (setq eww-hl-search-word nil)))
    )

  (defun browse-url-with-eww ()
    (interactive)
    (let ((url-region (bounds-of-thing-at-point 'url)))
      ;; url
      (if url-region
          (eww-browse-url (buffer-substring-no-properties (car url-region)
                                                          (cdr url-region))))
      ;; org-link
      (setq browse-url-browser-function 'eww-browse-url)
      (org-open-at-point)))
  (global-set-key (kbd "C-c p") 'browse-url-with-eww)

  (defun eww-disable-images ()
    "Do not display images on eww."
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image-alt)
    (eww-reload))
  (defun eww-enable-images ()
    "Display images on eww."
    (interactive)
    (setq-local shr-put-image-function 'shr-put-image)
    (eww-reload))
  (defun shr-put-image-alt (spec alt &optional flags)
    (insert alt))
  (defun eww-mode-hook--disable-image ()
    (setq-local shr-put-image-function 'shr-put-image-alt))
  (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

  ;; backport from 25.x
  (defcustom eww-bookmarks-directory user-emacs-directory
    "Directory where bookmark files will be stored."
    :version "24.5"
    :group 'eww
    :type 'string)

  (setq eww-bookmarks-directory (locate-user-emacs-file "var/eww/"))

  (defun eww-write-bookmarks ()
    (with-temp-file (expand-file-name "eww-bookmarks" eww-bookmarks-directory)
      (insert ";; Auto-generated file; don't edit\n")
      (pp eww-bookmarks (current-buffer))))

  (defun eww-read-bookmarks ()
    (let ((file (expand-file-name "eww-bookmarks" eww-bookmarks-directory)))
      (setq eww-bookmarks
            (unless (zerop (or (nth 7 (file-attributes file)) 0))
              (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer)))))))

  (when (require 'helm-eww-bookmark nil t)
    (define-key eww-mode-map "B" 'helm-eww-bookmark))

  (load "url-proxy-services")
  )


;;; my-inits-eww.el ends here
