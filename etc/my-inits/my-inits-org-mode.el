;;; my-inits-org-mode.el --- setting for org-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; org-mode の設定

;;; Code:

;; CUAとのキー重複を避ける
;; @see http://orgmode.org/manual/Conflicts.html
(setq org-replace-disputed-keys t)

(when (require 'org-install)
  (setq org-startup-folded nil)         ; ファイルを開いた時は全てが見えているようにする
  ;; (setq org-startup-truncated nil)      ; ファイルを開いた時は折り返しを有効にする
  ;; (setq org-return-follows-link t)      ; RET 押下時、リンク先に飛ばないようにする

  ;; auto-mode 設定
  (let ((doc-base (replace-regexp-in-string (getenv "HOME") "" (file-truename my-memo-dir))))
    (add-to-list 'auto-mode-alist `(,(concat doc-base ".+\\.txt\\'") . org-mode))
    ;; 暫定対処 リネーム後削除
    (add-to-list 'auto-mode-alist `(,(concat doc-base ".+\\.howm\\'") . org-mode)))
  (let ((doc-base (substring my-memo-dir 2)))
    (add-to-list 'auto-mode-alist `(,(concat doc-base ".+\\.txt\\'") . org-mode))
    ;; 暫定対処 リネーム後削除
    (add-to-list 'auto-mode-alist `(,(concat doc-base ".+\\.howm\\'") . org-mode)))
  
  ;; ディレクトリ設定
  (setq org-directory my-memo-dir)

  ;; Make windmove work in org-mode:
  ;; @see http://orgmode.org/manual/Conflicts.html
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; アドバイスの追加
  ;; ox-html.el 内、org-html-format-list-item(リスト要素をHTMLのリスト形式に変換)にて
  ;; 複数行にまたがる説明から改行を除去
  (defun my-remove-newline (s)
    (if (null s) s
      (if (string-match "[\r\n]" s)
          (my-remove-newline (replace-match "" nil nil s))
        s)))
  (defadvice org-html-format-list-item (before before-org-html-format-list-item)
    (ad-set-arg 0 (my-remove-newline (ad-get-arg 0))))
  (ad-activate 'org-html-format-list-item)

  ;; org publishing 用設定
  (setq org-src-preserve-indentation t) ; export 処理時のインデントを保持する
  (setq org-src-fontify-natively t)     ; コードブロックのカラーリング

  ;; リンク生成時、デフォルトでは拡張子が .org の場合のみ変換されるため、
  ;; Filterで .txt の場合も変換するよう対応
  (setq org-export-filter-link-functions ())
  (defun my-org-export-filter-link (text backend info)
    (replace-regexp-in-string "\\.txt\">" ".html\">" text))
  (add-to-list 'org-export-filter-link-functions 'my-org-export-filter-link)

  ;; MobileOrg
  ;;  (setq org-mobile-inbox-for-pull "~/doc/memo/howm/mobileorg/flagged.howm")
  (setq org-mobile-inbox-for-pull (expand-file-name (concat my-memo-dir "/mobileorg.txt")))
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
)

;;; my-inits-org-mode.el ends here
