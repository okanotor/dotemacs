;;; my-inits-conf-os-independent.el --- setting for miscs dependent on operating system -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; OS に依存しない各種設定。

;;; Code:

;; バックアップ設定
(setq make-backup-files t)
(setq backup-directory-alist
      `((".*" . ,(locate-user-emacs-file "var/backup/"))))

;; オートセーブ設定
(setq auto-save-default t)
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "var/auto-save/") t)))
(setq auto-save-list-file-prefix (locate-user-emacs-file "var/auto-save-list/.saves-"))

;; 略語展開設定
(setq abbrev-file-name (locate-user-emacs-file "var/abbrev/abbrev_defs"))

;; インデント関連の設定
(setq-default indent-tabs-mode nil)     ; インデントにタブを使用しない
(setq-default tab-width 4)              ; タブの長さ = 4

;; indicate-* については以下を参照
;; @see http://d.hatena.ne.jp/khiker/20100114/emacs_eof
;; @see http://ratememo.blog17.fc2.com/blog-entry-928.html
(setq-default indicate-empty-lines t)   ; 行末の空白を表示する
(setq-default indicate-buffer-boundaries 'right) ; バッファの境界にマークを表示

;; テキストの詰め込みの長さ
(set-fill-column 60)

;; マイナーモードの有効化・無効化
;; transient-mark-mode→有効
(transient-mark-mode t)

;; delete-selection-mode→有効
(delete-selection-mode t)

;; global-font-lock-mode→有効
(global-font-lock-mode t)

;; フォントロックサポートモード → jit-lock-mode を使用
(setq font-lock-support-mode 'jit-lock-mode)

;; line-number-mode, column-number-mode→有効
(line-number-mode t)
(column-number-mode t)

;; show-paren-mode → 有効
(show-paren-mode t)
(setq sho-paren-delay 0)
(setq sho-paren-style 'expression)
(set-face-attribute 'show-paren-match-face nil
                    :background "DarkOliveGreen1")

;; tool-bar-mode→無効
(tool-bar-mode 0)                       

;; global-linum-mode→有効
;; 但し、browse-kill-ring-mode と併用すると挙動がおかしくなるので、そこだけ無効化
;; @see https://github.com/tyraeltong/emacsd/blob/master/my-lisps/browse-kill-ring-settings.el
(global-linum-mode t)                   
(setq linum-format "%04d")
(defadvice linum-on (around linum-on-when-kill-ring-buffer activate)
  (if (eq major-mode 'browse-kill-ring-mode)
      (linum-mode -1)
    ad-do-it))

;; キーバインディング
(global-set-key
 (kbd "C-h")                       ; "C-h" を "1文字消す" に割り当てる
 'backward-delete-char-untabify)   ; ヘルプは F1 で実行可能
                                                            
;; whitespace をロード
(require 'whitespace nil t)

;; スクリプトファイルをセーブする際、自動的に chmod+x する
;; @see http://homepage.mac.com/zenitani/elisp-j.html
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; カーソル形状を変更
(add-to-list 'default-frame-alist '(cursor-type . hbar))

;; 「Shift+カーソル」でウィンドウ移動できるようにする
;; @see http://www.emacswiki.org/emacs/WindMove
;; @see http://www.bookshelf.jp/soft/meadow_30.html
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; imenu のインデックスを自動的に作成する
(setq imenu-auto-rescan t)

;; カスタマイズファイルの定義
(setq custom-file (locate-user-emacs-file "etc/inits/99-customize.el"))


;; url cookie の設定
(custom-set-variables
 '(url-configuration-directory (locate-user-emacs-file "var/url/"))
)

;; ブックマークファイルの定義
(eval-after-load "bookmark"
  (quote
   (progn
     (custom-set-variables
      '(bookmark-default-file (locate-user-emacs-file "var/bookmark/bookmarks"))))))

;;; my-inits-conf-os-independent.el ends here
