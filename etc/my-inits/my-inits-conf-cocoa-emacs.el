;;; my-inits-conf-cocoa-emacs.el --- setting for coca-emacs -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Cocoa Emacs 向けの設定

;;; Code:

(when cocoa-p
  
  ;; 初期フレームサイズの設定
  (setq default-frame-alist
	(append (list
		 '(left . 0)
		 '(top . 0)
		 '(width . 200)
		 '(height . 65))
		default-frame-alist))

  ;; AquaSKK を利用する際、C-j を AquaSKK に奪われないようにする
  ;; @see http://kosugi-tomo.vox.com/library/post/carbon-emacs-aquaskk-c-j-etc.html
  ;; @see http://recompile.net/archives/51148778.html
  (setq mac-pass-control-to-system nil)

  ;; 言語設定
  ;; @see http://sakito.jp/emacs/emacs23.html
  (set-language-environment 'Japanese)
  (prefer-coding-system 'utf-8)

  ;; メタキーの変更
  (setq mac-command-modifier (quote meta))
  (setq mac-option-modifier (quote super))

  ;; フォント設定
  (set-face-attribute 'default nil
		      :family "Ricty"
		      :height 140)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))

  ;; for Marverics
  ;; デフォルトディレクトリを "/" から "~/" に変更する
  (setq inhibit-splash-screen t)
  (defun cd-to-homedir-all-buffers()
    "Change every current directory of all buffers to the home directory."
    (mapc
     (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
  (add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

  ;; exec-path
 
  ;; Cocoa Emacs は launchd から直接呼び出されて起動するため、環境変数
  ;; はlaunchd のものを引き継ぐ。このため、 launchd で設定していない(=
  ;; シェルの設定ファイルにしかない) PATH はEmacs の exec-path には設
  ;; 定されていない。
  ;;
  ;; これを解消するための elisp (exec-path-from-shell) も存在するが、
  ;; 現在の環境では /usr/local/bin が /usr/bin や /bin の後に来ている
  ;; (/etc/paths で定義)。 Emacs ではできる限り Homebrew でインストー
  ;; ルしたものを使用したいが、環境はいじりたくないため、上記の elisp
  ;; は使いたくない。
  ;;
  ;; これらの理由により、ここで exec-path の定義を行う。
  ;;
  ;; なお、launchd の設定はデフォルトのままなので、 launchd 上での
  ;; PATH は、以下の 4 つがこの順で定義されている。
  ;;   /usr/bin /bin /usr/sbin /sbin
  ;; この値は launchd 上でハードコーディングされている模様。
  ;; * strings /sbin/launchd で確認
  ;;(setq exec-path '("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/usr/sbin" "/sbin"))
  )

;; my-inits-conf-cocoa-emacs.el ends here
