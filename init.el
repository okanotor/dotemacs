;;; init.el --- Emacs 初期化ファイル -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Emacs 初期化ファイルのエントリポイント。
;; init-loader を使用して残りの設定ファイルを読み込むので、事前に init-loader を取得しておく。
;; 但し、package-initialize は他のファイルで行うため、init-loader は package 経由で
;; インストールしないようにする。
;; ※事前に setup.sh 内で github を clone することで対応。

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; emacs -q -l した時に、 user-emacs-directory が変わるようにする
;; @see http://blog.shibayu36.org/entry/2015/05/01/172215
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; load-path を追加する関数
;; @see http://sakito.jp/emacs/emacs23.html
;; @see http://www.gfd-dennou.org/member/uwabami/cc-env/EmacsBasic.html
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (locate-user-emacs-file path)))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "lib/elisp" "etc/inits" "etc/inits-local")

;; 個人情報を読み込む
(load "personal-information" t t)

;; 空ファイルを作成する
;; update-init-loader-files-symlink で使用
(defun touch-file (file-name)
  (let ((buffer (find-file-noselect (expand-file-name file-name))))
    (set-buffer buffer)
    (basic-save-buffer)
    (kill-buffer buffer)))

;; init-loader 対象を定義し、リンクを張る
(setq init-loader-directory (locate-user-emacs-file "etc/inits"))
(setq init-loader-files-src-dir (locate-user-emacs-file "etc/my-inits"))

;; 初期化ファイルのリンク生成を行う
(defvar init-loader-files-alist)
(defun update-init-loader-files-symlink (&optional force)
  (interactive "p")
  (unless (load "init-loader-files-alist" t t)
    (message "failed loading init-loader-files-alist")
    )
  (let ((file (concat init-loader-directory "/.updated")))
    (when (or (not (file-exists-p (expand-file-name file))) force)
      ;; delete .elc files.
      (dolist (file (directory-files init-loader-directory t ".+\\.elc$"))
        (delete-file file))
      ;; delete .el files if it is symlink
      (dolist (file (directory-files init-loader-directory t ".+\\.el$"))
        (when (file-symlink-p file)
          (delete-file file)))
      ;; create .el symlinks
      (dolist (elem init-loader-files-alist)
        (let ((src-file (concat init-loader-files-src-dir "/my-inits-" (cdr elem) ".el"))
              (dst-file (concat init-loader-directory "/" (if (car elem) (concat (car elem) "-") "") (cdr elem) ".el")))
          (if (file-exists-p src-file)
              (make-symbolic-link src-file dst-file)
            (message (format "%s doesn't exist." src-file))
            )))
      ;; byte compile .el files
      (byte-recompile-directory init-loader-directory 0 t)
      ;; touch file
      (touch-file file)
      )))

;; init-loader を用いて初期化を実施
(if (require 'init-loader nil t)
    (progn
      (update-init-loader-files-symlink)
      (init-loader-load init-loader-directory)))

;;; init.el ends here
