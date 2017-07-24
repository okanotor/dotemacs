;;; my-inits-package.el --- setting for package -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Emacs 24 から導入されたパッケージシステムを使うための設定

;;; Code:

(when (require 'package nil t)
  ;; インストール先設定
  (setq package-user-dir (locate-user-emacs-file "lib/elisp/elpa"))

  ;; 参照先設定
  ;; ネットワーク接続の有無で切り替え
  (defvar can-connect-network t)
  (if can-connect-network
      (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
    (setq package-archives `(("local" . ,(expand-file-name (locate-user-emacs-file "var/cache/elisp")))))
    )
  (package-initialize)

  ;; インストールしておく package の一覧
  ;; インストールしていない場合は自動でインストールしておく
  ;; @see https://github.com/ludwigpacifici/dotemacs/blob/master/init.el
  (defvar my-packages nil)
  (setq my-packages
    '(
      ac-inf-ruby
      annotate
      anything
      ascii-art-to-unicode
      async
      auto-complete
      auto-install
      browse-kill-ring
      color-moccur
      ctable
      dash
;      descbinds-anything
      elscreen
      enh-ruby-mode
      ess
      ess-R-object-popup
      git-commit
      gnugo
      graphviz-dot-mode
      helm
      helm-core
      helm-descbinds
      hiwin
      htmlize
      inf-ruby
      irfc
      japanese-holidays
      julia-mode
      magit
      magit-popup
      markdown-mode
      markdown-mode+
      mic-paren
      milkode
      muse
      noflet
      open-junk-file
      org
      popup
      quack
      robe
      recentf-ext
      ruby-additional
      ruby-block
      ruby-electric
      rvm
      scheme-complete
      seq
      sicp
      smart-compile
      sql-indent
      sqlite
      sqlup-mode
      viewer
      w3m
      with-editor
      xpm
      yaml-mode
      yasnippet
      ))

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))

  ;; my-packages の設定内容を確認するための関数
  ;; パッケージの絶対パスを取得する
  (defun my-package-absolute-path (string)
    (expand-file-name (concat package-user-dir "/" string))
    )

  ;; インストール済のパッケージのパスを検索するための正規表現を取得する
  (defun my-package-path-regexp (package)
    (my-package-absolute-path (concat (regexp-quote (symbol-name package)) "-.*")))

  ;; インストール済のパッケージが my-packages に挙げられているかどうかを確認する
  (defun listed-on-my-packages-p (directory)
    (let ((result nil))
      (dolist (p my-packages result)
        (let ((regexp (my-package-path-regexp p)))
          (setq result (or result (string-match regexp directory)))))))

  ;; my-packages に挙げられていないディレクトリを表示する
  (defun display-packages-unlisted-on-my-packages ()
    (interactive)
    (let ((result nil))
      (dolist (file (directory-files (expand-file-name package-user-dir) t) result)
        (cond ((not (file-directory-p file)) (ignore))
              ((string-match (my-package-absolute-path "\\.\\.") file) (ignore))
              ((string-match (my-package-absolute-path "\\.") file) (ignore))
              ((string-match (my-package-absolute-path "archives") file) (ignore))
              ((listed-on-my-packages-p file) (ignore))
              (t (setq result (concat result (when result "\n") file)))))
      (message "%s" (or result "all packages are listed on my-packages."))))

  ;; アーカイブリストからパッケージを検索する
  ;;(defun package-find (name)
  ;;  (dolist (archive (mapcar 'car package-archives))
  ;;    (dolist (package (package--read-archive-file (concat "archives/" archive "/archive-contents")))
  ;;      (when (eq name (car package))
  ;;        (message "%s is archived at %s (%s)" name archive (package-version-join (package-desc-vers (cdr package))))))))

  ;; インストール済パッケージの中から最新のものの絶対パスを取得する
  ;; ELPA のパスはインストールしたバージョンで異なるため、パッケージ内のファイルを利用する際に使用する
  (defun latest-module-path (package)
    (nth 0 (reverse (directory-files (locate-user-emacs-file "lib/elisp/elpa") t (concat (symbol-name package) ".*"))))
    )
  )

;;; my-inits-package.el ends here
