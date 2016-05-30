;;; my-inits-sql-mode.el --- setting for sql-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; SQL mode の設定

;;; Code:

(when (require 'sql nil t)

  ;; 接続先リストの読み込み
  (load "sql-connection-alist")

  ;; master-mode の設定
  (require 'master nil t)

  ;; エンコーディングの設定
  ;; @see http://www.sato-noriaki.net/diary/20071113.html
  (set-terminal-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (add-hook 'sql-interactive-mode-hook
            (function (lambda ()
                        (set-buffer-process-coding-system 'utf-8 'utf-8))))

  ;; プロンプトの設定 (for postgresql)
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq sql-prompt-regexp "^[_[:alpha:][:digit:]]*[=][#>] ")
              (setq sql-prompt-cont-regexp "^[_[:alpha:][:digit:]]*[-][#>] ")))

  ;; sql-indent の設定
  (when (require 'sql-indent nil t)

    (setq sql-indent-first-column-regexp
          (concat "\\(^\\s-*" (regexp-opt '(
                                            "select" "update" "insert" "delete"
                                            "union" "intersect"
                                            "from" "where" "into" "group" "having" "order"
                                            "set"
                                            "create" "drop" "truncate" "alter" "grant" "revoke"
                                            "with" "limit" "offset"
                                            "--") t) "\\(\\b\\|\\s-\\)\\)\\|\\(^```$\\)"))

    (add-hook 'sql-mode-hook (function (lambda () (setq sql-indent-offset 2))))
    )

  ;; sqlup-mode の設定
  (when (require 'sqlup-mode nil t)
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
    )

  ;; Emacs 用にコンパイルした SQLite を使用するよう設定
  ;; 以下の通り configure オプションを指定してコンパイルする。
  ;; --disable-dependency-tracking : Homebrew と同じ
  ;; --enable-dynamic-extensions   : Homebrew と同じ。extension-functions を読みこむため必須。
  ;; --disable-readline            : 日本語を渡すためには必須。
  ;; --disable-shared              : ライブラリの依存関係とかで悩みたくないため

  ;; なお、Emacs 用バイナリを使用する理由は以下の通り。
  ;; - Mac OS X 標準の SQLite はバージョンが古いため、あまり使いたくない
  ;; - Homebrew でインストールした SQLite は readline を使っているせいか、
  ;;   日本語(多バイト文字?)が Emacs 経由で渡せないため、使いづらい。
  (when cocoa-p
    (let ((sqlite3-for-emacs (locate-user-emacs-file "opt/sqlite3/bin/sqlite3")))
      (when (and (file-exists-p sqlite3-for-emacs) (file-executable-p sqlite3-for-emacs))
        (setq sql-sqlite-program sqlite3-for-emacs)
        (setq sql-sqlite-options
              `("-init" ,(locate-user-emacs-file "/etc/sqliterc"))
              ))))

  ;; sqlite実行プログラムの設定 (for windows)
  (when windows-p
    (setq sql-sqlite-program "sqlite3.exe")
    )

  ;; sql-beautify
  ;; @ref https://www.emacswiki.org/emacs/SqlBeautify
  (defun sql-beautify ()
    "Beautify SQL in region or current sql sentence."
    (interactive)
    (unless mark-active
      (let ((sql-bounds (bounds-of-sql-at-point)))
        (set-mark (car sql-bounds))
        (goto-char (cdr sql-bounds))))
    (sql-beautify-region (region-beginning) (region-end)))
  
  (defun sql-beautify-region (beg end)
    "Beaufity SQL in region between beg and END."
    (setenv "CLASSPATH" (concat (getenv "CLASSPATH") ";" (replace-regexp-in-string "/" "\\\\" (expand-file-name (locate-user-emacs-file "lib/java/SqlBeautify-1.0.jar")))))
    (cd (locate-user-emacs-file "lib/java"))
    (let ((beautified-sql))
      (shell-command-on-region beg end "java tools.SqlBeautify" "*sqlbeautify*" nil)
      (with-current-buffer "*sqlbeautify*"
        (goto-char (point-min))
        (while (search-forward "\^M" nil t) ;delete ^m
          (replace-match "" nil nil))
        (setq beautified-sql (buffer-string)))
      (goto-char beg)
      (kill-region beg end)
      (insert beautified-sql)
      (kill-buffer "*sqlbeautify*")))

  (defun bounds-of-sql-at-point ()
    "get start and end point of current sql."
    (let ((pt (point)) begin end empty-line-p next-line-included tail-p)
      (when (and (looking-at "[ \t]*\\(\n\\|\\'\\)")
                 (looking-back "[ \t]*;[ \t]*" (beginning-of-line)))
        (search-backward-regexp "[ \t]*;[ \t]*" (beginning-of-line) t))
      (save-excursion
        (skip-chars-forward " \t\n\r")
        (re-search-backward ";[ \t\n\r]*\\|\\`\\|\n[\r\t ]*\n[^ \t]" nil t)
        (skip-syntax-forward "-")
        (setq begin (match-end 0)))
      (save-excursion
        (skip-chars-forward "\t\n\r")
        (re-search-forward "\n[\r\t ]*\n[^ \t]\\|\\'\\|[\t\n\r]*;" nil t)
        (unless (zerop (length (match-string 0)))
          (backward-char 1))
        (skip-syntax-backward "-")
        (setq end (match-beginning 0)))
      (goto-char pt)
      (cons begin end)))

  
  ;; my-sql-connect
  (defun my-sql-connect (connection &optional new-name)
    (interactive
     (if sql-connection-alist
         (list (sql-read-connection "Connection: " nil '(nil))
               current-prefix-arg)
       (user-error "No SQL Connections defined")))
    (if sql-connection-alist
        (when connection
          (let ((connect-set (assoc-string connection sql-connection-alist t)))
            (if connect-set
                (let ((connect-params (cdr connect-set))
                      (product nil))
                  (dolist (param connect-params product)
                    (when (eq (car param) 'sql-product)
                      (setq product (car (cdr param)))))
                  (if product
                      (progn
                        (sql-set-product (car (cdr product)))
                        (sql-connect connection new-name)
                        )
                    (user-error "SQL connection <%s> does not have 'sql-product" connection)
                    nil))
              (user-error "SQL connection <%s> does not exist" connection)
              nil)))
      (user-error "No SQL Connections defined")
      nil))
  )

;;; my-inits-sql-mode.el ends here
