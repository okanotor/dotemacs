;;; my-inits-source-reading --- setting for source reading environments -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;;;
;;; ソースコード読解環境に関する設定
;;; - org-mode を利用する
;;;   - INCLUDE が使えるので
;;;   - org-8.3系以降必須
;;; - ディレクトリ構成は以下の通り
;;;   ${SR_BASE}
;;;   + bin : 補助スクリプトの格納場所
;;;   + conf : 読解対象ソースのリスト
;;;   + doc : 読解メモの格納場所
;;;   + src : ソースコードの展開先
;;;   + tmp : ソースコードのダウンロード先
;;; - 事前に以下をインストール
;;;   - ctags : TAGS ファイルを作成するため
;;;   - milkode : milkode を利用するため
;;; - 本コードでは以下のことを行う
;;;   - ベースディレクトリの場所を環境変数に設定する
;;;   - 環境変数の内容を export 時に反映するフックの設定
;;;   - 環境変数の内容をソースコードジャンプの際に反映するアドバイスの設定
;;; - 本コードを読み込んだら、ソースコードのベースディレクトリを {{{sr_base}}} で指定する
;;;   - 読解ディレクトリを移しても耐えられるようにする

;;; Code:

(load "source-reading-settings" t t)

(when (getenv "SR_BASE")
  ;; 置換文字列の指定
  (defvar sr-base-keyword "{{{sr_base}}}")

  ;; ox-publish 向け設定
  (when (require 'ox-publish nil t)
    (let ((sr-base-dir (concat (expand-file-name (getenv "SR_BASE")))))
      (setq org-publish-project-alist
            `(
              ("source-reading"
               :base-directory ,(concat sr-base-dir "/doc")
               :base-extension "txt"
               :publishing-directory "~/srv/source_reading/"
               :publishing-function org-html-publish-to-html
               :recursive t)
              ))
      
      (add-to-list 'auto-mode-alist `(,(concat sr-base-dir "/doc/.*\\.txt\\'") . org-mode))

      (when (and (boundp 'view-mode-by-default-regexp-list) (listp view-mode-by-default-regexp-list))
        (add-to-list 'view-mode-by-default-regexp-list
                     (concat (regexp-quote sr-base-dir) "/src/.*\\'")))

      (let ((src-dir-list (directory-files (concat sr-base-dir "/src") t)))
        (dolist (src-dir src-dir-list)
          (unless (string-match "\\.\\'" src-dir)
            (let ((src-dir-top-file-list (directory-files src-dir t)))
              (dolist (src-dir-top-file src-dir-top-file-list)
                (when (string-match "TAGS\\'" src-dir-top-file)
                  (add-to-list 'tags-table-list src-dir-top-file)))))))
      )
    )
  
  ;; org-export 向け設定
  ;; org-export-before-processing-hook で、INCLUDEの展開前に
  ;; 文字列置換を行う
  (defun org-replace-source-reading-base-dir (val)
    (goto-char (point-min))
    (while (re-search-forward sr-base-keyword nil t) (replace-match (getenv "SR_BASE")))
    )
  (add-hook 'org-export-before-processing-hook 'org-replace-source-reading-base-dir)

  ;; source-jump 向け設定
  ;; ファイルを開く直前に文字列置換を行う
  ;; (advice-remove 'org-open-link-from-string #'filter-args-org-open-link-from-string)
  (defun filter-args-org-open-link-from-string (args)
    ;;(message "args[0] : %s" (car args))
    (let ((temp-string (car args)))
      (when (string-match sr-base-keyword temp-string)
        (setq temp-string (concat "[[" (replace-regexp-in-string (concat ".*" sr-base-keyword) (getenv "SR_BASE") temp-string))))
      (list temp-string (cadr args) (caddr args)))
    )
  (advice-add 'org-open-link-from-string :filter-args #'filter-args-org-open-link-from-string)
  )

  
