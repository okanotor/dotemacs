;;; my-inits-project-memo.el --- setting for project memo -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; プロジェクト別に作業記録を作成・管理するための設定

;;; Code:

(when (boundp 'my-memo-dir)
  ;; プロジェクト関連ファイルの格納先を指定
  (defvar my-project-dir (concat my-memo-dir "/project"))

  ;; プロジェクトの一覧を取得
  (defvar my-project-alist nil)
  (load "my-project-alist" t t)

  ;; プロジェクトのメモファイルを開く関数を定義
  (when my-project-alist
    (defun insert-project-memo-template (project date-string)
      (interactive (list (completing-read "Project: " my-project-alist)
                         (read-from-minibuffer "Date: " (format-time-string "%Y-%m-%d"))))
      (insert "#+TITLE: " date-string "\n"
              "\n"
              "** keywords :noexport:\n"
              "- " (cdr (assoc project my-project-alist)) "\n"
              "- " date-string "\n"
              "\n"
              "** "))
    
    (defun open-project-memo (project)
      (interactive (list (completing-read "Project: " my-project-alist)))
      (let* ((date-string (format-time-string "%Y-%m-%d"))
             (path (concat my-project-dir "/" project "/diary/" date-string ".txt"))
             (exists (file-exists-p path))
             (dir (file-name-directory path)))
        (make-directory dir t)
        (find-file-other-window path)
        (unless exists (insert-project-memo-template project date-string))))
    )

  ;; プロジェクト関連ファイルを org-publish 対象にする
  (add-to-list 'org-publish-project-alist
               `("project-memo"
                  :base-directory ,my-project-dir
                  :base-extension "txt"
                  :publishing-directory "~/srv/project_memo/"
                  :publishing-function org-html-publish-to-html
                  :recursive t
                  :with-title t
                  )
               )

  ;; プロジェクト関連の txt ファイルを org-mode で開けるようにする
  (add-to-list 'auto-mode-alist `(,(concat my-project-dir ".*\\.txt\\'") . org-mode))
  )

;;; my-inits-project-memo.el ends here

