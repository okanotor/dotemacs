;;; my-inits-ox-publish.el --- setting for ox-publish -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; 

;;; Code:
(when (require 'ox-publish nil t)
  (let ((sr-base-dir (concat (expand-file-name "~/var/proj/source_reading"))))
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

;;; my-inits-ox-publish.el ends here

