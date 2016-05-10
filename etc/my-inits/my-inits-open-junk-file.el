;;; my-inits-open-junk-file.el --- setting for open-junk-file -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; 一時ファイルを手軽に作れる open-junk-file の設定
;; ELPA からダウンロードする。
;;

;;; Code:

(when (require 'open-junk-file nil t)
  ;; 作成ファイルは全て howm と同じディレクトリに格納する
  (setq open-junk-file-format (concat my-memo-dir "/junk/%Y/%m/%Y-%m-%d-%H%M%S."))

  ;; howm用のタグを挿入
  ;; - 挿入時にコメントを記入できるよう改良
  ;; - リストにない拡張子の場合にエラーになる不具合を修正
  ;; - リストは自分用に変更
  ;; @see http://d.hatena.ne.jp/hotoku/20130301/1362148309
  (defvar open-junk-ext-tags-alist
    '(("el" ";;" "ELISP")
      ("r" "#" "R")
      ("rb" "#" "RUBY")
      ("sql" "--" "SQL")
      ("scm" ";;" "SCHEME")))
  (defadvice open-junk-file
    (after open-junk-file-insert-howm-comment-advice activate)
    "After open-junk-file, insert a tag into the opened buffer
to be searched by howm."
    (let* ((ext (replace-regexp-in-string "^.*\\.\\([^\\.]+\\)$" "\\1" buffer-file-name))
           (asc (assoc ext open-junk-ext-tags-alist)))
      (when asc
        (let* ((prefix (cadr asc))
               (tag (caddr asc)))
          (insert prefix " %" tag " " (read-string "COMMENT: ") "\n")))))

  ;; elisp ファイルの場合、lisp-interaction-mode で開く
  (defadvice open-junk-file (after open-junk-file-emacs-lisp-advice activate)
    (when (string-match ".*\\.el$" buffer-file-name)
      (lisp-interaction-mode))))

;;; my-inits-open-junk-file.el ends here
