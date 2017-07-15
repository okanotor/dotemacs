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
  ;; - 備忘録を出力できるよう修正
  ;; @see http://d.hatena.ne.jp/hotoku/20130301/1362148309
  (defvar open-junk-ext-tags-alist
    '(("el" ";;" "ELISP")
      ("r" "#" "R" ("- R 起動 => M-x R"
                    "- 編集中のコードを実行 => C-c C-c"))
      ("rb" "#" "RUBY" ("- inf-ruby 起動 => M-x inf-ruby"
                        "  - 最後の式を実行 => C-x C-e (ruby-send-last-sexp)"
                        "  - ブロック実行 => C-c C-b (ruby-send-block)"
                        "  - リージョン実行 => C-c C-r (ruby-send-region)"
                        "- robe 起動 (事前に inf-ruby を起動しておく) => M-x robe-start"
                        "- rcodetools関連"
                        "  - コメント挿入 => M-; を2回"
                        "  - xmpfilter実行 => C-Return"))
      ("sql" "--" "SQL" ("- SQLクライアント起動 => M-x my-sql-connect"
                         "  - 接続情報は sql-connection-alist に書いておく"
                         "- 編集中のSQLを実行 => C-c C-c"
                         "- 選択中のリージョンのSQLを実行 => C-c C-r"))
      ("scm" ";;" "SCHEME")))
  (defadvice open-junk-file
    (after open-junk-file-insert-howm-comment-advice activate)
    "After open-junk-file, insert a tag into the opened buffer
to be searched by howm."
    (let* ((ext (replace-regexp-in-string "^.*\\.\\([^\\.]+\\)$" "\\1" buffer-file-name))
           (asc (assoc ext open-junk-ext-tags-alist)))
      (when asc
        (let* ((prefix (cadr asc))
               (tag (caddr asc))
               (memo (cadddr asc)))
          (insert prefix " %" tag " " (read-string "COMMENT: ") "\n")
          (when memo
            (insert prefix "\n")
            (dolist (text memo)
              (insert prefix " " text "\n")))
          ))))

  ;; elisp ファイルの場合、lisp-interaction-mode で開く
  (defadvice open-junk-file (after open-junk-file-emacs-lisp-advice activate)
    (when (string-match ".*\\.el$" buffer-file-name)
      (lisp-interaction-mode))))

;;; my-inits-open-junk-file.el ends here
