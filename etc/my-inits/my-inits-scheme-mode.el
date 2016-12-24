;;; my-inits-scheme-mode.el --- setting for scheme-mode -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; scheme 用の設定
;; scheme-mode, cmuscheme は標準で入っている。
;; quack, scheme-complete は ELPA 経由でインストール可能

;;; Code:

;; autoload の設定
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run on inferior Scheme process." t)

;; 実装定義

;; パスを生成する関数
(setq scheme-program-path '("/usr/local/bin" "/usr/bin"))
(defun find-scheme-program (program-name &optional options)
  (let ((program-list (mapcar (lambda (x) (concat x "/" program-name)) scheme-program-path))
        (result nil))
    (unless (or result (null program-list))
      (let ((program (car program-list)))
        (if (and (file-exists-p program) (file-executable-p program))
            (setq result (concat program (when options (concat " " options))))
          (setq program-list (cdr program-list)))))
    result))

;; gauche
;; @see http://practical-scheme.net/gauche/index.html
(setq gauche-program-name (find-scheme-program "gosh" "-i"))
(when gauche-program-name
  (defun run-gosh ()
    (interactive)
    (run-scheme gauche-program-name))
  (add-to-list 'process-coding-system-alist
               '("gosh" . (utf-8 . utf-8))))

;; mit-scheme
;; @see http://www.gnu.org/software/mit-scheme/
(setq mit-scheme-program-name (find-scheme-program "mit-scheme"))
(when mit-scheme-program-name
  (defun run-mit ()
    (interactive)
    (run-scheme mit-scheme-program-name)))

;; racket (旧 PLT scheme)
;; @see http://www.racket-lang.org/
(setq racket-program-name (find-scheme-program "racket"))
(when racket-program-name
  (defun run-racket ()
    (interactive)
    (run-scheme racket-program-name)))

(setq default-scheme-program-name
      (or gauche-program-name mit-scheme-program-name racket-program-name))

;; scheme インタプリタを別ウィンドウで起動する
;; @see http://karetta.jp/book-node/gauche-hacks/004682
;; @see http://blog.livedoor.jp/incomplete_7/archives/51521573.html
;; @see http://saito.hatenablog.jp/entry/20081021/1224614106
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (let ((sw (selected-window)))
    (save-excursion
      (switch-to-buffer-other-window
       (get-buffer-create "*scheme*"))
      (run-scheme default-scheme-program-name)
      (select-window sw))))
(define-key global-map (kbd "C-c S") 'scheme-other-window)

;; フック設定
;; @see http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/scheme.html
(add-hook 'cmuscheme-load-hook
          (lambda () (define-key inferior-scheme-mode-map (kbd "C-c C-t") 'favorite-cmd)))

;; SICP-info を info のパスに追加
(when (require 'sicp nil t)
  (let ((latest-sicp-module (nth 0 (reverse (directory-files (locate-user-emacs-file "lib/elisp/elpa") t "sicp.*")))))
    (add-to-list 'Info-directory-list latest-sicp-module)
    )
  )

;; quack の設定
;; @see http://www.neilvandyke.org/quack/
;; @see http://www.emacswiki.org/emacs/QuackMode
(when (require 'quack nil t)
  ;; 基本設定
  (setq quack-browse-url-browser-function (quote w3m-browse-url))
  (setq quack-pretty-lambda-p t)
  (setq quack-smart-open-paren-p t)

  ;; quack でのフォント設定内容を反映させるため、アドバイズを追加
  (defadvice quack-install-fontification (after after-quack-install-fontification activate)
    (when (fboundp 'font-lock-refresh-defaults)
      (font-lock-refresh-defaults)))
)

;; scheme-complete の設定
;; @see http://d.hatena.ne.jp/kobapan/20091205/1259972925
;; @see http://ayato.hateblo.jp/entry/20120722/1342947934
;; @see http://yuseinishiyama.com/posts/2013/12/01/autocomplite-scheme-with-emacs-and-gauche/
;; @see http://d.hatena.ne.jp/kobapan/20091205/1259972925

;; auto-complete にまかせるようにしたため、一旦削除
;; TODO auto-complete との連携がうまくいく方式を再検討
;; - 課題
;;   - モジュールの呼び出し関係にかかわらず設定・起動できるようにする

;;; my-inits-scheme-mode.el ends here
