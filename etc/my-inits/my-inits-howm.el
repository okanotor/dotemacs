;;; my-inits-howm.el --- setting for howm -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; howm (Hitori Otegaru Wiki Modoki) の設定
;; ELPA に登録されているが、文字コードの変換などを行いたいため、
;; ELPA を使わず、公式サイトからダウンロードしてくる。
;;
;; @see http://howm.sourceforge.jp/
;; @see http://www.emacswiki.org/emacs/HowmMode

;; TODO: !! File howm-vars uses old-style backquotes !!

;;; Code:

(setq howm-keyword-file (locate-user-emacs-file "var/howm/howm-keys"))
(setq howm-history-file (locate-user-emacs-file "var/howm/howm-history"))
(setq howm-menu-lang 'ja)
(setq howm-directory my-memo-dir)
(setq howm-view-title-header "*")
(global-unset-key (kbd "C-x C-z"))
(setq howm-prefix (kbd "C-x C-z"))
(global-set-key (kbd "C-x C-z ,") 'howm-menu)
(setq howm-file-name-format "junk/%Y/%m/%Y-%m-%d-%H%M%S.txt")
(mapc
 (lambda (f) (autoload f "howm" "Hitori Otegaru Wiki Modoki" t))
 '(
   howm-menu
   howm-list-all
   howm-list-recent
   howm-list-grep
   howm-create
   howm-keyword-to-kill-ring))
;(add-hook 'org-mode-hook 'howm-mode)

;;; my-inits-howm.el ends here
