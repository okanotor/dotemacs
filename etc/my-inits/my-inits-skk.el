;;; my-inits-skk.el --- setting for skk -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; SKK(Simple Kana Kanji Converter) の設定

;;; Code:
(when (require 'skk-autoloads nil t)
  (global-set-key (kbd "C-x C-j") 'skk-mode)
  (global-set-key [zenkaku-hankaku] 'skk-mode)
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
  (let ((var-skk-dir (locate-user-emacs-file "var/skk")))
    (setq skk-large-jisyo (concat var-skk-dir "/SKK-JISYO.L"))
    (setq skk-jisyo (concat var-skk-dir "/private-jisyo.txt"))
    (setq skk-backup-jisyo (concat var-skk-dir "/private-jisyo.txt.bak"))
    (setq skk-record-file (concat var-skk-dir "/skk-record"))
  )
  (setq skk-egg-like-newline t)
  (setq default-input-method "japanese-skk")
  (setq skk-sticky-key ";")

  (when (require 'org-mode nil t)
    (defadvice org-return (around skk-in-org-table activate)
      "use return to skk-kakutei in table of org-mode"
      (cond ((and (org-at-table-p) skk-henkan-mode) (skk-kakutei))
            (t ad-do-it)))
    )
  )

;;; my-inits-skk.el ends here

