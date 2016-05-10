;;; my-inits-calendar.el --- setting for calendar -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; カレンダーのための設定
;; japanese-holiday を使用して、カレンダー上に日本の祝日を表示させる
;; japanese-holiday は ELPA からダウンロードする
;; @see http://d.hatena.ne.jp/rubikitch/20090216/1234746280

;;; Code:

(when (require 'calendar nil t)
  ;; キーマップの設定
  (define-key calendar-mode-map (kbd "M-f") 'calendar-forward-month)
  (define-key calendar-mode-map (kbd "M-b") 'calendar-backward-month)
  (define-key calendar-mode-map (kbd "M-n") 'calendar-forward-year)
  (define-key calendar-mode-map (kbd "M-p") 'calendar-backward-year)

  ;; 「今日」をマークする
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  (when (require 'holidays nil t)
    (eval-after-load "holidays"
      '(progn
         (when (require 'japanese-holidays nil t)
           ;; 祝日カレンダーを設定
           (setq calendar-holidays
                 (append
                  japanese-holidays
                  holiday-local-holidays
                  holiday-other-holidays
                  ))

           ;; 祝日をカレンダーに表示
           (setq calendar-mark-holidays-flag t)

           ;; 土曜日・日曜日に色をつける
           ;; 土曜日と日曜日は別の色にする
           (setq japanese-holiday-weekend '(0 6)
                 japanese-holiday-weekend-marker
                 '(holiday nil nil nil nil nil japanese-holiday-saturday))
           (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
           (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend))))
    )
  )

;;; my-inits-calendar.el ends here
