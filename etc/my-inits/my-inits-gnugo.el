;;; my-inits-gnugo.el --- setting for gnugo -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; GNU Go の設定

;;; Code:

(when (require 'gnugo nil t)
  (let ((boardsize 9))
    (add-to-list 'gnugo-option-history (format "--boardsize %d --color black --level 1" boardsize))
    
    (when (require 'gnugo-imgen nil t)
      (setq gnugo-xpms (gnugo-imgen-create-xpms boardsize))
      )
    )
  )

;;; my-inits-gnugo.el ends here
