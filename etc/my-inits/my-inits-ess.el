;;; my-inits-ess.el --- setting for R -*- mode: emacs-lisp; coding: utf-8-unix -*-

;;; Commentary:
;; Emacs から R を使うための ESS の設定。
;; @see http://futurismo.biz/archives/2840

;;; Code:

(when (require 'ess-site nil t)
  ;; R 起動時にワーキングディレクトリを訊ねない
  (setq ess-ask-for-ess-directory nil)

  ;; auto-complete を有効にする
  (setq ess-use-auto-complete t)

  ;; データビューワを利用できるようにする
  (when (require 'ess-R-data-view nil t)
    (define-key ess-mode-map (kbd "C-c v") 'ess-R-dv-pprint)
    )

  ;; helm-R を利用できるようにする
  (when (require 'helm-R nil t)
    (define-key ess-mode-map (kbd "C-c h") 'helm-for-R)
    (define-key inferior-ess-mode-map (kbd "C-c h") 'helm-for-R)
    )
  )

;; @see http://sheephead.homelinux.org/2009/10/26/1673/
;; -> anything 側に持っていくべきか、それとも
;; eval-after-load とすべきか
;; add anything source "R Help"
;; (setq anything-c-source-R-help
;;       '((name . "R objects / help")
;;         (init . (lambda ()
;;                   ;; this grabs the process name associated with the buffer
;;                   (setq anything-c-ess-local-process-name ess-local-process-name)))
;;         (candidates . (lambda ()
;;                         (condition-case nil
;;                             (ess-get-object-list anything-c-ess-local-process-name)
;;                           (error nil))))
;;         (action
;;          ("help" . ess-display-help-on-object)
;;          ("head (10)" . (lambda (obj-name)
;;                           (ess-execute (concat "head(" obj-name ", n = 10)n") nil (concat "R head: " obj-name))))
;;          ("head (100)" . (lambda (obj-name)
;;                            (ess-execute (concat "head(" obj-name ", n = 100)n") nil (concat "R head: " obj-name))))
;;          ("tail" . (lambda (obj-name)
;;                      (ess-execute (concat "tail(" obj-name ", n = 10)n") nil (concat "R tail: " obj-name))))
;;          ("str" . (lambda (obj-name)
;;                     (ess-execute (concat "str(" obj-name ")n") nil (concat "R str: " obj-name))))
;;          ("summary" . (lambda (obj-name)
;;                         (ess-execute (concat "summary(" obj-name ")n") nil (concat "R summary: " obj-name))))
;;          ("view source" . (lambda (obj-name)
;;                             (ess-execute (concat "print(" obj-name ")n") nil (concat "R object: " obj-name))))
;;          ("dput" . (lambda (obj-name)
;;                      (ess-execute (concat "dput(" obj-name ")n") nil (concat "R dput: " obj-name)))))
;;         (volatile)))
;; (add-to-list 'anything-sources anything-c-source-R-help)

;; add anything source "R local"
;; (setq anything-c-source-R-local
;;       '((name . "R local objects")
;;         (init . (lambda ()
;;                   ;; this grabs the process name associated with the buffer
;;                   (setq anything-c-ess-local-process-name ess-local-process-name)
;;                   ;; this grabs the buffer for later use
;;                   (setq anything-c-ess-buffer (current-buffer))))
;;         (candidates . (lambda ()
;;                         (let (buf)
;;                           (condition-case nil
;;                               (with-temp-buffer
;;                                 (progn
;;                                   (setq buf (current-buffer))
;;                                   (with-current-buffer anything-c-ess-buffer
;;                                     (ess-command "print(ls.str(), max.level=0)\n" buf))
;;                                   (split-string (buffer-string) "\n" t)))
;;                             (error nil)))))
;;         (display-to-real . (lambda (obj-name) (car (split-string obj-name " : " t))))
;;         (action
;;          ("str" . (lambda (obj-name)
;;                     (ess-execute (concat "str(" obj-name ")n") nil (concat "R str: " obj-name))))
;;          ("summary" . (lambda (obj-name)
;;                         (ess-execute (concat "summary(" obj-name ")n") nil (concat "R summary: " obj-name))))
;;          ("head (10)" . (lambda (obj-name)
;;                           (ess-execute (concat "head(" obj-name ", n = 10)n") nil (concat "R head: " obj-name))))
;;          ("head (100)" . (lambda (obj-name)
;;                            (ess-execute (concat "head(" obj-name ", n = 100)n") nil (concat "R head: " obj-name))))
;;          ("tail" . (lambda (obj-name)
;;                      (ess-execute (concat "tail(" obj-name ", n = 10)n") nil (concat "R tail: " obj-name))))
;;          ("print" . (lambda (obj-name)
;;                       (ess-execute (concat "print(" obj-name ")n") nil (concat "R object: " obj-name))))
;;          ("dput" . (lambda (obj-name)
;;                      (ess-execute (concat "dput(" obj-name ")n") nil (concat "R dput: " obj-name)))))
;;        (volatile)))
;(add-to-list 'anything-sources anything-c-source-R-local)

;;; my-inits-ess.el ends here
