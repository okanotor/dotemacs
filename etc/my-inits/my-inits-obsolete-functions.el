;;

;; if-found
;; autoload-if-found
;; @see http://www.sodan.org/~knagano/emacs/dotemacs.html
(defun autoload-if-found (function &optional file docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))

;; load-if-found
;; →除去したい
(defun load-if-found (file &optional noerror nomessage nosuffix must-suffix)
  "load file iff. FILE has found."
  (and (locate-library file)
       (load file noerror nomessage nosuffix must-suffix)))

;; require-if-found
;; →除去したい
(defun require-if-found (feature &optional filename noerror)
  "require feature iff. library has found."
  (let ((library-name (if filename filename (symbol-name feature))))
    (and
     (locate-library library-name)
     (require feature filename noerror))))
