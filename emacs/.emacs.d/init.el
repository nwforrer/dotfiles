;; need org to tangle
(require 'org)

;; open the .org config
(find-file (concat user-emacs-directory "config.org"))
;; tangle it
(org-babel-tangle)
;; load it
(load-file (concat user-emacs-directory "config.el"))
;; byte-compile it
;(byte-compile-file (concat user-emacs-directory "config.el"))


;;; init.el ends here
