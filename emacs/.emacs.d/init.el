;; enter a debugger if there is an error loading stuff
(setq debug-on-error t)
(setq debug-on-quit t)

(require 'cl)

;; switch C-x and C-t as it's easier with Dvorak
(define-key key-translation-map "\C-t" "\C-x")
(define-key key-translation-map "\C-x" "\C-t")

;; define package source list
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; load custom file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(defvar my/did-refresh-packages nil
  "Flag for whether packages have been refreshed yet")

;; install-pkgs will iterate over a list and install each package in it.
;; if my/did-refresh-packages is nil, also refresh the package manager
(defun install-pkgs (list)
  (dolist (pkg list)
    (progn
      (if (not (package-installed-p pkg))
	  (progn
	    (if (not my/did-refresh-packages)
		(progn (package-refresh-contents)
		       (setq my/did-refresh-packages t)))
	    (package-install pkg))))))

;; pin some packages that don't work well using the bleeding edge
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((org-plus-contrib                . "org")
	  (cider                           . "melpa-stable")
	  (ac-cider                        . "melpa-stable")
	  (clojure-mode                    . "melpa-stable")
	  (clojure-mode-extra-font-locking . "melpa-stable")
	  (company-cider                   . "melpa-stable"))))

;; install things needed for all other package installation/configuration.
(install-pkgs '(use-package))
;; load use-package for loading packages everywhere else
(require 'use-package nil t)
;; set to t to debug package loading or nil to disable
(setq use-package-verbose nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load all the custom modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar after-my-hook nil
  "Hooks to run after all of my stuff has been loaded")

(defvar emacs-start-time (current-time)
  "Time Emacs was started.")

(add-to-list 'load-path "~/.emacs.d/mine/")
(add-to-list 'load-path "~/.emacs.d/erc-extras/")

(defmacro try-load (module)
  "Try to load the given module, logging an error if unable to load"
  `(condition-case ex
       (require ,module)
     ('error
      (message "Mine: Unable to load [%s] module: %s", module ex))))

;; My modules
(try-load 'my-core)
(try-load 'my-appearance)
(try-load 'my-navigation)
(try-load 'my-notify)
(try-load 'my-completion)
(try-load 'my-develop)
(try-load 'my-git)
(try-load 'my-org)
(try-load 'my-writing)
(try-load 'my-dired)
(try-load 'my-java)
(try-load 'my-web)
(try-load 'my-shell)
(try-load 'my-mail)
(try-load 'my-irc)
(try-load 'my-rss)

;; hooks
(add-hook 'after-my-hook
	  (lambda ()
	    (message "My stuff has been loaded")))

(defun my/time-since-start ()
  (float-time (time-subtract (current-time)
			     emacs-start-time)))

(add-hook 'after-my-hook
	  `(lambda ()
	     (let ((elapsed (my/time-since-start)))
	       (message "Loading %s...done (%.3fs)"
			,load-file-name elapsed))) t)
(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed (my/time-since-start)))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed))) t)
(run-hooks 'after-my-hook)

;; turn debugging back off if there were no errors
(setq debug-on-error nil)
(setq debug-on-quit nil)
