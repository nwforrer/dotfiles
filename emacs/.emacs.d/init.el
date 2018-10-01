;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; disable ui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;; disable startup screen
(setq inhibit-startup-screen t)
;; only ask y on n, not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; split window on startup, and open todo list
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (split-window-right)
	    (org-todo-list)))

;; custom functions
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; install use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

(setq use-package-compute-statistics t)

;; set some global variables
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq use-package-always-ensure t)

;; set the default font
(set-frame-font "Liberation Mono-12")

;; automatically follow log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; include quicklisp slime helper
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "sbcl")
;; (setq slime-contribs '(slime-fancy))

;; install theme
(setq *my-theme* 'spacemacs)
(cond ((equal *my-theme* 'gruvbox)
       (use-package gruvbox-theme))
      ((equal *my-theme* 'spacemacs)
       (use-package spacemacs-theme
	 :defer t
	 :init
	 (load-theme 'spacemacs-dark t)
	 (setq spacemacs-theme-org-agenda-height nil
	       spacemacs-theme-org-height nil))
       (use-package spaceline
       	 :demand t
       	 :init
       	 (setq powerline-default-separator 'arrow-fade)
       	 :config
       	 (require 'spaceline-config)
       	 (spaceline-spacemacs-theme))))

;; install packages
(use-package org
  :defer t
  :config
  (setq org-startup-indented t
	org-directory "~/usr/doc"
	org-agenda-files '("~/usr/doc")))
(use-package smart-tab
  :config
  (global-smart-tab-mode 1))
(use-package company
  :hook (after-init . global-company-mode))
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode))
(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))
(use-package eshell
  :bind ("<f12>" . eshell))
(use-package magit
  :bind ("C-x g" . magit-status))
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))
(use-package podcaster
  :defer t
  :config
  (setq podcaster-feeds-urls '("https://pinecast.com/feed/emacscast")
	podcaster-mp3-player "ffplay"))
;;; various language modes
(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :interpreter "js2-mode")
(use-package yaml-mode
  :defer t)
(use-package markdown-mode
  :defer t)
(use-package dockerfile-mode
  :defer t)
(use-package ng2-mode
  :defer t)
(use-package pug-mode
  :defer t)
(use-package restclient
  :defer t
  :mode "\\.http\\'"
  :interpreter "restclient-mode")
(use-package csv-mode
  :defer t)
(use-package nasm-mode
  :defer t
  :mode "\\.s\\'")
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (lisp-mode       . enable-paredit-mode)
	 (slime-repl-mode . (lambda ()
			      (paredit-mode +1)
			      (override-slime-repl-bindings-with-paredit)))))

;;; java
(use-package autodisass-java-bytecode
  :defer t)
(use-package google-c-style
  :defer t
  :commands
  (google-set-c-style))
(use-package meghanada
  :hook ((java-mode . meghanada-mode)
	 (java-mode . flycheck-mode)
	 (java-mode . (lambda ()
			(google-set-c-style)
			(setq c-basic-offset 4)))))

;;; helm
(use-package helm
  :bind (("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list)
	 ("M-x" . helm-M-x))
  :config
  :demand
  (helm-mode 1))
(use-package projectile
  :config
  (projectile-mode))
(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (which-key spaceline spacemacs-theme podcaster smart-tab paredit slime company-mode nasm-mode csv-mode restclient pug-mode realgud autodisass-java-bytecode meghanada dockerfile-mode markdown-mode helm-projectile ng2-mode helm js2-mode gruvbox-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
