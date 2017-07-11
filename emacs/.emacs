;; INSTALL PACKAGES
;; --------------------------------------
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    gruvbox-theme
    elpy
    json-mode
    js2-mode
    projectile
    eclim
;    yasnippet
    magit))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;;---------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'gruvbox t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; Disable the bell sound, and blink instead
(setq visible-bell 1)

;; Set the font size
(set-face-attribute 'default nil :height 120)

;; Magit Status keybinding
(global-set-key (kbd "C-x g") 'magit-status)

;; PYTHON
;; Enable the elpy package (for Python)
(elpy-enable)

;; Javascript configs
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; JSON configs
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; Smart tab completion
(require 'smart-tab)
(global-smart-tab-mode 1)

;; gnus
(require 'nnir) ; enable searching across mail

;; JAVA
(require 'eclim)
(setq eclimd-autostart t)
; show compilation errors in the echo area
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
; company-mode for pop-up auto completion
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)
(defun my-java-mode-hook ()
    (eclim-mode t))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eclim-eclipse-dirs
   (quote
    ("~/usr/local/src/eclipse" "/Applications/eclipse" "/usr/lib/eclipse" "/usr/local/lib/eclipse" "/usr/share/eclipse" "/Applications/Eclipse.app/Contents/Eclipse/")))
 '(eclim-executable "~/usr/local/src/eclipse/eclim")
 '(package-selected-packages
   (quote
    (flycheck eclim elpy js2-mode json-mode json-reformat json-snatcher gruvbox-theme yaml-mode pug-mode better-defaults projectile magit)))
 '(python-shell-interpreter "python3")
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
