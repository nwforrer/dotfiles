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
    mu4e-alert
    mu4e-maildirs-extension
;    yasnippet
    magit))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;;---------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 127))

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

;; E-mail (mu4e)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; user mu4e as the default Emacs mail client
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder "/sent")
(setq mu4e-trash-folder "/trash")

;; don't save message to Sent Messages, Gmail/IMAP take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some shortcuts
;; switch to the inbox -- press ji
;; archive messages -- ma
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"     . ?i)
         ("/sent"      . ?s)
         ("/trash"     . ?t)
         ("/archive"   . ?a)))

;; update mail using 'U' in the main view
(setq
 mu4e-get-mail-command "offlineimap"
 mu4e-update-interval 300) ;; update every 5 minutes

;; user config
(setq
 user-mail-address "nforrer@redhat.com"
 user-full-name "Nicholas Forrer"
 mu4e-compose-signature (concat
                         "Nick Forrer\n"
                         "Software Applications Engineer, Document Management and Collaboration\n"
                         "Red Hat\n"
                         "IM: nforrer\n"))

;; sending mail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "nforrer@redhat.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; show maildirs summary on the main window
(mu4e-maildirs-extension)

;; display alerts on new mail
(mu4e-alert-enable-notifications)
(mu4e-alert-set-default-style 'notifications)
(setq mu4e-alert-interesting-mail-query
      (concat "maildir:/INBOX AND date:today..now"
              " AND flag:unread"))

;; display stuff on modeline as well as notify
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)


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
    (mu4e-maildirs-extension mu4e-alert weechat company-emacs-eclim flycheck eclim elpy js2-mode json-mode json-reformat json-snatcher gruvbox-theme yaml-mode pug-mode better-defaults projectile magit)))
 '(python-shell-interpreter "python3")
 '(send-mail-function (quote smtpmail-send-it))
 '(transient-mark-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
