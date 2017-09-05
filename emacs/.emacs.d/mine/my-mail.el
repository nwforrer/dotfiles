(provide 'my-mail)

(add-to-list 'load-path "~/usr/local/src/mu/mu4e")
(use-package mu4e
  :if window-system
  :config
  (add-hook 'message-mode-hook 'turn-on-flyspell)
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)

  (use-package mu4e-alert
    :ensure t)
  (use-package mu4e-maildirs-extension
    :ensure t)

  ;; mu4e as the default Emacs mail client
  (setq mail-user-agent 'mu4e-user-agent)

  ;; save attachments to the downloads directory
  (setq mu4e-attachment-dir "~/Downloads")

  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-sent-folder "/sent")
  (setq mu4e-trash-folder "/trash")

  ;; Don't save messages to Sent, Gmail takes care of that
  (setq mu4e-sent-messages-behavior 'delete)

  ;; setup some shortcuts
  ;; switch to the inbox -- press ji
  ;; archive messages -- ma
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"       . ?i)
           ("/sent"        . ?s)
           ("/trash"       . ?t)
           ("/archive"     . ?a)
           ("/memo-list"   . ?m)
           ("/rdu-list"    . ?r)
           ("/friday-list" . ?f)))

  ;; update mail using 'U' in the main view
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300) ;; update every 5 minutes

  ;; user config
  (setq user-mail-address "nforrer@redhat.com"
        user-full-name "Nicholas Forrer"
        mu4e-compose-signature (concat "Nick Forrer\n"
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
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
