(provide 'my-notify)

(use-package alert
  :ensure t
  :config
  (when (eq system-type 'gnu/linux)
    (setq alert-default-style 'notifications)))

(use-package sauron
  :ensure t
  :init
  (when (not (boundp 'dbus-compiled-version))
    ;; remove dbus if it is not compiled
    (require 'sauron)
    (setq sauron-modules (remove 'sauron-dbus sauron-modules)))

  (setq sauron-max-line-length 120
        sauron-watch-nicks '("nforrer" "nicksmaddog")
        sauron-nick-insensitivity 20
        sauron-frame-geometry "120x36+0+0")
  ;; filter out IRC spam
  (defun tsp/hide-irc-user-spam (origin priority msg &optional properties)
    (or (string-match "^*** Users" msg)))
  (add-hook 'sauron-event-block-functions #'tsp/hide-irc-user-spam)

  (sauron-start-hidden)
  ;; need to stop tracking notifications, because sauron will be sending them!
  (sauron-notifications-stop)
  (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)
  :commands (sauron-toggle-hide-show)
  :bind ("M-o" . sauron-toggle-hide-show)
  :config
  ;; add the unread sauron notification count to the modeline
  (add-to-list 'global-mode-string '(cdr (sauron-count-events)))

  (defun my/compilation-finish (buffer msg)
    "Send a souron notification for compilation completing"
    (interactive)
    (sauron-add-event 'compilation
                      3
                      (format "[%s]: %s" buffer msg)
                      (lambda () (switch-to-buffer-other-window "*compilation*"))
                      nil))
  (add-to-list 'compilation-finish-functions #'my/compilation-finish)

  (defun finish()
    "Generic function for signaling something is \"done\"."
    (interactive)
    (sauron-add-event major-mode
                      3
                      (concat "Finished command in " (buffer-name))
                      (lambda () (switch-to-buffer-other-window (buffer-name)))
                      nil)))

;; notifications for running eshell commands
(use-package eshell
  :config
  ;; seconds a command must take before showing alert
  (setq my/eshell-time-before-alert 5.0)

  (defun my/eshell-precommand ()
    (interactive)
    (setq-local my/eshell-command-start-time (current-time)))

  (defun my/eshell-command-finished ()
    (interactive)
    (when (and (boundp 'my/eshell-command-start-time)
               (> (float-time (time-subtract (current-time) my/eshell-command-start-time))
                  my/eshell-time-before-alert))
      (sauron-add-event major-mode
                        (if (zerop eshell-last-command-status)
                            3 4)
                        (format "EShell: command [%s] finished, status: %s"
                                eshell-last-command-name
                                eshell-last-command-status)
                        (lambda () (switch-to-buffer-other-window (buffer-name)))
                        nil)))

  (add-hook 'eshell-pre-command-hook #'my/eshell-precommand)
  (add-hook 'eshell-post-command-hook #'my/eshell-command-finished))
