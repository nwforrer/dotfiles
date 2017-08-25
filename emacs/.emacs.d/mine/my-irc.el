(provide 'my-irc)

(defun start-erc ()
  (interactive)
  (load-file "~/.ercpass")
  (use-package erc
    :init
    (setq erc-nick "nforrer"
          erc-hide-list '("JOIN" "PART" "QUIT")
          erc-fill-column 100
          erc-server-reconnect-timeout 5
          erc-server-reconnect-attempts 3
          erc-autojoin-channels-alist '(("irc.devel.redhat.com" "#paas" "#iso")
                                        ("irc.corp.redhat.com" "#iso" "#productivity")))
    (defun eos/disable-font-lock ()
      (font-lock-mode -1))
    ;; ERC is crazy, for some reason it doesn't like font-lock...
    (add-hook 'erc-mode-hook #'eos/disable-font-lock))
  (use-package erc-hl-nicks :ensure t)
  (use-package ercn
    :ensure t
    :init
    (setq ercn-notify-rules
          '((current-nick . all)
            (query-buffer . all)))

    (use-package s :ensure t)
    (when (fboundp 'alert)
      (defun do-notify (nickname message)
        (interactive)
        ;; Alert not needed, Sauron handles this now
        ;; (alert (concat nickname ": "
        ;;                (s-trim (s-collapse-whitespace message)))
        ;;        :title (buffer-name))
        (message "[%s] %s: %s"
                 (buffer-name) nickname (s-trim (s-collapse-whitespace message))))
      (add-hook 'ercn-notify-hook 'do-notify)))

  (defun erc-cmd-UNTRACK (&optional target)
    "Add TARGET to the list of target to be tracked."
    (if target
        (erc-with-server-buffer
          (let ((untracked (car (erc-member-ignore-case target erc-track-exclude))))
            (if untracked
                (erc-display-line
                 (erc-make-notice (format "%s is not currently tracked!" target))
                 'active)
              (add-to-list 'erc-track-exclude target)
              (erc-display-line
               (erc-make-notice (format "Now not tracking %s" target))
               'active))))

      (if (null erc-track-exclude)
          (erc-display-line (erc-make-notice "Untracked targets list is empty") 'active)

        (erc-display-line (erc-make-notice "Untracked targets list:") 'active)
        (mapc #'(lambda (item)
                  (erc-display-line (erc-make-notice item) 'active))
              (erc-with-server-buffer erc-track-exclude))))
    t)


  (defun erc-cmd-TRACK (target)
    "Remove TARGET of the list of targets which they should not be tracked.
   If no TARGET argument is specified, list the contents of `erc-track-exclude'."
    (when target
      (erc-with-server-buffer
        (let ((tracked (not (car (erc-member-ignore-case target erc-track-exclude)))))
          (if tracked
              (erc-display-line
               (erc-make-notice (format "%s is currently tracked!" target))
               'active)
            (setq erc-track-exclude (remove target erc-track-exclude))
            (erc-display-line
             (erc-make-notice (format "Now tracking %s" target))
             'active)))))
    t)

  (defun my/add-server-to-chan-name (orig-fun server port target)
    (let ((generated-name (funcall orig-fun server port target)))
      (concat (cl-subseq server 4 7) "-" generated-name)))

  (advice-add 'erc-generate-new-buffer-name :around #'my/add-server-to-chan-name)

  (setq erc-log-channels-directory "~/.erc/logs/")
  (setq erc-save-buffer-on-part t)

  (erc :server "irc.devel.redhat.com"
       :port 6667
       :nick "nforrer")
  (erc-tls :server "irc.corp.redhat.com"
           :port 6667
           :nick "nforrer"
           :password rhat-corp-pass))
