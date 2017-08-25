(provide 'my-dired)

(defun my/dired-mode-hook ()
  (setq-local truncate-lines t))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (use-package dired-x
    :init (setq-default dired-omit-files-p t)
    :config
    (add-to-list 'dired-omit-extensions ".DS_Store"))
  (customize-set-variable 'diredp-hide-details-initially-flag nil)
  (use-package dired+
    :ensure t)
  (use-package dired-aux
    :init
    (use-package dired-async
      :ensure async))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq ls-lisp-dirs-first t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-dwim-target t
        ;; -F marks links with @
        dired-ls-F-marks-symlinks t
        delete-by-moving-to-trash t
        ;; don't auto refresh dired
        global-auto-revert-non-file-buffers nil
        wdired-allow-to-change-permissions t)
  (define-key dired-mode-map (kbd "C-M-u") #'dired-up-directory)
  (define-key dired-mode-map (kbd "M-o") #'my/dired-open)
  (define-key dired-mode-map (kbd "C-x C-q") #'wdired-change-to-wdired-mode)
  (bind-key "l" #'dired-up-directory dired-mode-map)
  (bind-key "M-!" #'async-shell-command dired-mode-map)
  (add-hook 'dired-mode-hook 'my/turn-on-hl-line)
  (add-hook 'dired-mode-hook #'my/dired-mode-hook))

(use-package quick-preview
  :ensure t
  :init
  (global-set-key (kbd "C-c q") 'quick-preview-at-point)
  (define-key dired-mode-map (kbd "Q") 'quick-preview-at-point))

(use-package async :ensure t)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
