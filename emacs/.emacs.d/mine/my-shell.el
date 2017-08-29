(provide 'my-shell)

;; make sure things that use $EDITOR will use the
;; currently running emacs instance
(use-package with-editor
  :ensure t
  :init
  (progn
    (add-hook 'shell-mode-hook 'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

(defun my/setup-eshell ()
  (interactive)
  (local-set-key (kbd "M-P") 'eshell-previous-prompt)
  (local-set-key (kbd "M-N") 'eshell-next-prompt)
  (local-set-key (kbd "M-R") 'eshell-previous-matching-input))

;; helper to sudo edit a file
(defun sudoec (file)
  (interactive)
  (find-file (concat "/sudo::" (expand-file-name file))))

(use-package eshell
  :commands (eshell eshell-command)
  :bind ("C-c m" . eshell)
  :init
  (require 'em-smart)
  (setq eshell-glob-case-insensitive nil
        eshell-error-if-no-glob nil
        eshell-scroll-to-bottom-on-input nil
        eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)
  :config
  (defalias 'emacs 'find-file)
  (defalias 'em 'find-file)
  (defalias 'd 'dired)
  (defalias 'sec 'sudoec)
  (setenv "PAGER" "cat"))
