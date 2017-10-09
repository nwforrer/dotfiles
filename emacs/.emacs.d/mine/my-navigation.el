(provide 'my-navigation)

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package find-file-in-repository
  :ensure t
  :config
  (global-set-key (kbd "C-x f") 'find-file-in-repository))
