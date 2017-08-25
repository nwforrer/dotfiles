(provide 'my-java)

;; decide between meghanada or eclim
(setq my/use-meghanada t)
(setq my/use-eclim nil)

;; java imports
(use-package java-imports
  :ensure t
  :config
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  (add-hook 'java-mode-hook 'java-imports-scan-file))

;; meghanada
(use-package meghanada
  :ensure t
  :init
  (when my/use-meghanada
    (add-hook 'java-mode-hook #'meghanada-mode)
    (add-hook 'java-mode-hook 'flycheck-mode)
    ;;    (bind-key "C-c M-." 'meghanada-jump-declaration java-mode-map)))
    ))
