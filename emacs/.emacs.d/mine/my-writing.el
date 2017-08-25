(provide 'my-writing)

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("github\\.com.*\\.txt\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh"))
  (add-hook 'markdown-mode-hook #'flyspell-mode))

(use-package yaml-mode
  :ensure t)

(use-package adoc-mode
  :ensure t)
