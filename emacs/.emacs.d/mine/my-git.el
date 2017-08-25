(provide 'my-git)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init (add-hook 'magit-mode-hook 'my/turn-on-hl-line)
  :config
  (setq git-commit-summary-max-length 70))

(use-package ediff
  :init
  (setq
   ;; always split nicely for wide screens
   ediff-split-window-function 'spit-window-horizontally)
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents
       ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents
       ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))
