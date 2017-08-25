(provide 'my-completion)

(use-package dabbrev
  :init
  (setq dabbrev-case-fold-search nil))

(use-package hippie-exp
  :init
  ;; force hippie-expand completions to be case-sensitive
  (defadvice hippie-expand (around hippie-expand-case-fold activate)
    "Try to do case-sensitive matching (not effective with all functions)."
    (let ((case-fold-search nil))
      ad-do-it))

  :config
  (setq hippie-expand-try-functions-list
	'(;; try to expand word "dynamically", searching the current buffer
	  try-expand-dabbrev
	  ;; try to expand word "dynamically", searching all other buffers
	  try-expand-dabbrev-all-buffers
	  ;; try to expand word "dynamically", searching the kill ring
	  try-expand-dabbrev-from-kill
	  ;; try to cemplete text as a file name, as many characters as unique
	  try-complete-file-name-partially
	  ;; try to complete text as a file name
	  try-complete-file-name
	  ;; try to expand word before point according to all abbrev tables
	  try-expand-all-abbrevs
	  ;; try to complete the current line to an entire line in the buffer
	  try-expand-list
	  ;; try to complete the current line to an entire line in the buffer
	  try-expand-line
	  ;; try to complete the current line to an entire line in a different buffer
	  try-expand-line-all-buffers
	  ;; try to complete as an Emacs Lisp symbol, as many characters as unique
	  try-complete-lisp-symbol-partially
	  ;; try to complete word as an Emacs Lisp symbol
	  try-complete-lisp-symbol)))
	  
(use-package company
  :ensure t
  :diminish company-mode
  ;; stupid flyspell steals the binding i really want, 'C-.'
  :bind (("C-c ." . company-complete)
	 ("C-." . company-complete))
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (use-package company-quickhelp
    :ensure t
    :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
    :config (setq company-quickhelp-delay 2))
  ;; setup statistics for company completions
  (use-package company-statistics
    :ensure t
    :init (add-hook 'after-init-hook #'company-statistics-mode))
  :config
  (setq company-selection-wrap-around t
	;; do or don't automatically start completion after <idle time>
	company-idle-delay 1.0
	;; at least 3 letters need to be there though
	company-minimum-prefix-length 3
	;; show completion numbers for hotkeys
	company-show-numbers t
	;; align annotations to the right
	company-tooltip-align-annotations t
	company-search-regexp-function #'company-search-flex-regexp)
  (bind-keys :map company-active-map
	     ("C-n" . company-select-next)
	     ("C-p" . company-select-previous)
	     ("C-d" . company-show-doc-buffer)
	     ("C-l" . company-show-location)
	     ("<tab>" . company-complete)))

(use-package company-dabbrev
  :init
  (setq company-dabbrev-ignore-case nil
	;; don't downcase dabbrev suggestions
	company-dabbrev-downcase nil
	company-dabbrev-downcase nil))

(use-package company-dabbrev-code
  :init
  (setq company-dabbrev-code-modes t
	company-dabbrev-code-ignore-case nil))

;; use smart-tab to complete everywhere except for ERC, shell, and mu4e
(use-package smart-tab
  :ensure t
  :defer t
  :diminish ""
  :init
  (global-smart-tab-mode 1)
  (setq smart-tab-using-hippie-expand t)
  :config
  (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
  (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
  (add-to-list 'smart-tab-disabled-major-modes 'shell-mode))
