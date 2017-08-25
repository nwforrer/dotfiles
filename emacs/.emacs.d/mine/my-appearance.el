(provide 'my-appearance)

;(;defvar my/background 'light)
(defvar my/background 'dark)

;; paren-face adds a face for parens, which is used by themes to darken the parens.
(use-package paren-face
  :ensure t
  :init (global-paren-face-mode))

;; don't use dialog boxes, just ask inside Emacs
(setq use-dialog-box nil)

;; set the color theme.
;; leuven-theme for a light background, or tomorrow-night for dark.
(if (eq my/background 'dark)
    (progn
      (use-package color-theme-sanityinc-tomorrow
	:ensure t
	:init
	(load-theme 'sanityinc-tomorrow-night t)
	;; just slightly more bright foreground text, default is #c5c8c6
	(set-face-foreground 'default "#e5e8e6")
	;; darken newline whitespace marks and blend into the background
	(require 'whitespace)
	(set-face-foreground 'whitespace-newline "#555856")
	(set-face-background 'whitespace-newline (face-attribute 'default :background)))))

;; set fonts
;; the original font height (so it can be restored later)
(setq my/original-height 105)
(defun my/setup-fonts ()
  (when (eq window-system 'x)
    ;; default font and variable-pitch fonts
    (set-face-attribute 'default nil
			:family "Hack"
			:height my/original-height)
    (dolist (face '(mode-line mode-line-inactive minibuffer-prompt))
      (set-face-attribute face nil :family "Hack"
			  :height my/original-height))
    (set-face-attribute 'variable-pitch nil
			:family "DejaVu Sans" :height my/original-height)
    ))

(when (eq window-system 'x)
  (add-hook 'after-init-hook #'my/setup-fonts))

;; smooth scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-margin 5
      hscroll-step 5)
			    
;; line highlighting
;; flag whether to turn on hl-line or not
(setq my/hl-line-enabled t)
(defun my/turn-on-hl-line ()
  (interactive)
  (when my/hl-line-enabled
    (hl-line-mode 1)))
(defun my/turn-off-hl-line ()
  (interactive)
  (hl-line-mode -1))
;; turn it on by default (if enabled) in prog-mode
(add-hook 'prog-mode-hook #'my/turn-on-hl-line)
(add-hook 'mu4e-view-mode-hook #'my/turn-on-hl-line)
(add-hook 'erc-mode-hook #'my/turn-on-hl-line)
