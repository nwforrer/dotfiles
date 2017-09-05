(provide 'my-develop)

;; remove some back-ends from vc-mode, no need to check these things
;; (i use magit for git anyway)
(setq vc-handled-backends '(git svn))

;; hide the lighter for subword-mode
(use-package subword
  :diminish subword-mode)

(use-package log4j-mode
  :ensure t
  :init
  (add-hook #'log4j-mode-hook #'view-mode)
  (add-hook #'log4j-mode-hook #'read-only-mode)
  (add-hook #'log4j-mode-hook 'my/turn-on-hl-line))

;; highlight idle things, but only in certain modes
(use-package idle-highlight-mode
  :ensure t
  :init
  (add-hook 'java-mode-hook #'idle-highlight-mode)
  (add-hook 'emacs-lisp-mode-hook #'idle-highlight-mode)
  (add-hook 'clojure-lisp-mode-hook #'idle-highlight-mode)
  :config
  (setq idle-highlight-idle-time 1.5))

;; show the compilation buffer with "C-x c"
(defun my/last-compilation-buffer ()
  "Display the last compilation buffer in current window."
  (interactive)
  (if (buffer-live-p compilation-last-buffer)
      (set-window-buffer (get-buffer-window) compilation-last-buffer)
    (message "Last compilation buffer is killed.")))

(global-set-key (kbd "C-x c") #'my/last-compilation-buffer)

;; always save files for compilation
(setq compilation-ask-about-save nil)

;; use M-n and M-p to jump between the same variable in multiple places
(use-package smartscan
  :ensure t
  :init (add-hook #'prog-mode-hook #'smartscan-mode)
  :config
  (bind-key "M-'" #'other-window smartscan-map)
  (setq smartscan-symbol-selector "symbol"))

;; toggle boolean states
(use-package bool-flip
  :ensure t
  :bind ("C-c C-b" . bool-flip-do-flip))

;; use "C-c C-p" and "C-c C-n" to navigate by function declaration
(defun my/previous-function ()
  (interactive)
  (beginning-of-defun))
(defun my/next-function ()
  (interactive)
  (beginning-of-defun -1))

(bind-key "C-c C-p" 'my/previous-function prog-mode-map)
(bind-key "C-c C-n" 'my/next-function prog-mode-map)

;; dtrt-indent (do the right thing) to try to figure out best indentation
;; (invoked manually for now)
(use-package dtrt-indent
  :ensure t
  :diminish t
  :config
  (setq dtrt-indent-active-mode-line-info ""))

;; additional highlighting
;; highlight numbers
(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))
;; highlight quoted symbols
(use-package highlight-quoted
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'highlight-quoted-mode))
;; highlight predefined symbols in elisp
(use-package highlight-defined
  :ensure t
  :init
  (add-hook 'emacs-lisp-hook #'highlight-defined-mode))
;; highlight operators like & and ; in C-like modes
(use-package highlight-operators
  :ensure t
  :init
  (add-hook 'c-mode-common-hook #'highlight-operators-mode))
;; highlight escape sequences
(use-package highlight-escape-sequences
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'hes-mode))

;; auto indent
(electric-indent-mode 1)

;; Ignore electric indentation for python and yaml
(defun electric-indent-ignore-mode (char)
  "Ignore electric indentation for python-mode"
  (if (or (equal major-mode 'python-mode)
          (equal major-mode 'yaml-mode))
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-mode)

(electric-layout-mode 1)

;; add highlighting for FIXME, TODO, NOCOMMIT
(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code TODO"
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOCOMMIT\\(?:(.*)\\)?:?\\)\\>"  1 'error prepend))))

(add-hook 'prog-mode-hook #'my/add-watchwords)

;; paredit everywhere
(eval-after-load 'paredit-everywhere
  '(define-key paredit-everywhere-mode-map (kbd "M-s") nil))
(use-package paredit-everywhere
  :ensure t
  :disabled t
  :init (add-hook 'prog-mode-hook 'paredit-everywhere-mode))


;; offline documentation with helm-dash
;; TODO: make sure to run M-x helm-dash-install-docset
(use-package helm-dash
  :ensure t
  :bind (("C-c D" . helm-dash))
  :init
  (setq helm-dash-common-docsets '("ElasticSearch")
        helm-dash-min-length 2)
  :config
  (defun my/use-java-docset ()
    (setq-local helm-dash-docsets '("Java")))
  (defun my/use-elisp-docset ()
    (setq-local helm-dash-docsets '("Emacs Lisp")))
  (add-hook 'java-mode-hook #'my/use-java-docset)
  (add-hook 'emacs-lisp-mode-hook #'my/use-elisp-docset))

;; automatically detect indent style of file
(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode t))

;; Javascript
(setq-default js-indent-level 2)
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config (js2-imenu-extras-setup))
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; Pug
(use-package pug-mode
  :ensure t
  :mode "\\.pug\\'")

;; Shell scripting
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; Common Lisp
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program (executable-find "clisp"))
  (setq slime-contribs '(slime-fancy)))
