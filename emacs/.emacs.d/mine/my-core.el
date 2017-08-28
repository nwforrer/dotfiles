(provide 'my-core)

;; config personal info
(setq user-full-name "Nicholas Forrer"
      user-mail-address "nforrer@redhat.com")

;; always prefer UTF-8
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; turn on syntax highlighting for all buffers
(global-font-lock-mode t)

;; raise the maximum number of logs in *Messages*
(setq message-log-max 16384)

;; increase delay to assume Emacs is idle
(setq idle-update-delay 2)

;; when selecting a region, then start typing, delete the region
(delete-selection-mode 1)

;; when changing buffers, disable the current buffers mark
(transient-mark-mode 1)

;; turn off some modes
(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; never beep
(setq ring-bell-function (lambda ()))
;; don't show startup message
(setq inhibit-startup-screen t)

;; show lines/colums in the mode-line
(line-number-mode 1)
(column-number-mode 1)

;; ignore case when using completion for file names
(setq read-file-name-completion-ignore-case t)

;; just type 'y' instead of 'yes' at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; confirm before killing emacs, in graphical sessions
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; move around lines based on how they're displayed, rather than the actual line
(setq line-move-visual t)

;; hide the mouse while typing
(setq make-pointer-invisible t)

;; set fill-column to 80 and set tab width to 2
(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; fix some weird color escape sequences
(setq system-uses-terminfo nil)

;; resolve symlinks
(setq-default find-file-visit-truename t)

;; require a newline at the end of files
(setq require-final-newline t)

;; uniquify buffers
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; split windows a bit better (don't split horizontally)
(setq split-height-threshold nil)
(setq split-width-threshold 180)

;; seed the random number generator
(random t)

;; switch to unified diffs by default
(setq diff-switches "-u")

;; turn on auto-fill mode in text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(use-package diminish
  :init (diminish 'auto-fill-function ""))

;; set the internal calculator not to go to scientific form so quickly
(setq calc-display-sci-low -5)

;; bury the *scratch* buffer, but never kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; start a server if not running, but only for gui
(require 'server nil t)
(use-package server
  :if window-system
  :init
  (when (not (server-running-p server-name))
    (server-start)))

;; prettify all the symbols, if available (an Emacs 24.4 feature)
(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (push '("fn" . ?ƒ) prettify-symbols-alist)))
  (global-prettify-symbols-mode +1))

;; save the *scratch* buffer periodically, and restore when starting emacs
(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name 'persistent-scratch-file-name'."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max) "~/.emacs.d/persistent-scratch")))

(defun load-persistent-scratch ()
  "Load the contents of 'persistent-scratch-file-name' into the scratch buffer,
clearing it's contents first."
  (if (file-exists-p "~/.emacs.d/persistent-scratch")
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents "~/.emacs.d/persistent-scratch"))))

(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)

;; change clipboard settings to better integrate with Linux
(setq x-select-enable-clipboard t)
;; treat clipboard input as UTF-8 string first; compound text next, etc
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; store temporary files in a single place instead of everywhere
;; delete auto-save-files
(setq delete-auto-save-files t)
;; create the directory for backups if it doesn't exist
(when (not (file-exists-p "~/.emacs.d/backups"))
  (make-directory "~/.emacs.d/backups"))
(setq-default backup-directory-alist
              '((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/backups" t)))
;; delete old backups silently
(setq delete-old-versions t)

;; set up the site-lisp directory so that things can be loaded from there if applicable
(when (file-exists-p "~/.emacs.d/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp"))

;; ignore case when performing completion
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; hydra, for a unified interface to parts of my config
(use-package hydra :ensure t)

(defhydra my/hydra-about-emacs ()
  "
    About Emacs                                                        [_q_] quit
    ^^--------------------------------------------------------------------------
    PID:             %s(emacs-pid)
    Uptime:          %s(emacs-uptime)
    Init time:       %s(emacs-init-time)
    Directory:       %s(identity user-emacs-directory)
    Invoked from:    %s(concat invocation-directory invocation-name)
    Version:         %s(identity emacs-version)

    User Info
    ^^--------------------------------------------------------------------------
    User name:       %s(user-full-name)
    Login (real):    %s(user-login-name) (%s(user-real-login-name))
      UID (real):    %s(user-uid) (%s(user-real-uid))
      GID (real):    %s(group-gid) (%s(group-real-gid))
    Mail address:    %s(identity user-mail-address)

    System Info
    ^^--------------------------------------------------------------------------
    System name:     %s(system-name)
    System type:     %s(identity system-type)
    System config:   %s(identity system-configuration)
    "
  ("q" nil nil))

(defhydra my/hydra nil
  "
╭────────────────────────────────────────────────────────╯
  [_a_] Org Agenda       [_E_] ERC       [_m_] Mail
  [_t_] Toggle map       [_T_] Twitter   [_M_] Music
  [_s_] Skeletons        [_P_] Prodigy   [_g_] Gnus
  [_p_] Proced           [_W_] Weather   [(] Macros
  [_c_] Multi-compile    [_R_] RSS       [`] Errors
  [_d_] Downloads        [_D_] Debbugs   [_C_] ES-CC
  [_b_] Project's Eshell [_S_] Smerge    [_B_] Bookmarks
  [_q_] quit
"
  ("A" my/hydra-about-emacs/body :exit t)
  ("E" (when (y-or-n-p "Really start ERC?") (start-erc)) :exit t)
  ("R" elfeed :exit t))

;; bind the main hydra menu to M-t
(global-set-key (kbd "M-t") 'my/hydra/body)

;; linux specific stuff
(when (eq system-type 'gnu/linux)
  ;; whether to use GTK tooltips or emacs ones
  ;; (setq x-gtk-use-system-tooltips nil)
  (setq x-gtk-use-system-tooltips t)

  (defun tsp/max-fullscreen ()
    (interactive)
    (toggle-frame-maximized))

  ;; fullscreen
  (add-hook 'after-init-hook #'tsp/max-fullscreen)

  (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
