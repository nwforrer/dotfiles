; My simple .emacs file.
; Inspired by Casey Muratori's .emacs file

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-S-c") 'compile)

(setq compilation-directory-locked nil)
(setq my-makescript "./build.sh")

; Clock
(display-time)

; Startup windowing
(split-window-horizontally)

; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.h$"      . c++-mode)
         ("\\.c$"      . c++-mode)
         ) auto-mode-alist))

; C/C++ mode handling
(defun my-c-hook ()
  (defun my-header-format ()
    "Format the given file as a header file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "#if !defined(")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H)\n")
    (insert "/* ======================================================================\n")
    (insert "   $File: $\n")
    (insert "   $Date: $\n")
    (insert "   $Revision: $\n")
    (insert "   $Creator: Nick Forrer $\n")
    (insert "   $Notice: Licensed under the GNU Public License. $\n")
    (insert "   ====================================================================== */\n")
    (insert "\n")
    (insert "#define ")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H\n")
    (insert "#endif")
  )

  (defun my-source-format ()
    "Format the given file as a source file."
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "/* ======================================================================\n")
    (insert "   $File: $\n")
    (insert "   $Date: $\n")
    (insert "   $Revision: $\n")
    (insert "   $Creator: Nick Forrer $\n")
    (insert "   $Notice: Licensed under the GNU Public License. $\n")
    (insert "   ====================================================================== */\n")
  )
  
  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]h" buffer-file-name) (my-header-format))
        ((string-match "[.]cpp" buffer-file-name) (my-source-format)))

  (defun my-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.cpp" buffer-file-name)
        (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
        (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
          (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if CorrespondingFileName (find-file CorrespondingFileName)
      (error "Unable to find a corresponding file")))
  (defun my-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (my-find-corresponding-file)
    (other-window -1))

  (define-key c++-mode-map [f12] 'my-find-corresponding-file)
  (define-key c++-mode-map [M-f12] 'my-find-corresponding-file-other-window)
)

(add-hook 'c-mode-common-hook 'my-c-hook)

; Bright-red TODOs
 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
 (make-face 'font-lock-fixme-face)
 (make-face 'font-lock-study-face)
 (make-face 'font-lock-important-face)
 (make-face 'font-lock-note-face)
 (mapc (lambda (mode)
	 (font-lock-add-keywords
	  mode
	  '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	    ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
	    ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
	fixme-modes)
 (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
 (modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
 (modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
 (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

; Compilation
(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p my-makescript) t
      (cd "../")
      (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile my-makescript))
  (other-window 1))
(global-set-key (kbd "C-S-b") 'make-without-asking)
; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
