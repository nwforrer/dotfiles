(provide 'my-web)

(global-set-key (kbd "C-x m") 'browse-url-at-point)

(use-package eww
  :defer t
  :init
  (setq browse-url-browser-function
        '((".*google.*maps.*" . browse-url-generic)
          ;; Github goes to firefox, but not gist
          ("http.*\/\/github.com" . browse-url-generic)
          ("groups.google.com" . browse-url-generic)
          ("docs.google.com" . browse-url-generic)
          ("melpa.org" . browse-url-generic)
          ("stackoverflow\.com" . browse-url-generic)
          ("apache\.org\/jira" . browse-url-generic)
          ("t.co" . browse-url-generic)
          ("twitter.com" . browse-url-generic)
          ("youtube.com" . browse-url-generic)
          ("." . eww-browse-url)))
  (setq shr-external-browser 'browse-url-generic)
  (setq browse-url-generic-program (executable-find "chromium-browser"))
  (add-hook 'eww-mode-hook #'toggle-word-wrap)
  (add-hook 'eww-mode-hook #'visual-line-mode)
  :config
  (use-package s :ensure t)
  (define-key eww-mode-map "o" 'eww)
  (define-key eww-mode-map "O" 'eww-browse-with-external-browser)
  (define-key eww-mode-map "j" 'next-line)
  (define-key eww-mode-map "k" 'previous-line)

  (use-package eww-lnum
    :ensure t
    :config
    (bind-key "f" #'eww-lnum-follow eww-mode-map)
    (bind-key "U" #'eww-lnum-universal eww-mode-map)))

(use-package link-hint
  :ensure t
  :bind ("C-c f" . link-hint-open-link))

(defun browse-last-url-in-brower ()
  (interactive)
  (save-excursion
    (ffap-next-url t t)))

(global-set-key (kbd "C-c u") 'browse-last-url-in-brower)
