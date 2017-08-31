(provide 'my-music)

(use-package mingus
  :ensure t
  :init
  (setenv "MPD_HOST" "localhost")
  (setenv "MPD_PORT" "6600"))

(defhydra my/hydra-mpd nil
  "MPD Actions"
  ("p" (progn (save-window-excursion
                (async-shell-command "mpc toggle" (get-buffer-create "*tmp*"))))
   "Play/Pause")
  ("/" mingus-search "Search" :exit t)
  ("c" (message "Currently Playing: %s"
                (shell-command-to-string "mpc status")) "Currently Playing")
  ("s" mingus "Show Mingus" :exit t)
  ("m" mingus "Mingus" :exit t)
  ("<" (progn (require 'mpc) (mpc-prev)
              (message "Currently Playing: %s"
                (shell-command-to-string "mpc status"))) "Previous")
  (">" (progn (require 'mpc) (mpc-next)
              (message "Currently Playing: %s"
                (shell-command-to-string "mpc status"))) "Next")
  ("+" (dotimes (i 5) (mingus-vol-up)) "Louder")
  ("-" (dotimes (i 5) (mingus-vol-down)) "Quieter")
  ("n" (ansi-term (executable-find "ncmpcpp"))
   "ncmpcpp" :exit t)
  ("q" nil "Quit"))
