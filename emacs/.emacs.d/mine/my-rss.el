(provide 'my-rss)

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed)
  :init
  ;; URLs in no particular order
  (setq elfeed-use-curl t)
  (setq elfeed-feeds
        '(;; Blogs
          ("http://blog.z3bra.org/rss/feed.xml" blog)

          ;; Linux
          ("http://www.phoronix.com/rss.php" linux news)
          ("http://fedoramagazine.org/feed/" linux)
          ("http://feeds.feedburner.com/mylinuxrig" linux)

          ;; News
          ("http://feeds.arstechnica.com/arstechnica/index/" news)
          ("https://opensource.com/feed" news)

          ;; Reddit
          ("https://www.reddit.com/r/emacs/.rss" emacs reddit)
          ("https://www.reddit.com/r/orgmode/.rss" emacs reddit)
          ))
  :config
  (define-key elfeed-show-mode-map (kbd "j") 'next-line)
  (define-key elfeed-show-mode-map (kbd "k") 'previous-line))
