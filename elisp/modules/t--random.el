;;; elisp/modules/t--random.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages are just some various packages, mostly funny and/or not so much used

(use-package counsel-spotify
  :disabled
  :after counsel
  :commands counsel-spotify-hydra/body
  :config
  (setq counsel-spotify-client-id "515f0ff545a349bcadf98efab945972f"
        counsel-spotify-client-secret "7618bf445df14b568782b13e37cf63e6")
  :pretty-hydra
  ((:title "Counsel-Spotify" :color amaranth :quit-key "q")
   ("Commands"
    (("p" counsel-spotify-previous "previous")
     ("n" counsel-spotify-next "next")
     ("SPC" counsel-spotify-toggle-play-pause "play/pause")
     ("x" counsel-spotify-play "play (unused)"))
    "Search"
    (("a" counsel-spotify-search-artist "artist" :color blue)
     ("b" counsel-spotify-search-album "album" :color blue)
     ("t" counsel-spotify-search-track "track" :color blue)
     ("r" counsel-spotify-search-tracks-by-artist "tracks-by-artist" :color blue)
     ("e" counsel-spotify-search-tracks-by-albums "tracks-by-albums" :color blue)))))

(use-package elcord
  :commands elcord-mode
  :config
  (setq elcord-use-major-mode-as-main-icon t))

(use-package parrot
  :disabled
  :hook (prog-mode . parrot-mode)
  :config
  (parrot-set-parrot-type 'default)
  (dolist (entry '((:rot ("let" "const" "var"))
                   (:rot ("beforeCreate" "created" "beforeMount" "mounted" "beforeDestroy" "destroyed"))
                   (:rot ("$off" "$emit"))
                   (:rot ("∃" "∀"))
                   (:rot ("∨" "∧"))))
    (add-to-list 'parrot-rotate-dict entry)))

(use-package secret-mode
  :load-path (lambda () (concat user-emacs-directory "elisp/extlisp/secret-mode.el"))
  :commands secret-mode)

(use-package browse-url-dwim
  :commands (browse-url-dwim
             browse-url-dwim-guess
             browse-url-dwim-search)
  :config
  (setq browse-url-dwim-search-url "https://duckduckgo.com/?q="))

(use-package screenshot
  :disabled
  :load-path (lambda () (concat user-emacs-directory "elisp/extlisp/screenshot/")))

(use-package carbon-now-sh
  :commands (carbon-now-sh))

(use-package fireplace
  :commands (fireplace))

(use-package minesweeper
  :commands (minesweeper))

(use-package speed-type
  :commands (speed-type-text speed-type-region speed-type-buffer))

(use-package explain-pause-mode
  :vc (:fetcher github :repo "lastquestion/explain-pause-mode")
  :commands explain-pause-mode)

(provide 't--random)
