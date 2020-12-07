;;; elisp/modules/t--random.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages are just some various packages, mostly funny and/or not so much used

(use-package counsel-spotify
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
  :hook (prog-mode . parrot-mode)
  :config
  (parrot-set-parrot-type 'default)
  (dolist (entry '((:rot ("let" "const" "var"))
                   (:rot ("beforeCreate" "created" "beforeMount" "mounted" "beforeDestroy" "destroyed"))
                   (:rot ("$off" "$emit"))
                   (:rot ("∃" "∀"))
                   (:rot ("∨" "∧"))))
    (add-to-list 'parrot-rotate-dict entry)))

(use-package browse-url-dwim
  :config
  (setq browse-url-dwim-search-url "https://duckduckgo.com/?q="))

(provide 't--random)
