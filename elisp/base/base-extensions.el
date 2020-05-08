;;; elisp/base/base-extensions.el -*- lexical-binding: t; -*-

(use-package use-package-chords
  :config
  (key-chord-mode 1))

(use-package major-mode-hydra)


(use-package all-the-icons)

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package buffer-move
  :commands buffer-move-hydra/body
  :init
  (require 'move-border)
  :pretty-hydra
  ((:color red :quit-key "q")
   ("Buffer-move"
    (("i" buf-move-up "↑")
     ("k" buf-move-down "↓")
     ("j" buf-move-left "←")
     ("l" buf-move-right "→"))
    "Switch window"
    (("," xah-next-window-or-frame "other-window-or-frame"))
    "Resize"
    (("J" move-border-left "←")
     ("K" move-border-down "↓")
     ("I" move-border-up "↑")
     ("L" move-border-right "→")
     ("5" balance-windows "balance"))
    "Split"
    (("4" split-window-right "horizontally")
     ("3" split-window-below "vertically"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase)
     ("-" text-scale-decrease "out")
     ("0" text-scale-adjust "reset")))))

(use-package company
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-idle-delay 0))

(use-package counsel-spotify
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

(use-package dashboard
  :config
  (let ((logo "~/.emacs.d/private/logos/logo.png"))
    (if (file-exists-p logo)
        (setq dashboard-startup-banner logo)))
  (setq dashboard-banner-logo-title
        "Vous entrez dans le monde de la peur et des poignets cassés")
  (setq dashboard-items '((recents . 7)
                          (projects . 7)
                          (bookmarks . 2)
                          (agenda . 2)
                          (registers . 2)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (dashboard-setup-startup-hook))

(use-package deadgrep
  :commands deadgrep)

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-rainbow
  :after dired
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-icon t)
  (display-battery-mode)
  :hook (after-init . doom-modeline-mode))

(use-package dumb-jump
  :commands (dumb-jump-go
	     dumb-jump-go-prompt
	     dumb-jump-back)
  :config
  (setq dumb-jump-selector 'ivy))

(use-package ediff
  :commands ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package elcord
  :commands elcord-mode
  :config
  (setq elcord-use-major-mode-as-main-icon t))

(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; (use-package fancy-battery
;;   :hook (doom-modeline-mode . fancy-battery-mode))

(use-package flycheck)

(use-package helpful
  :commands (helpful-callable
	     helpful-at-point
	     helpful-variable
	     helpful-key))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package ibuffer-vc
  :after ibuffer
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                vc-relative-file)))
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package counsel
  :commands (counsel-M-x find-file counsel-find-file)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-ag)
  :config
  (counsel-projectile-on))

(use-package ivy
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

;; (use-package all-the-icons-ivy
;;   :after ivy
;;   :config
;;   (all-the-icons-ivy-setup))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column))
  ;; (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

(use-package hydra)

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.03))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package neotree
  :config
  (setq neo-theme 'icons
        neo-smart-open t
        neo-window-fixed-size nil))
  ;; Disable linum for neotree
  ;; (add-hook 'neo-after-create-hook #'(lambda (arg) (linum-mode 1))))

(use-package nlinum
  :hook ((prog-mode text-mode conf-mode) . nlinum-mode)
  :config
  (setq nlinum-format " %3d "))

(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org")
        org-todo-keywords '((sequence "TODO" "DOING" "TEST" "DONE"))
        org-log-done 'time)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :mode-hydra
  (org-mode
   (:title "Org" :color blue :quit-key "q")
   ("TODO"
    (("t" org-todo "cycle state" :color amaranth)
     ("y" org-insert-todo-subheading "insert"))
    "Clock"
    (("i" org-clock-in "in")
     ("o" org-clock-out "out")
     ("e" org-clock-modify-effort-estimate "update effort" :color amaranth)
     ("p" org-clock-update-time-maybe "compute time")))))

(use-package org-projectile
  :bind
  (("C-c n p" . org-projectile-project-todo-completing-read)
   ("C-c c" . org-capture))
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :after org
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))

(use-package parrot
  :hook (prog-mode . parrot-mode)
  :config
  (parrot-set-parrot-type 'default)
  (dolist (entry '((:rot ("let" "const" "var"))
                   (:rot ("beforeCreate" "created" "beforeMount" "mounted" "beforeDestroy" "destroyed"))
                   (:rot ("$off" "$emit"))))
    (add-to-list 'parrot-rotate-dict entry)))

(use-package page-break-lines)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package popwin
  :config
  (popwin-mode 1))

(use-package projectile
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))

  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; Projectile setup for npm
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm install"
                                    :test "npm test"
                                    :run "npm run serve"
                                    :test-suffix ".spec")

  (projectile-global-mode))

(use-package rainbow-mode
  :hook (prog-mode conf-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf")
	recentf-max-saved-items 25)
  (recentf-mode 1))

(use-package restclient
  :mode-hydra
  (restclient-mode
   (:color blue :quit-key "q")
   ("Restclient"
    (("j" restclient-jump-prev "prev")
     (";" restclient-jump-next "next")
     ("SPC" restclient-http-send-current-stay-in-window "send & stay")
     ("RET" restclient-http-send-current "send & go")))))

(use-package scratch
  :commands scratch)

(use-package smartparens
  :chords
  (("kj" . sp-backward-slurp-sexp)
   ("kl" . sp-forward-slurp-sexp)
   ("ij" . sp-backward-barf-sexp)
   ("il" . sp-forward-barf-sexp)
   ("jl" . sp-slurp-hybrid-sexp)
   ("ik" . sp-transpose-hybrid-sexp)))

(use-package smex
  :commands smex)

(use-package stupid-indent-mode
  :hook conf-mode)

(use-package undo-tree
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.7)
  (which-key-mode))

(use-package windmove
  :bind
  ("S-<up>" . windmove-up)
  ("S-<down>" . windmove-down)
  ("S-<left>" . windmove-left)
  ("S-<right>" . windmove-right))

(use-package wgrep
  :defer t)

(use-package yasnippet
  :hook
  (prog-mode . yas-global-mode))

;; (use-package xah-fly-keys
;;   :config
;;   (xah-fly-keys-set-layout "qwerty")

;;   (define-key xah-fly-leader-key-map (kbd "f") 'counsel-switch-buffer)
;;   (define-key xah-fly-key-map (kbd "8") 'er/expand-region)
;;   (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;;   (xah-fly-keys 1))


(provide 'base-extensions)
