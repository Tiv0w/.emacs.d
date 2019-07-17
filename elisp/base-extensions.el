(use-package use-package-chords
  :config
  (key-chord-mode 1))

(use-package major-mode-hydra)


(use-package all-the-icons)

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package ace-isearch
  :config
  (setq ace-isearch-function-from-isearch 'ace-isearch-swiper-from-isearch)
  (setq ace-isearch-use-jump 'printing-char)
  (setq ace-isearch-function 'avy-goto-word-1)
  (setq ace-isearch-input-length 4)
  (global-ace-isearch-mode +1))

(use-package buffer-move
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Buffer-move"
    (("i" buf-move-up "↑")
     ("j" buf-move-left "←")
     ("k" buf-move-down "↓")
     ("l" buf-move-right "→")))))

(use-package company
  :config
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package dashboard
  :config
  (setq dashboard-startup-banner "~/.emacs.d/private/logos/scream2.png")
  (setq dashboard-banner-logo-title
        "Vous entrez dans le monde de la peur et des poignets cassés")
  (setq dashboard-items '((recents . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (dashboard-setup-startup-hook))

(use-package deadgrep)

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-icon t)
  :hook (after-init . doom-modeline-mode))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

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

(use-package flycheck)

(use-package god-mode
  :config
  (defun god-mode-update-cursor ()
    (setq cursor-type (if god-local-mode
                          '(hbar . 2)
                        't)))
  :hook
  ((god-mode-enabled . god-mode-update-cursor)
   (god-mode-disabled . god-mode-update-cursor)))

(use-package counsel
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


(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'column)
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

(use-package hlinum
  :config
  (hlinum-activate))

(use-package hydra)

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.03)
  (key-chord-mode 1))

(use-package linum
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))

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
        neo-window-fixed-size nil)
  ;; Disable linum for neotree
  (add-hook 'neo-after-create-hook #'linum-mode))

(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org"))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))

(use-package page-break-lines)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package popwin
  :config
  (popwin-mode 1))

(use-package projectile
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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package scratch)

(use-package smartparens
  :chords
  (("kj" . sp-backward-slurp-sexp)
   ("kl" . sp-forward-slurp-sexp)
   ("ij" . sp-backward-barf-sexp)
   ("il" . sp-forward-barf-sexp)
   ("jl" . sp-slurp-hybrid-sexp)
   ("ik" . sp-transpose-hybrid-sexp)))

(use-package smex)

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

(use-package wgrep)

(use-package yasnippet
  :config
  (yas-global-mode 1))

;; (use-package xah-fly-keys
;;   :config
;;   (xah-fly-keys-set-layout "qwerty")

;;   (define-key xah-fly-leader-key-map (kbd "f") 'counsel-switch-buffer)
;;   (define-key xah-fly-key-map (kbd "8") 'er/expand-region)
;;   (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;;   (xah-fly-keys 1))


(provide 'base-extensions)
