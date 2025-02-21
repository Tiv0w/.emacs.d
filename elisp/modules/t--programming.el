;;; elisp/modules/t--programming.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages are the ones I use the most while doing some programming.


(use-package bury-successful-compilation
  :disabled)

(use-package devdocs
  :commands (devdocs-lookup))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; (use-package eglot
;;   :hook ((c++-mode c-mode v-mode) . eglot-ensure)
;;   :config
;;   (setq eglot-stay-out-of '(company))
;;   (setq eglot-stay-out-of '())
;;   (add-to-list 'eglot-server-programs
;;                '((c++-mode c-mode) . ("clangd" "-j=3" "--clang-tidy")))
;;   (add-to-list 'eglot-server-programs '(v-mode . ("vls")))
;;   (add-to-list 'eglot-server-programs '(java-mode . ("jdtls"))))

(use-package fancy-compilation
  :config
  (setq fancy-compilation-override-colors nil)
  (fancy-compilation-mode t))

(use-package flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :hook (after-init . global-flycheck-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package iedit
  :commands (iedit-mode lsp-iedit-highlights))

(use-package litable
  :commands litable-mode)

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p)
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" temp-dir)
        projectile-completion-system 'auto
        projectile-indexing-method 'hybrid)
  ;; Projectile setup for npm
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm install"
                                    :test "npm test"
                                    :run "npm run serve"
                                    :test-suffix ".spec")
  (projectile-mode +1))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :commands restclient-mode
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

(use-package treemacs
  :commands treemacs)

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package vterm
  :commands vterm)

(use-package yasnippet
  :hook (prog-mode . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 't--programming)
