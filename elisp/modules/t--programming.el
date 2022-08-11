;;; elisp/modules/t--programming.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages are the ones I use the most while doing some programming.


(use-package apheleia
  :config
  (push '(vfmt . ("v" "fmt" "-w")) apheleia-formatters)
  (push '(v-mode . vfmt) apheleia-mode-alist))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-require-match 'never))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package devdocs
  :commands (devdocs-lookup))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
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

(use-package flycheck)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package litable
  :commands litable-mode)

(use-package projectile
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" temp-dir)
        projectile-completion-system 'ivy
        projectile-indexing-method 'hybrid)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Projectile setup for npm
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm install"
                                    :test "npm test"
                                    :run "npm run serve"
                                    :test-suffix ".spec")
  (projectile-global-mode))

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
