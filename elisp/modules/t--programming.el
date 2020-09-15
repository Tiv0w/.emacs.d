;;; elisp/modules/t--programming.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages are the ones I use the most while doing some programming.

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

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package flycheck)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package litable
  :commands litable-mode)

(use-package neotree
  :config
  (setq neo-theme 'icons
        neo-smart-open t
        neo-window-fixed-size nil))
  ;; Disable linum for neotree
  ;; (add-hook 'neo-after-create-hook #'(lambda (arg) (linum-mode 1))))

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

(use-package yasnippet
  :hook (prog-mode . yas-global-mode))

(provide 't--programming)
