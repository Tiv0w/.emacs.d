;;; elisp/modules/t--magit.el -*- lexical-binding: t; -*-
;;; Commentary:
; Magit setup.

;;; Code:

(use-package magit
  :commands (magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read
	magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(use-package magit-popup)

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package forge
  :after magit
  :mode-hydra
  (forge-topic-mode
   (:color blue :quit-key "q")
   ("Forge topic"
    (("m" magit-edit-thing "edit at point")
     ("p" forge-create-post "create post")
     ("b" forge-browse-topic "browse topic")))))

(use-package gitmoji
  :load-path "~/prog/elisp/gitmoji/"
  :config
  (gitmoji-commit-mode t))

(provide 't--magit)
