;;; elisp/modules/t--magit.el -*- lexical-binding: t; -*-
;;; Commentary:
; Magit setup.

;;; Code:

(use-package magit
  :commands magit-status
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-section-initial-visibility-alist '((stashes . hide)
                                                 (unpushed . show))
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(use-package magit-popup
  :after magit)

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

(use-package magit-gitflow
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package git-messenger
  :after magit
  :commands git-messenger:popup-message)

(use-package browse-at-remote
  :after magit
  :commands browse-at-remote)

(use-package gitmoji
  :disabled
  :load-path "~/prog/elisp/gitmoji/"
  :config
  (gitmoji-commit-mode t))


(provide 't--magit)
