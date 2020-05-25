;;; elisp/modules/t--magit.el -*- lexical-binding: t; -*-
;;; Commentary:
; Magit setup.

;;; Code:

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

(use-package gitmoji-commit
  :load-path "~/prog/elisp/gitmoji-commit/")

(provide 't--magit)