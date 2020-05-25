;;; elisp/modules/t--useless.el -*- lexical-binding: t; -*-
;;; Commentary:
; Some useless (or very rarely used packages). Some are here as fallbacks.

;;; Code:

;; (use-package all-the-icons-ivy
;;   :after ivy
;;   :config
;;   (all-the-icons-ivy-setup))

(use-package smex
  :commands smex)

(use-package windmove
  :bind
  ("S-<up>" . windmove-up)
  ("S-<down>" . windmove-down)
  ("S-<left>" . windmove-left)
  ("S-<right>" . windmove-right))

(use-package wgrep
  :defer t)

(provide 't--useless)
