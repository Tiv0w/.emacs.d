;;; elisp/modules/t--useless.el -*- lexical-binding: t; -*-
;;; Commentary:
; Some useless (or very rarely used packages). Some are here as fallbacks.

;;; Code:

(use-package smex
  :commands smex)

(use-package windmove
  :bind
  ("S-<up>" . windmove-up)
  ("S-<down>" . windmove-down)
  ("S-<left>" . windmove-left)
  ("S-<right>" . windmove-right))

(provide 't--useless)
