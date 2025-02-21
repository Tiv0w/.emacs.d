;;; elisp/modules/t--packages.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages are needed for the init of other packages.

;;; Code:

(use-package use-package-chords
  :disabled)

(use-package major-mode-hydra)

(use-package hydra)

(use-package key-chord
  :disabled
  :config
  (setq key-chord-two-keys-delay 0.03))

(provide 't--packages)
