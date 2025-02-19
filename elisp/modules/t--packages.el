;;; elisp/modules/t--packages.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages are needed for the init of other packages.

;;; Code:

(use-package use-package-chords
  :config
  ;; (key-chord-mode 1)
  )

(use-package major-mode-hydra)

(use-package hydra)

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.03))

(provide 't--packages)
