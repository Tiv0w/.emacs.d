;;; elisp/modules/t--editing.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages are here to enhance editing (useful for a text editor to be able to edit text).

;;; Code:

(use-package aggressive-indent-mode
  :hook (prog-mode . aggressive-indent-mode)
  :load-path "~/prog/elisp/aggressive-indent-mode/")

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package buffer-move
  :commands buffer-move-hydra/body
  :init
  (require 'move-border)
  :pretty-hydra
  ((:color red :quit-key "q")
   ("Buffer-move"
    (("i" buf-move-up "↑")
     ("k" buf-move-down "↓")
     ("j" buf-move-left "←")
     ("l" buf-move-right "→"))
    "Switch window"
    (("," xah-next-window-or-frame "other-window-or-frame"))
    "Resize"
    (("J" move-border-left "←")
     ("K" move-border-down "↓")
     ("I" move-border-up "↑")
     ("L" move-border-right "→")
     ("5" balance-windows "balance"))
    "Split"
    (("4" split-window-right "horizontally")
     ("3" split-window-below "vertically"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase)
     ("-" text-scale-decrease "out")
     ("0" text-scale-adjust "reset")))))

(use-package deadgrep
  :ensure-system-package (rg . ripgrep)
  :commands deadgrep)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package smartparens
  :chords
  (("kj" . sp-backward-slurp-sexp)
   ("kl" . sp-forward-slurp-sexp)
   ("ij" . sp-backward-barf-sexp)
   ("il" . sp-forward-barf-sexp)
   ("jl" . sp-slurp-hybrid-sexp)
   ("ik" . sp-transpose-hybrid-sexp)))

(use-package stupid-indent-mode
  :hook conf-mode)

(use-package subword
  :hook ((prog-mode conf-mode) . subword-mode))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil
	undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(provide 't--editing)
