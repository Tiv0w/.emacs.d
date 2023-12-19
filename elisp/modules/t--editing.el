;;; elisp/modules/t--editing.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages are here to enhance editing (useful for a text editor to be able to edit text).

;;; Code:

(use-package aggressive-indent-mode
  :disabled
  :load-path "~/prog/elisp/aggressive-indent-mode/"
  :config
  (global-aggressive-indent-mode)
  (add-hook 'makefile-mode-hook
            (lambda ()
              (aggressive-indent-mode -1)) 0 t))

(use-package avy
  :commands (avy-goto-char
             avy-goto-char-timer)
  :config
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line))

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

(use-package crux
  :commands
  (crux-duplicate-and-comment-current-line-or-region
   crux-rename-file-and-buffer
   crux-delete-file-and-buffer))

(use-package deadgrep
  :ensure-system-package (rg . ripgrep)
  :commands deadgrep)

(use-package expand-region
  :commands (er/expand-region)
  :bind
  ("C-=" . er/expand-region))

(use-package embrace
  :commands embrace-commander)

(use-package ialign
  :commands ialign)

(use-package iy-go-to-char
  :load-path "./elisp/extlisp/iy-go-to-char.el"
  :commands iy-go-to-char)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          org-self-insert-command
          forward-char
          backward-char
          previous-line
          next-line
          xah-fly-insert-mode-activate
          xah-fly-command-mode-activate
          mwheel-scroll)))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  :config
  (define-key mc/keymap (kbd "RET") nil))

(use-package smartparens
  :chords
  (("kj" . sp-backward-slurp-sexp)
   ("kl" . sp-forward-slurp-sexp)
   ("ij" . sp-backward-barf-sexp)
   ("il" . sp-forward-barf-sexp)
   ("jl" . sp-slurp-hybrid-sexp)
   ("ik" . sp-transpose-hybrid-sexp)))

(use-package string-inflection
  :config
  (defun string-inflection-cycle-auto ()
    "Switching string case by major-mode."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     (t
      (string-inflection-ruby-style-cycle)))))

(use-package stupid-indent-mode
  :hook conf-mode)

(use-package subword
  :hook ((prog-mode conf-mode) . subword-mode))

(use-package ace-window
  :config
  (setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers)
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (set-face-attribute
   'aw-leading-char-face nil
   :foreground "white" :background "red"
   :weight 'bold :height 2.5 :box '(:line-width 10 :color "red")))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(provide 't--editing)
