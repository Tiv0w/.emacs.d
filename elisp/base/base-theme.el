;;; elisp/base/base-theme.el -*- lexical-binding: t; -*-

(defun t--setup-italics ()
  "Sets up the italics how I like it.
By default it changes the comments, keywords, builtins and types to italics."
  (interactive)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
  (set-face-attribute 'font-lock-builtin-face nil :slant 'italic)
  (set-face-attribute 'font-lock-type-face nil :slant 'italic))


(use-package doom-themes
  :defer nil
  :demand t
  :init
  (load-theme 'doom-oceanic-next t))

(use-package ewal-doom-themes
  :load-path "./elisp/extlisp/ewal/doom-themes/ewal-doom-themes.el"
  :commands load-theme
  :config
  (progn
    (load-file (concat user-emacs-directory "/elisp/extlisp/ewal/doom-themes/ewal-doom-themes.el"))
    (load-file (concat user-emacs-directory "/elisp/extlisp/ewal/doom-themes/ewal-doom-vibrant-theme.el"))
    (load-theme 'ewal-doom-vibrant t)
    (enable-theme 'ewal-doom-vibrant)))

(provide 'base-theme)
