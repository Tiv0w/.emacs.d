;;; elisp/base/base-theme.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;; Things related to themes and theming

;;; Code:
;; Transparency support
;; Works well on Emacs [26, 27, 28, 29, 30] built with toolkit [lucid, gtk, pgtk] on Manjaro
;; Requires a compositor, works well with picom
;; Also tested for Wayland on 29 and 30
(defun t--transparency (value)
  "Set the transparency of the frame window to VALUE.
0 equals to transparent, and 100 is fully opaque."
  (interactive
   (let ((transparency-val (frame-parameter
                            (selected-frame)
                            (if (version< emacs-version "29") 'alpha 'alpha-background))))
     (list (read-number
            (format "Transparency (0 to 100, current value: %d): " transparency-val)))))
  (set-frame-parameter
   (selected-frame)
   (if (version< emacs-version "29") 'alpha 'alpha-background)
   value))

(defvar t--preferred-light-theme 'doom-solarized-light)
(defvar t--preferred-dark-theme 'doom-vibrant)

(defun t--light-env-setup ()
  "Set up my light working environement."
  (interactive)
  (load-theme t--preferred-light-theme t)
  (t--setup-italics)
  (t--transparency 100))

(defun t--dark-env-setup ()
  "Set up my dark working environement."
  (interactive)
  (load-theme t--preferred-dark-theme t)
  (t--setup-italics)
  (t--transparency 100))

(defun t--toggle-between-light-and-dark-themes ()
  "Toggle between `t--preferred-dark-theme' and `t--preferred-light-theme'."
  (interactive)
  (if (eq (car custom-enabled-themes) t--preferred-light-theme)
      (t--dark-env-setup)
    (t--light-env-setup)))

(use-package doom-themes
  :defer nil
  :demand t
  :init
  (load-theme t--preferred-dark-theme t))

(use-package ewal-doom-themes
  :disabled
  :defer nil
  :demand t
  :config
  (enable-theme 'ewal-doom-vibrant))


(provide 'base-theme)
;;; base-theme.el ends here
