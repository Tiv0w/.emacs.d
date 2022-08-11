;;; elisp/base/base-theme.el -*- lexical-binding: t; -*-

;; Transparency support
;; Works well on Emacs [26, 27, 28] built with toolkit [lucid, gtk] on Manjaro
;; Requires a compositor, works well with picom
(defun t--transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive
   (let ((transparency-val (frame-parameter (selected-frame) 'alpha)))
     (list (read-number
            (format "Transparency (0 to 100, current value: %d): " transparency-val)))))
  (set-frame-parameter (selected-frame) 'alpha value))


(defun t--light-env-setup ()
  "Sets up my light working environement."
  (interactive)
  (load-theme 'doom-solarized-light t)
  (t--setup-italics)
  (t--transparency 100))

(defun t--dark-env-setup ()
  "Sets up my dark working environement."
  (interactive)
  (load-theme 'doom-vibrant t)
  (t--setup-italics)
  (t--transparency 90))


(use-package doom-themes
  :defer nil
  :demand t
  :init
  (load-theme 'doom-vibrant t))

(use-package ewal-doom-themes
  :disabled
  :defer nil
  :demand t
  :config
  (enable-theme 'ewal-doom-vibrant))


(provide 'base-theme)
