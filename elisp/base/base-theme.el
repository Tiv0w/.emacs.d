;;; elisp/base/base-theme.el -*- lexical-binding: t; -*-

;; Transparency support
;; Works well on Emacs [26, 27] built with toolkit [lucid, gtk] on Manjaro
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
  (load-theme 'doom-one-light t)
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
  :load-path "./elisp/extlisp/ewal/doom-themes/ewal-doom-themes.el"
  :commands load-theme
  :config
  (progn
    (load-file (concat user-emacs-directory "/elisp/extlisp/ewal/doom-themes/ewal-doom-themes.el"))
    (load-file (concat user-emacs-directory "/elisp/extlisp/ewal/doom-themes/ewal-doom-vibrant-theme.el"))
    (load-theme 'ewal-doom-vibrant t)
    (enable-theme 'ewal-doom-vibrant)))


(provide 'base-theme)
