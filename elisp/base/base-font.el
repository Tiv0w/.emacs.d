;;; elisp/base/base-font.el -*- lexical-binding: t; -*-
;; Font setup

;; (set-face-attribute 'default nil :font "DejaVu Sans Mono") ;; default
;; (set-face-attribute 'default nil :font "Fira Mono" :height 100)
;; (set-face-attribute 'default nil :font "Iosevka Term SS05" :height 110)
(set-face-attribute 'default nil :family "Office Code Pro D" :height 100)
(set-face-attribute 'fixed-pitch nil :family "Office Code Pro D")
(set-face-attribute 'variable-pitch nil :family "Google Sans" :height 100)

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (set-face-font 'default "Office Code Pro D")))

(provide 'base-font)
