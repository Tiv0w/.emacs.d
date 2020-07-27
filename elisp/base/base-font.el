;;; elisp/base/base-font.el -*- lexical-binding: t; -*-
;; Font setup

;; (set-face-attribute 'default nil :font "DejaVu Sans Mono") ;; default
;; (set-face-attribute 'default nil :font "Fira Mono" :height 100)
;; (set-face-attribute 'default nil :font "Iosevka Term SS05" :height 110)
;; (set-face-attribute 'default nil :font "Space Mono")

;; (set-face-attribute 'default nil :font "Fantasque Sans Mono")
;; (set-face-attribute 'default nil :font "Nimbus Mono PS")
;; (set-face-attribute 'default nil :font "Space Mono")
;; (set-face-attribute 'default nil :font "Office Code Pro D")
;; (set-face-attribute 'default nil :font "Ubuntu Mono")

(defvar-local font-height (if (getenv "IS_LAPTOP") 120 100))

(set-face-attribute 'default nil :family "Office Code Pro D" :height font-height)
(set-face-attribute 'fixed-pitch nil :family "Office Code Pro D")
(set-face-attribute 'variable-pitch nil :family "Google Sans" :height font-height)

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (set-face-font 'default "Office Code Pro D")))

(provide 'base-font)
