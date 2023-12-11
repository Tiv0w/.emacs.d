;;; elisp/base/base-font.el -*- lexical-binding: t; -*-
;; Font setup

;; (set-face-attribute 'default nil :font "DejaVu Sans Mono") ;; default
;; (set-face-attribute 'default nil :font "Fira Mono" :height 100)
;; (set-face-attribute 'default nil :font "Iosevka Term SS05" :height 110)
;; (set-face-attribute 'default nil :font "Space Mono")

;; (set-face-attribute 'default nil :font "Julia Mono")
;; (set-face-attribute 'default nil :font "Fantasque Sans Mono")
;; (set-face-attribute 'default nil :font "Nimbus Mono PS")
;; (set-face-attribute 'default nil :font "Space Mono")
;; (set-face-attribute 'default nil :font "Office Code Pro D")
;; (set-face-attribute 'default nil :font "Ubuntu Mono")

(defvar t--font-height (if (getenv "IS_LAPTOP") 120 100))
(defvar t--main-font "Office Code Pro D")

(defun t--setup-fonts (frame)
  "Sets up the fonts with a main font, Unicode fallback and emojis.
This function is hooked to `after-make-frame-functions'."
  (set-face-attribute 'default nil :family t--main-font :height t--font-height)
  (set-face-attribute 'fixed-pitch nil :family t--main-font)
  (set-face-attribute 'variable-pitch nil :family "Google Sans" :height t--font-height)
  (set-fontset-font t nil
                    (font-spec :family "Julia Mono" :height t--font-height))
  (set-fontset-font t '(#x1f000 . #x1faff)
                    (font-spec :family "Blobmoji" :height t--font-height))
  ;; (set-fontset-font t '(#x1f000 . #x1faff)
  ;;                   (font-spec :family "Noto Color Emoji" :height t--font-height))
  (set-face-font 'default t--main-font))

(add-hook 'after-make-frame-functions #'t--setup-fonts)

(defun t--setup-italics ()
  "Sets up the italics how I like it.
By default it changes the comments, keywords, builtins and types to italics."
  (interactive)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
  (set-face-attribute 'font-lock-builtin-face nil :slant 'italic)
  (set-face-attribute 'font-lock-type-face nil :slant 'italic))

(defun t--change-font-height (height)
  "Change the font height of the default and variable-pitch faces."
  (interactive
   (list (read-number (format "Font height (current: %d): " t--font-height))))
  (setq t--font-height height)
  (set-face-attribute 'default nil :height height)
  (set-face-attribute 'variable-pitch nil :height height))

(provide 'base-font)
