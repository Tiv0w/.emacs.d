;;; early-init.el -*- lexical-binding: t; -*-

;;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;;; before package and UI initialization happens.


;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)


;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.


;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)


(defvar copy--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)


;; Setting this value should reduce PGTK build lag
(setq-default pgtk-wait-for-event-timeout 0)
