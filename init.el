;;; init.el -*- lexical-binding: t; -*-
;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(let ((default-directory (concat user-emacs-directory "elisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))


;;; Base Emacs configurations
(require 'base)
(require 'base-theme)
(require 'base-font)
(require 'base-extensions)
(require 'base-functions)
(require 'base-global-keys)

(require 'pretty-code)

;;; Programming Languages
;; (require 'lang-python)
;; (require 'lang-ruby)
;; (require 'lang-go)
;; (require 'lang-php)
;; (require 'lang-haskell)
;; (require 'lang-elixir)
;; (require 'lang-rust)
;; (require 'lang-racket)
(require 'lang-clojure)
;; (require 'lang-c)
(require 'lang-web)
(require 'lang-javascript)
(require 'lang-vue)
(require 'lang-typescript)
;; (require 'lang-common-lisp)

(require 'lang-markdown)
;; (require 'lang-latex)

(require 'xah-fly-keys-setup)


;;; And we're done!
(message "The Almighty Editor started in %s." (emacs-init-time))


;; restore some early optimizations, after most of the work is done
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist copy--file-name-handler-alist
          ;; Make gc pauses faster by decreasing the threshold.
          gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))
