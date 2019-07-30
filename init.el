;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 70 1000 1000))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'base)
(require 'base-theme)
(require 'base-font)
(require 'base-extensions)
(require 'base-functions)
(require 'base-global-keys)

(require 'pretty-code)


;; (require 'lang-python)

;; (require 'lang-ruby)

;; (require 'lang-go)

;; (require 'lang-php)

(require 'lang-web)

(require 'lang-javascript)

;; (require 'lang-haskell)

;; (require 'lang-elixir)

;; (require 'lang-rust)

;; (require 'lang-racket)

;; (require 'lang-c)

(require 'lang-vue)

;; (require 'lang-markdown)

;; (require 'lang-clojure)

;; (require 'lang-latex)

(require 'xah-fly-keys-setup)


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
