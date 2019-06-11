;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'base)
(require 'base-theme)
(require 'base-extensions)
(require 'base-functions)
(require 'base-global-keys)

(require 'pretty-code)


(require 'lang-python)

(require 'lang-ruby)

(require 'lang-go)

(require 'lang-php)

(require 'lang-web)

(require 'lang-javascript)

(require 'lang-haskell)

(require 'lang-elixir)

(require 'lang-rust)

(require 'lang-racket)

(require 'lang-c)

(require 'lang-vue)

(require 'lang-markdown)

(require 'lang-clojure)

(require 'xah-fly-keys-setup)