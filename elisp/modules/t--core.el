;;; elisp/modules/t--core.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages are a very important part of my setup, and need to be well loaded.

;;; Code:

(require 't--packages) ;; load it first because it sets up things for the rest

(require 't--ivy)
(require 't--magit)
(require 't--lsp)
(require 't--tree-sitter)
(require 't--formatter)

(require 't--pretty-code) ;; pretty-code is required by org

(require 't--editing)
(require 't--emacs)
(require 't--env)
(use-package t--language
  :ensure nil
  :defer 10)
(use-package t--org
  :ensure nil
  :defer 10)
(require 't--pdf)
(require 't--programming)
(require 't--random)
(require 't--sudo)
(require 't--useless)
(require 't--visual)

;; Load configuration for particular tools and systems
(require 't--ansible)

(provide 't--core)
