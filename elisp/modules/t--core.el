;;; elisp/modules/t--core.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages are a very important part of my setup, and need to be well loaded.

;;; Code:

(require 't--packages) ;; load it first because it sets up things for the rest

(require 't--ivy)
(require 't--magit)

(require 't--editing)
(require 't--emacs)
(require 't--env)
(require 't--org)
(require 't--pretty-code)
(require 't--programming)
(require 't--random)
(require 't--useless)
(require 't--visual)

(provide 't--core)
