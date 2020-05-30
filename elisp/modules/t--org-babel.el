;;; elisp/modules/t--org-babel.el -*- lexical-binding: t; -*-
;;; Commentary:
; Org-babel setup.

;;; Code:

;; Emacs builtins
(require 'ob-js)

;; External packages
(use-package ob-typescript)
(use-package ob-restclient)

(provide 't--org-babel)
