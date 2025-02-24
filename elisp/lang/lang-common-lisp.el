;;; elisp/lang/lang-common-lisp.el -*- lexical-binding: t; -*-

(setq inferior-lisp-program "sbcl")

(use-package sly
  :hook lisp-mode)


(provide 'lang-common-lisp)
