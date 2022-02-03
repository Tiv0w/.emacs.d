;;; elisp/lang/lang-ocaml.el -*- lexical-binding: t; -*-

(use-package tuareg)

(use-package merlin
  :hook ((tuareg-mode caml-mode) . merlin-mode))

(provide 'lang-ocaml)
