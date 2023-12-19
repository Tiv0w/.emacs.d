;;; elisp/lang/lang-ocaml.el -*- lexical-binding: t; -*-

(use-package caml-mode
  :ensure nil
  :defer t)

(use-package tuareg
  :hook (caml-mode . tuareg-mode))

(use-package merlin
  :hook ((tuareg-mode caml-mode) . merlin-mode))

(provide 'lang-ocaml)
