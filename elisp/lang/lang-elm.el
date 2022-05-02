;;; elisp/lang/lang-elm.el -*- lexical-binding: t; -*-

(use-package elm-mode)

(use-package lsp-mode
  :hook (elm-mode . lsp))


(provide 'lang-elm)
