;;; elisp/lang/lang-elm.el -*- lexical-binding: t; -*-

(use-package elm-mode
  :defer t
  :hook (elm-mode . lsp-deferred))

(provide 'lang-elm)
