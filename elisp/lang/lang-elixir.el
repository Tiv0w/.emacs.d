;;; elisp/lang/lang-elixir.el -*- lexical-binding: t; -*-

(use-package elixir-mode
  :defer t
  :hook (elixir-mode . lsp-deferred))

(use-package alchemist
  :hook (elixir-mode . alchemist-mode))

;; (use-package flycheck-mix
;;   :commands (flycheck-mix-setup))

(provide 'lang-elixir)
