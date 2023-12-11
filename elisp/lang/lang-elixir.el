;;; elisp/lang/lang-elixir.el -*- lexical-binding: t; -*-

(use-package elixir-mode)

(use-package alchemist
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

;; (use-package flycheck-mix
;;   :commands (flycheck-mix-setup))

(use-package lsp-mode
  :hook (elixir-mode . lsp)
        (lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil))


(provide 'lang-elixir)
