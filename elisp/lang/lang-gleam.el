;;; lang-gleam.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; My setup for Gleam
;;; First language to use treesit integration

;;; Code:

(use-package gleam-ts-mode
  :mode "\\.gleam\\'"
  :hook
  (gleam-ts-mode . lsp-deferred)
  (gleam-ts-mode . apheleia-mode)
  :config
  (unless (treesit-language-available-p 'gleam)
    (gleam-ts-install-grammar)))


(provide 'lang-gleam)
;;; lang-gleam.el ends here
