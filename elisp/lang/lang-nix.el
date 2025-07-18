;;; lang-nix.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; My setup for Nix

;;; Code:

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook
  (nix-ts-mode . lsp-deferred)
  ;; (nix-ts-mode . apheleia-mode)
)


(provide 'lang-nix)
;;; lang-nix.el ends here
