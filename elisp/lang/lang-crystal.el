;;; elisp/lang/lang-crystal.el -*- lexical-binding: t; -*-


(use-package crystal-mode
  :defer t
  :hook (crystal-mode . lsp-deferred))

(use-package flycheck
  :hook (crystal-mode . flycheck-mode))

(use-package flycheck-crystal
  :after (flycheck crystal-mode))

(use-package emacs-cracker
  :disabled ; until cracker is fixed for Crystal 1.0.0
  )

(use-package ameba
  :hook (crystal-mode . ameba-mode))

(use-package flycheck-ameba
  :hook (crystal-mode . flycheck-ameba-setup)
  :after (flycheck ameba))

(provide 'lang-crystal)
