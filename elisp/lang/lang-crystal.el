;;; elisp/lang/lang-crystal.el -*- lexical-binding: t; -*-


(use-package crystal-mode)

(use-package flycheck
  :hook (crystal-mode . flycheck-mode)
  :config
  (use-package flycheck-crystal))


(use-package emacs-cracker
  :disabled ; until cracker is fixed for Crystal 1.0.0
  )


(provide 'lang-crystal)
