;;; elisp/lang/lang-r.el -*- lexical-binding: t; -*-

;;; An ESS setup for R
;;; Uses lintr and R's languageserver

(use-package ess)

(use-package flycheck
  :hook (ess-r-mode . flycheck-mode))

(use-package
  :hook (ess-r-mode . lsp))

(provide 'lang-r)
