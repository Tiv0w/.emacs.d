;;; elisp/lang/lang-graphviz.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Graphviz setup.

;;; Code:


(use-package graphviz-dot-mode
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  :disabled
  :ensure graphviz-dot-mode
  :after graphviz-dot-mode)

(use-package gvpr-mode
  :disabled
  :commands gvpr-mode)

(provide 'lang-graphviz)
