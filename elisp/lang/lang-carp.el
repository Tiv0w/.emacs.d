;;; elisp/lang/lang-carp.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages provide an environment to code in Carp.

;;; Code:


(use-package carp-mode
  :defer t
  :load-path (lambda () (concat user-emacs-directory "elisp/extlisp/carp-emacs/"))
  :mode "\\.carp\\'")

(use-package flycheck
  :hook (carp-mode . flycheck-mode))

(use-package carp-flycheck
  :after (flycheck carp-mode)
  :load-path (lambda () (concat user-emacs-directory "elisp/extlisp/carp-emacs/")))


(provide 'lang-carp)
