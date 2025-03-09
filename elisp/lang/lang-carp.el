;;; lang-carp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
; These packages provide an environment to code in Carp.

;;; Code:

(use-package carp-mode
  :defer t
  :vc (:url "https://github.com/carp-lang/carp-emacs")
  :mode "\\.carp\\'")

(use-package flycheck
  :hook (carp-mode . flycheck-mode))

(use-package carp-flycheck
  :after (flycheck carp-mode)
  :vc (:url "https://github.com/carp-lang/carp-emacs"))


(provide 'lang-carp)
;;; lang-carp.el ends here
