;;; elisp/lang/lang-shell.el -*- lexical-binding: t; -*-
;;; Shell scripts setup (sh, bash, zsh)


(use-package sh-script
  :ensure nil ; builtin
  :mode ("/bspwmrc\\'" . sh-mode))

(use-package company-shell
  :after (company sh-script)
  :config
  (add-to-list 'company-backends 'company-shell)
  (setq company-shell-delete-duplicates t))

(use-package flycheck
  :hook (sh-mode . flycheck-mode))


(provide 'lang-shell)
