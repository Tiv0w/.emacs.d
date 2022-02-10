;;; elisp/lang/lang-shell.el -*- lexical-binding: t; -*-
;;; Shell scripts setup (sh, bash, zsh)

(use-package company-shell
  :after (company sh-script)
  :config
  (add-to-list 'company-backends 'company-shell)
  (setq company-shell-delete-duplicates t))

(use-package flycheck
  :hook (sh-mode . flycheck-mode))

(set-pretty-symbols! '(sh-mode)
  :def "function"
  :true "true"
  :false "false"
  :not "!"
  :and "&&"
  :or "||"
  :in "in"
  :for "for"
  :return "return")


(provide 'lang-shell)
