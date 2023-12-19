;;; elisp/modules/t--env.el -*- lexical-binding: t; -*-
;;; Commentary:
; Environment related packages.

;;; Code:

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (when (memq window-system '(mac ns x))
    (dolist (var '("GOPATH" "PYTHONPATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

(provide 't--env)
