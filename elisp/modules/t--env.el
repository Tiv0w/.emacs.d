;;; elisp/modules/t--env.el -*- lexical-binding: t; -*-
;;; Commentary:
; Environment related packages.

;;; Code:

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  (exec-path-from-shell-initialize))

(provide 't--env)
