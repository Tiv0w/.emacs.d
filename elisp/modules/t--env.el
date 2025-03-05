;;; elisp/modules/t--env.el -*- lexical-binding: t; -*-
;;; Commentary:
; Environment related packages.

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (when (memq window-system '(mac ns x pgtk))
    (dolist (var '("GOPATH" "PYTHONPATH" "JAVA_HOME"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize))

  (defun t--env/setup-fnm-env ()
    (let ((exec-path-from-shell-arguments '("-l" "-i")))
      (exec-path-from-shell-copy-env "PATH")))

  (add-hook 'prog-mode-hook 't--env/setup-fnm-env))

(provide 't--env)
;;; t--env.el ends here
