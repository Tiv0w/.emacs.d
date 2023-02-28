;;; elisp/modules/t--ansible.el -*- lexical-binding: t; -*-
;;; Commentary:
; Configuration for Ansible.

;;; Code:

(use-package ansible
  :hook (yaml-mode . ansible)
  :if (executable-find "ansible")
  :config
  (setq ansible-section-face 'font-lock-doc-face
	ansible-task-label-face 'font-lock-doc-face)
  (set (make-local-variable 'company-backends)
       '((company-ansible company-capf company-files))))

(use-package ansible-doc
  :after ansible
  :commands ansible-doc
  :mode-hydra
  (yaml-mode
   (:title "Yaml" :color blue :quit-key "q")
   ("Ansible"
    (("d" ansible-doc "doc at point")
     ("w" ansible-decrypt-buffer "decrypt buffer")
     ("e" ansible-encrypt-buffer "encrypt buffer")))))

(use-package company-ansible
  :after (ansible company))

(use-package jinja2-mode
  :mode "\\.j2\\'"
  :config
  (setq jinja2-enable-indent-on-save nil))

(provide 't--ansible)
