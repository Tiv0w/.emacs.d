;;; elisp/modules/t--ansible.el --- -*- lexical-binding: t; -*-
;;; Commentary:
; Configuration for Ansible.

;;; Code:

;; detect filenames compatible with Ansible's recommended layout.
;; http://docs.ansible.com/playbooks_best_practices.html#directory-layout
(defun spacemacs//ansible-should-enable? ()
  "Return non-nil if `ansible' should be enabled for the current file."
  (let ((spacemacs--ansible-filename-re
         "/\\(main\\|site\\|encrypted\\|\\(\\(roles\\|tasks\\|handlers\\|vars\\|defaults\\|meta\\|group_vars\\|host_vars\\)/.+\\)\\)\\.ya?ml$"))
    (and (stringp buffer-file-name)
         (string-match spacemacs--ansible-filename-re buffer-file-name))))

(defun spacemacs/ansible-maybe-enable ()
  "Enable `ansible-mode' if required."
  (when (spacemacs//ansible-should-enable?)
    (ansible-mode 1)))


(use-package ansible
  :hook
  ((yaml-mode yaml-ts-mode) . spacemacs/ansible-maybe-enable)
  (ansible-mode . lsp-deferred)
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
  (ansible-mode
   (:title "Yaml" :color blue :quit-key "q")
   ("Ansible"
    (("d" ansible-doc "doc at point")
     ("w" ansible-decrypt-buffer "decrypt buffer")
     ("e" ansible-encrypt-buffer "encrypt buffer")))))

(use-package company-ansible
  :disabled
  :after (ansible company))

(use-package jinja2-mode
  :mode "\\.j2\\'"
  :config
  (setq jinja2-enable-indent-on-save nil))

(provide 't--ansible)
;;; t--ansible.el ends here
