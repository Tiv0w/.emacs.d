;;; elisp/modules/t--vertico.el -*- lexical-binding: t; -*-
;;; Commentary:
; Vertico/Consult/Marginalia setup.

;;; Code:


(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 17
        vertico-cycle t)
  (vertico-mouse-mode 1)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package vertico-directory
  :after vertico
  :ensure vertico
  :bind (:map vertico-map
              ("/"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names, e.g. cleans ~/foo/bar/~/ to ~/
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))


(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap isearch-forward] . 'consult-line)
         ([remap goto-line] . 'consult-goto-line)
         ;; ([remap imenu] . 'consult-imenu)
         )
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-function #'projectile-project-root
        consult-line-start-from-top nil)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (defun consult-ripgrep-thing-at-point ()
    "`consult-ripgrep' with `thing-at-point'."
    (interactive)
    (let ((thing (thing-at-point-boosted)))
      (when (use-region-p)
        (deactivate-mark))
      (consult-ripgrep nil (regexp-quote thing))))

  (defun consult-line-thing-at-point ()
    "`consult-line' with `thing-at-point'."
    (interactive)
    (let ((thing (thing-at-point-boosted)))
      (when (use-region-p)
        (deactivate-mark))
      (consult-line (regexp-quote thing)))))

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir))
  :config
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs))


(use-package marginalia
  :after vertico
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note: this forces loading the package.
  (marginalia-mode)
  :config
  (add-to-list 'marginalia-command-categories '(projectile-switch-to-buffer . buffer)))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (setq marginalia--project-root #'projectile-project-root)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode)
  :config (setq wgrep-auto-save-buffer t))

(provide 't--vertico)
