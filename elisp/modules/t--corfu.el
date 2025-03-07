;;; t--corfu.el --- Corfu completion setup -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages setup up in buffer completion with corfu.

;;; Code:

(use-package corfu
  ;; :hook ((prog-mode comint-mode) . company-mode)
  :init
  (global-corfu-mode)
  :config
  (setopt corfu-auto t
          corfu-auto-delay 0.24
          corfu-auto-prefix 2
          corfu-cycle t
          corfu-preselect 'valid
          corfu-count 10
          corfu-preview-current nil
          corfu-on-exact-match nil
          corfu-quit-no-match t
          corfu-quit-at-boundary 'separator
          corfu-separator ?-)
  (eldoc-add-command #'corfu-insert)

  (add-hook
   'minibuffer-setup-hook
   (defun corfu-enable-in-minibuffer ()
     "Enable Corfu in the minibuffer if `completion-at-point' is bound."
     (when (where-is-internal #'completion-at-point (list (current-local-map)))
       ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
       (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                   corfu-popupinfo-delay nil)
       (corfu-mode 1))))

  (add-hook
   'corfu-mode-hook
   (defun corfu-setup-completion-styles ()
     "Set up specific orderless style for corfu (different than minibuffer)."
     (setq-local completion-styles '(orderless-corfu-prefixes basic)
                 completion-category-overrides nil
                 completion-category-defaults nil
                 orderless-component-separator "-"
                 corfu-separator ?-))))


(use-package corfu-popupinfo
  :ensure corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (("C-c d" . corfu-popupinfo-map)
         ("C-c f" . corfu-popupinfo-toggle))
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.5)
        corfu-popupinfo-max-height 15
        corfu-popupinfo-hide nil))

(use-package corfu-echo
  :ensure corfu
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-terminal
  :when (not (display-graphic-p))
  :hook (corfu-mode . corfu-terminal-mode))

(use-package nerd-icons-corfu
  :after corfu
  :defer t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package cape
  :bind ("C-c c" . cape-prefix-map)
  :init
  (add-hook 'prog-mode-hook
            (defun corfu-add-cape-file-h ()
              (add-hook 'completion-at-point-functions #'cape-file -10 t)))
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))


(provide 't--corfu)
;;; t--corfu.el ends here
