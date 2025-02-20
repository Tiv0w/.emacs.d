;;; elisp/modules/t--corfu.el -*- lexical-binding: t; -*-
;;; Commentary:
;; These packages setup up completion with corfu.


(use-package corfu
  ;; :hook ((prog-mode comint-mode) . company-mode)
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.24
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'valid
        corfu-count 10
        corfu-preview-current nil
        corfu-on-exact-match nil
        corfu-quit-no-match t
        corfu-quit-at-boundary t)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package corfu-popupinfo
  :ensure corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (("C-c d" . corfu-popupinfo-map)
         ("C-c f" . corfu-popupinfo-toggle))
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))

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

;; (use-package company-box
;;   :after company
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-icons-alist 'company-box-icons-images))


(provide 't--corfu)
