;;; elisp/lang/lang-rust.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Support for the Rust language
;; Linting, project config, live documentation and jump to definition

;;; Code:

(use-package rust-mode
  :bind (:map rust-mode-map
              (("C-c C-t" . racer-describe)))
  :config
  (add-hook 'rust-mode-hook 'flycheck-mode)
  ;; format rust buffers on save using rustfmt
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'rust-mode)
                (rust-format-buffer)))))


(use-package flycheck-rust
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))


;; cargo-mode for all the cargo related operations
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))


;; racer-mode for getting IDE like features for rust-mode
(use-package racer
  :hook (rust-mode . racer-mode)
  :config
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH")) ;; needed
  (defun my-racer-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files))))

  (add-hook 'racer-mode-hook 'company-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode))

(provide 'lang-rust)
