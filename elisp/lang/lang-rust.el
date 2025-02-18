;;; elisp/lang/lang-rust.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Support for the Rust language
;; Linting, project config, live documentation and jump to definition

;;; Code:

(use-package rust-mode
  :defer t
  :config
  (setq indent-tabs-mode nil
        rust-format-on-save t)
  (add-hook 'rust-mode-hook 'flycheck-mode))

(use-package rustic
  :mode ("\\.rs\\'" . rust-mode)
  :mode ("\\.rs\\'" . rustic-mode)
  :hook
  (rustic-mode . apheleia-mode)
  :config
  (setq rustic-method-chain t
        rustic-babel-format-src-block nil
        rustic-format-trigger nil)

  (setq rustic-lsp-client 'lsp-mode)

  :mode-hydra
  ((:quit-key "q")
   ("Rust"
    (("w" rust-run-clippy "clippy")
     ("e" rust-compile "compile")
     ("r" rust-run "run")
     ("t" rust-test "test")
     ("p" rust-promote-module-into-dir "promote module"))
    "Cargo"
    (("u" cargo-process-doc-open "cargo doc open"))
    "Racer"
    (("d" racer-describe "doc")
     ("s" racer-find-definition "find def")))))

(use-package tree-sitter
  :ensure t
  :hook (rustic-mode . tree-sitter-hl-mode))

(use-package flycheck
  :hook (rustic-mode . flycheck-mode))

;; (use-package flycheck-rust
;;   :after (flycheck-mode rust-mode)
;;   :hook (flycheck-mode . flycheck-rust-setup))

;; cargo-mode for all the cargo related operations
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode))

;; racer-mode for getting IDE like features for rust-mode
;; (use-package racer
;;   :hook (rust-mode . racer-mode)
;;   :config
;;   (let ((rustup-home (if (getenv "RUSTUP_HOME")
;;                         (getenv "RUSTUP_HOME")
;;                       "$HOME/.rustup")))
;;     (setq racer-rust-src-path
;;           (concat
;;            rustup-home
;;            "/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library"))) ;; needed
;;   (defun my-racer-mode-hook ()
;;     (set (make-local-variable 'company-backends)
;;          '((company-capf company-files))))

;;   (defun t--racer--describe-at-point (name)
;;   "Get a descriptions of the symbols matching symbol at point and
;; NAME.  If there are multiple possibilities with this NAME, prompt
;; the user to choose.  Return a list of all possibilities that
;; start with the user's selection."
;;   (let* ((output-lines (save-excursion
;;                          ;; Move to the end of the current symbol, to
;;                          ;; increase racer accuracy.
;;                          (skip-syntax-forward "w_")
;;                          (racer--call-at-point "complete-with-snippet")))
;;          (all-matches (--map (when (s-starts-with-p "MATCH " it)
;;                                (racer--split-snippet-match it))
;;                              output-lines))
;;          (relevant-matches (--filter (equal (plist-get it :name) name)
;;                                      all-matches)))
;;     (racer--order-descriptions
;;      (if (> (length relevant-matches) 1)
;;          ;; We might have multiple matches with the same name but
;;          ;; different types. E.g. Vec::from.
;;          (first relevant-matches)
;;        relevant-matches))))
;;   (setq-local eldoc-documentation-function #'t--racer--describe-at-point)

;;   (add-hook 'racer-mode-hook 'company-mode)
;;   (add-hook 'racer-mode-hook 'eldoc-mode))

(provide 'lang-rust)
