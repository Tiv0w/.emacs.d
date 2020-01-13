;; haskell-mode configuration
;; https://github.com/haskell/haskell-mode
(use-package haskell-mode
  ;; haskell-mode swaps `C-m' and `C-j' behavior. Revert it back
  :bind (:map haskell-mode-map
              ("C-m" . newline)
              ("C-j" . electric-newline-and-maybe-indent))
  :config
  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (set (make-local-variable 'company-backends)
         '((company-intero company-files))))
  (eval-after-load 'haskell-indentation
    '(setcdr haskell-indentation-mode-map nil))
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'haskell-mode-hook 'company-mode)
  ;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  )

;; intero-mode for a complete IDE solution to haskell
;; commercialhaskell.github.io/intero
(use-package intero
  :after haskell-mode
  :hook (haskell-mode . intero-mode))

;; hindent - format haskell code automatically
;; https://github.com/chrisdone/hindent
(when (executable-find "hindent")
  (use-package hindent
    :after haskell-mode
    :hook (haskell-mode . hindent-mode)
    :config
    ;; reformat the buffer using hindent on save
    (setq hindent-reformat-buffer-on-save t)))

(provide 'lang-haskell)
