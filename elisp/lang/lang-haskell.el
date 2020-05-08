;;; elisp/lang/lang-haskell.el -*- lexical-binding: t; -*-

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

(use-package dante
  :after haskell-mode
  :hook (haskell-mode . dante-mode)
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  :config
  (add-hook 'dante-mode-hook
	    '(lambda () (flycheck-add-next-checker 'haskell-dante
						   '(warning . haskell-hlint)))))

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
