(TeX-latex-mode)

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-close-quote "")
(setq TeX-open-quote "")

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

(defun prelude-latex-mode-defaults ()
  "Default Prelude hook for `LaTeX-mode'."
  (turn-on-auto-fill)
  (abbrev-mode +1)
  (smartparens-mode +1)
  (case prelude-latex-fast-math-entry
    (LaTeX-math-mode (LaTeX-math-mode 1))
    (cdlatex (turn-on-cdlatex)))


  (use-package flycheck
    :hook (TeX-latex-mode . flycheck-mode))

  (use-package flyspell
    :config
    (setq ispell-program-name "hunspell")
    (ispell-change-dictionary "francais"))


  (use-package flycheck-grammalecte
    :init
    (setq flycheck-grammalecte-enabled-modes
          '(org-mode text-mode mail-mode latex-mode TeX-latex-mode markdown-mode))
    :config
    (setq flycheck-grammalecte-report-apos nil))

  (use-package darkroom
    :hook (TeX-latex-mode . darkroom-mode)
    :config
    (add-hook 'darkroom-mode-hook #'visual-line-mode)))

(setq prelude-latex-mode-hook 'prelude-latex-mode-defaults)

(add-hook 'TeX-latex-mode-hook (lambda ()
                                 (run-hooks 'prelude-latex-mode-hook)))
(provide 'lang-latex)
