;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-close-quote "")
(setq TeX-open-quote "")

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)


(use-package flycheck
  :hook (LaTeX-mode . flycheck-mode))

(use-package flyspell
  :hook (LaTeX-mode . flyspell-mode)
  :init
  (setq ispell-program-name "hunspell")
  :config
  (ispell-change-dictionary "francais"))


(use-package flycheck-grammalecte
  :init
  (setq flycheck-grammalecte-enabled-modes
        '(org-mode text-mode mail-mode latex-mode TeX-latex-mode markdown-mode))
  :config
  (setq flycheck-grammalecte-report-apos nil))

(use-package darkroom
  :hook (LaTeX-mode . darkroom-mode)
  :config
  (add-hook 'darkroom-mode-hook 'visual-line-mode))

(provide 'lang-latex)
