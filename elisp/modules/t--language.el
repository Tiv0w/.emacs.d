;;; elisp/modules/t--language.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages are related to human languages.

;;; Code:


(use-package academic-phrases
  :commands (academic-phrases academic-phrases-by-section))

(use-package accent
  :commands accent-menu
  :config
  (setq accent-position 'after))

(use-package company-ispell
  :ensure company
  :config
  (add-to-list 'company-backends 'company-ispell t))

(use-package flycheck-grammalecte
  :after flycheck
  :init
  (setq flycheck-grammalecte-enabled-modes
        '(org-mode text-mode mail-mode latex-mode markdown-mode))
  :config
  (setq flycheck-grammalecte-report-apos nil)
  (setq flycheck-grammalecte-report-esp nil)
  (setq flycheck-grammalecte-report-nbsp nil)
  (flycheck-grammalecte-setup))

(use-package guess-language
  :hook (text-mode . guess-language-mode)
  :config
  (setq guess-language-languages '(en fr)
        guess-language-min-paragraph-length 35))


(provide 't--language)
