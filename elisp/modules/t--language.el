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
  (push 'company-ispell company-backends))

(use-package guess-language
  :hook (text-mode . guess-language-mode)
  :config
  (setq guess-language-languages '(en fr)
        guess-language-min-paragraph-length 35))


(provide 't--language)
