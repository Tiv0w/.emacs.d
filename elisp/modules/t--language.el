;;; elisp/modules/t--language.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages are related to human languages.

;;; Code:


(use-package guess-language
  :hook (text-mode . guess-language-mode)
  :config
  (setq guess-language-languages '(en fr)
	guess-language-min-paragraph-length 35))

(use-package company-ispell
  :ensure company
  :config
  (push 'company-ispell company-backends))


(provide 't--language)
