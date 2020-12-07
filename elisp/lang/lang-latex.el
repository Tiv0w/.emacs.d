;;; elisp/lang/lang-latex.el -*- lexical-binding: t; -*-

;; AUCTeX configuration

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-close-quote ""
      TeX-open-quote ""
      TeX-PDF-mode t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook (lambda () (setq ispell-parser 'tex)))

(add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))
(add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
;; (when (executable-find "zathura")
;;        (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(use-package flyspell
  :hook (LaTeX-mode . flyspell-mode)
  :init
  (setq ispell-program-name "aspell"
	ispell-dictionary "francais"))

(use-package company-auctex
  :hook (LaTeX-mode . company-mode)
  :config
  (set (make-local-variable 'company-backends)
       '((company-auctex-environments company-auctex-macros company-files))))

;; (use-package flycheck
;;   :hook (LaTeX-mode . flycheck-mode))

;; (use-package flycheck-grammalecte
;;   :init
;;   (setq flycheck-grammalecte-enabled-modes
;;         '(org-mode text-mode mail-mode latex-mode TeX-latex-mode markdown-mode))
;;   :config
;;   (setq flycheck-grammalecte-report-apos nil))

(provide 'lang-latex)
