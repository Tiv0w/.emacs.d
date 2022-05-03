;;; elisp/lang/lang-latex.el -*- lexical-binding: t; -*-

;; AUCTeX configuration
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-auto-save t
      TeX-parse-self t
      TeX-close-quote ""
      TeX-open-quote ""
      TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  ;; (when (executable-find "zathura")
  ;;        (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode))


;; (add-hook 'LaTeX-mode-hook (lambda () (setq ispell-parser 'tex)))


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

;; TODO: setup reftex and tout le blabla
(use-package company-reftex
  :hook (LaTeX-mode . reftex-mode)
  :config
  (add-to-list 'company-backends 'company-reftex-citations))

;; (use-package latex-extra
;;   :config)

;; (use-package flycheck
;;   :hook (LaTeX-mode . flycheck-mode))

;; (use-package flycheck-grammalecte
;;   :init
;;   (setq flycheck-grammalecte-enabled-modes
;;         '(org-mode text-mode mail-mode latex-mode TeX-latex-mode markdown-mode))
;;   :config
;;   (setq flycheck-grammalecte-report-apos nil))

(provide 'lang-latex)
