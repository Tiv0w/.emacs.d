;;; elisp/lang/lang-crystal.el -*- lexical-binding: t; -*-


(use-package crystal-mode)

(use-package flycheck
  :hook (crystal-mode . flycheck-mode)
  :config
  (use-package flycheck-crystal))


(use-package emacs-cracker
  :disabled ; until cracker is fixed for Crystal 1.0.0
  )

(use-package ameba
  :hook (crystal-mode . ameba-mode))

(use-package flycheck-ameba
  :hook (crystal-mode . flycheck-ameba-setup)
  :after (:all flycheck ameba))

;; (use-package eglot
;;   :hook (crystal-mode . eglot-ensure)
;;   :config
;;   (setq eglot-stay-out-of '())
;;   (add-to-list 'eglot-server-programs
;;                '(crystal-mode . ("crystalline"))))


;; (use-package lsp-mode
;;   :hook (crystal-mode . lsp)
;;   ;; :config
;;   ;; (defgroup lsp-crystalline nil
;;   ;;   "LSP support for Crystal via crystalline."
;;   ;;   :group 'lsp-mode
;;   ;;   :link '(url-link "https://github.com/elbywan/crystalline"))

;;   ;; (defcustom lsp-clients-crystalline-executable '("crystalline" "--stdio")
;;   ;;   "Command to start the crystalline language server."
;;   ;;   :group 'lsp-crystalline
;;   ;;   :risky t
;;   ;;   :type 'file)

;;   ;; (lsp-register-client
;;   ;;  (make-lsp-client :new-connection (lsp-stdio-connection lsp-clients-crystalline-executable)
;;   ;;                   :major-modes '(crystal-mode)
;;   ;;                   :server-id 'crystalline))

;;   ;; (lsp-consistency-check lsp-crystal)
;; )

(provide 'lang-crystal)
