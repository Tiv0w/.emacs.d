;;; elisp/modules/t--vertico.el -*- lexical-binding: t; -*-
;;; Commentary:
; Vertico/Consult/Marginalia setup.

;;; Code:


(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 17
        vertico-cycle t)
  (vertico-mouse-mode 1))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode)
  (setq marginalia--project-root #'projectile-project-root)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  )


;; (use-package counsel
;;   :commands (counsel-M-x find-file counsel-find-file)
;;   :bind
;;   ("M-x" . counsel-M-x)
;;   ("C-x C-m" . counsel-M-x)
;;   ("C-x C-f" . counsel-find-file)
;;   ("C-x c k" . counsel-yank-pop))

;; (use-package counsel-projectile
;;   :bind
;;   ("C-x v" . counsel-projectile)
;;   ("C-x c p" . counsel-projectile-ag)
;;   :config
;;   (counsel-projectile-on))

;; (use-package ivy
;;   :bind
;;   ("C-x s" . swiper)
;;   ("C-x C-r" . ivy-resume)
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers nil
;;         ivy-re-builders-alist '((read-file-name-internal . ivy--regex-ignore-order)
;;                                 (projectile-completing-read . ivy--regex-ignore-order)
;;                                 (counsel-M-x . ivy--regex-ignore-order)
;;                                 (insert-char . ivy--regex-ignore-order)
;;                                 (t . ivy--regex-plus)))
;;   (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;; (use-package nerd-icons-ivy-rich
;;   :init (nerd-icons-ivy-rich-mode 1))

;; (use-package ivy-rich
;;   :after ivy
;;   :config
;;   (ivy-rich-mode 1))

(provide 't--vertico)
