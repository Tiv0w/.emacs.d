;;; elisp/modules/t--ivy.el -*- lexical-binding: t; -*-
;;; Commentary:
; Ivy/Counsel/Swiper setup.

;;; Code:

(use-package counsel
  :commands (counsel-M-x find-file counsel-find-file)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-ag)
  :config
  (counsel-projectile-on))

(use-package ivy
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil
	ivy-re-builders-alist '((read-file-name-internal . ivy--regex-ignore-order)
				(projectile-completing-read . ivy--regex-ignore-order)
				(counsel-M-x . ivy--regex-ignore-order)
				(t . ivy--regex-plus)))
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(provide 't--ivy)
