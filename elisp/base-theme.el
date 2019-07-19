;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))

(use-package doom-themes
  :defer nil
  :demand t
  :init
  (load-theme 'doom-Iosvkem t))

(set-face-font 'default "Fira Mono")

(provide 'base-theme)
