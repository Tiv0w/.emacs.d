;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))

(use-package doom-themes
  :defer t
  :init
  (load-theme 'doom-Iosvkem t))

;; Font setup

;;(set-face-attribute 'default nil :font "DejaVu Sans Mono") ;; default
(set-face-attribute 'default nil :font "Fira Mono" :height 100)
;;(set-face-attribute 'default nil :font "Iosevka Term SS05" :height 110)

(provide 'base-theme)
