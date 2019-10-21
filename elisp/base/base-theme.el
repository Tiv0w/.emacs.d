;; (use-package doom-themes
;;   :defer nil
;;   :demand t
;;   :init
;;   (load-theme 'doom-vibrant t))

(use-package ewal-doom-themes
  :load-path "./elisp/extlisp/ewal/doom-themes/ewal-doom-themes.el"
  :defer nil
  :demand t
  :config
  (progn
    (load-file (concat user-emacs-directory "/elisp/extlisp/ewal/doom-themes/ewal-doom-themes.el"))
    (load-file (concat user-emacs-directory "/elisp/extlisp/ewal/doom-themes/ewal-doom-vibrant-theme.el"))
    (load-theme 'ewal-doom-vibrant t)
    (enable-theme 'ewal-doom-vibrant)))

(provide 'base-theme)
