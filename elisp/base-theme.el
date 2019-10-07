;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))

(use-package doom-themes
  :defer nil
  :demand t
  :init
  (load-theme 'doom-vibrant t))


(use-package ewal-doom-themes
  :load-path "/home/bobmc/.emacs.d/elisp/ewal/"
  :defer nil
  :demand t
  :config
  (progn
    (load-file "/home/bobmc/.emacs.d/elisp/ewal/doom-themes/ewal-doom-themes.el")
    (load-file "/home/bobmc/.emacs.d/elisp/ewal/doom-themes/ewal-doom-one-theme.el")
    (load-file "/home/bobmc/.emacs.d/elisp/ewal/doom-themes/ewal-doom-vibrant-theme.el")
    (load-theme 'ewal-doom-vibrant t)
    (enable-theme 'ewal-doom-vibrant)))

;; (progn
;;   (load-file "/home/bobmc/.emacs.d/elisp/ewal/doom-themes/ewal-doom-themes.el")
;;   (load-file "/home/bobmc/.emacs.d/elisp/ewal/doom-themes/ewal-doom-one-theme.el")
;;   (load-file "/home/bobmc/.emacs.d/elisp/ewal/doom-themes/ewal-doom-vibrant-theme.el")
;;   (load-theme 'ewal-doom-vibrant t)
;;   (enable-theme 'ewal-doom-vibrant))


;; (use-package ewal
;;   :init (setq ewal-use-built-in-always-p nil
;;               ewal-use-built-in-on-failure-p t
;;               ewal-built-in-palette "sexy-material"))

;; (use-package ewal-spacemacs-themes
;;   :init (progn
;;           (setq spacemacs-theme-underline-parens t
;;                 my:rice:font (font-spec
;;                               :family "Office Code Pro D"
;;                               :weight 'semi-bold
;;                               :size 11.0))
;;                               ;; ))
;;           (show-paren-mode +1)
;;           (global-hl-line-mode)
;;           (set-frame-font my:rice:font nil t)
;;           (add-to-list  'default-frame-alist
;;                         `(font . ,(font-xlfd-name my:rice:font))))
;;   :config (progn
;;             (load-theme 'ewal-spacemacs-modern t)
;;             (enable-theme 'ewal-spacemacs-modern)))

(provide 'base-theme)
