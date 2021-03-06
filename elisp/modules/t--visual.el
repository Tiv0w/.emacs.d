;;; elisp/modules/t--visual.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages have a visual effect on Emacs.

;;; Code:

(use-package all-the-icons)

(use-package dashboard
  :config
  (let ((logo (concat user-emacs-directory "private/logos/logo.png")))
    (if (file-exists-p logo)
        (setq dashboard-startup-banner logo)))
  (setq dashboard-banner-logo-title
        "Vous entrez dans le monde de la peur et des poignets cassés")
  (setq dashboard-items '((recents . 7)
                          (projects . 7)
                          (bookmarks . 2)
                          (agenda . 2)
                          (registers . 2)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (dashboard-setup-startup-hook))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode markdown-mode fundamental-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width 3))

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-file-name-style 'buffer-name
        doom-modeline-icon t
        doom-modeline-percent-position nil)
  (display-battery-mode)
  :hook (after-init . doom-modeline-mode))

(use-package highlight-indent-guides
  :hook ((prog-mode conf-mode yaml-mode restclient-mode) . highlight-indent-guides-mode)
  :defer nil
  :config
  (setq highlight-indent-guides-method 'column)

  ;; to fix a problem with indent-guides not showing in daemon
  (defun t--setup-highlight-indent-guides ()
    (when (display-graphic-p)
      (highlight-indent-guides-auto-set-faces)))
  (add-hook 'server-after-make-frame-hook
            #'t--setup-highlight-indent-guides))

(use-package mixed-pitch
  :disabled
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-variable-pitch-cursor nil)
  (t--add-to-list-multiple 'mixed-pitch-fixed-pitch-faces
                           '(line-number line-number-current-line)))

(use-package mini-frame
  :config
  (setq resize-mini-frames t
        mini-frame-show-parameters '((top . 30)
                                     (width . 0.5)
                                     (left . 0.5)))
  ;; mini-frame-show-parameters '((top . -230)
  ;;                              (width . 0.7)
  ;;                              (left . 0.5)))
  ;; (mini-frame-mode)
  )

(use-package page-break-lines)

(use-package popwin
  :config
  (popwin-mode 1))

(use-package rainbow-mode
  :hook (prog-mode conf-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode org-mode) . rainbow-delimiters-mode))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.7)
  (which-key-mode))

(provide 't--visual)
