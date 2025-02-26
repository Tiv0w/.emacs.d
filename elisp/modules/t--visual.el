;;; elisp/modules/t--visual.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages have a visual effect on Emacs.

;;; Code:

(use-package all-the-icons
  :disabled)

(use-package centaur-tabs
  :commands centaur-tabs-mode
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-cycle-scope 'tabs))

(use-package dashboard
  :init
  (let ((logo (concat user-emacs-directory "private/logos/logo.webp")))
    (setq dashboard-startup-banner (if (file-exists-p logo) logo 1)))
  (setq dashboard-items '((recents . 10)
                          (projects . 7))
        dashboard-startupify-list '(dashboard-insert-banner
                                    ;; dashboard-insert-banner-title
                                    ;; dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer)
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-show-shortcuts nil
        ;; Always display (term and GUI) with nerd-icons
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons)
  :config
  (dashboard-setup-startup-hook))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode conf-mode markdown-mode fundamental-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon t
        doom-modeline-bar-width 3
        doom-modeline-modal-icon nil
        doom-modeline-battery nil
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-percent-position nil))

(use-package highlight-indent-guides
  ;; :disabled
  :hook ((prog-mode conf-mode yaml-mode restclient-mode) . highlight-indent-guides-mode)
  :defer nil
  :config
  (setq highlight-indent-guides-method 'column)

  ;; to fix a problem with indent-guides not showing in daemon
  (defun t--setup-highlight-indent-guides ()
    (when (and (display-graphic-p) (frame-focus-state))
      (highlight-indent-guides-auto-set-faces)))
  (add-hook 'server-after-make-frame-hook
            #'t--setup-highlight-indent-guides)
  (add-function :after after-focus-change-function
                #'t--setup-highlight-indent-guides))

;; TODO: change for indent-bars in Emacs 30
(use-package indent-bars
  :disabled
  :hook ((prog-mode conf-mode text-mode restclient-mode) . t--enable-indent-bars)
  :init
  (setq indent-bars-prefer-character
        ;; FIX: A bitmap init bug in emacs-pgtk (before v30) could cause
        ;; crashes (see jdtsmith/indent-bars#3).
        (and (featurep 'pgtk) (< emacs-major-version 30))
        indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-width-frac 0.1
        indent-bars-pad-frac 0.1
        indent-bars-starting-column 0
        indent-bars-color-by-depth nil
        indent-bars-highlight-current-depth nil)
  (defun t--enable-indent-bars ()
    (unless (frame-parameter nil 'frame-parent)
      (indent-bars-mode +1)))

  (eval-after-load 'magit-blame
    (add-to-list 'magit-blame-disable-modes 'indent-bars-mode)))


(use-package mixed-pitch
  :disabled
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-variable-pitch-cursor nil)
  (t--add-to-list-multiple 'mixed-pitch-fixed-pitch-faces
                           '(line-number line-number-current-line)))

(use-package mini-frame
  :disabled
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

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package page-break-lines)

(use-package popwin
  :config
  (popwin-mode 1))

(use-package rainbow-mode
  :hook (prog-mode conf-mode))

(use-package rainbow-blocks
  :commands rainbow-blocks-mode)

(use-package rainbow-delimiters
  :hook ((prog-mode org-mode) . rainbow-delimiters-mode))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.7)
  (which-key-mode))

(provide 't--visual)
;;; t--visual.el ends here
