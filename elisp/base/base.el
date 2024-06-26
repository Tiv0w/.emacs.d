;;; elisp/base/base.el -*- lexical-binding: t; -*-


(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("org" . "https://orgmode.org/elpa/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; remove these 3 lines when using Emacs 30+
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories")

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Open new emacsclient with dashboard
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Emacs customizations
(setq confirm-kill-emacs                  'y-or-n-p
      confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      disabled-command-function           nil
      mouse-yank-at-point                 t
      electric-pair-mode                  t
      column-number-mode                  t
      require-final-newline               t
      load-prefer-newer                   t
      visible-bell                        t
      ring-bell-function                  'ignore
      dired-listing-switches              "-alhF"
      custom-file                         (concat user-emacs-directory ".custom.el")
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      exec-path                          (append exec-path '("/usr/local/bin/"))
      indent-tabs-mode                   nil
      inhibit-startup-message            t
      fringes-outside-margins            t
      select-enable-clipboard            t
      use-package-always-ensure          t
      use-package-verbose                t)

;; Makes Tramp only check for Git, might make it a bit faster
(setq-default vc-handled-backends '(Git))

;; Bookmarks
(setq
 ;; persistent bookmarks
 bookmark-save-flag                      t
 bookmark-default-file              (concat temp-dir "/bookmarks"))

;; Backups enabled, use nil to disable
(setq
 history-length                     1000
 backup-inhibited                   nil
 make-backup-files                  t
 auto-save-default                  t
 auto-save-list-file-name           (concat temp-dir "/autosave")
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t)))

(unless (file-exists-p (concat temp-dir "/auto-save-list"))
                       (make-directory (concat temp-dir "/auto-save-list") :parents))

(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode -1)
(global-auto-revert-mode t)


(defun defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h ()
  ;; Defer it: commands launched immediately after will enjoy the benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

(show-paren-mode 1)
(electric-pair-mode 1)

;; Emacsclient setup
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))

;; Start with maximized frame
(add-to-list 'default-frame-alist '(fullscreen . maximized));;works with emacsclient too yay

(if (version< emacs-version "29")
    (add-to-list 'default-frame-alist '(alpha . 90))
  (progn
    (add-to-list 'default-frame-alist '(alpha-background . 90))
    (add-to-list 'default-frame-alist '(alpha . 100))))

;; Emacs 29 changes
(when (>= emacs-major-version 29)
  (setq pixel-scroll-precision-mode t))

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Need to load custom file to avoid being overwritten
;; more at https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(load custom-file t)


(provide 'base)
;;; base ends here
