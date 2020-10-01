;;; elisp/xah-fly-keys-setup.el -*- lexical-binding: t; -*-

;;; My own xah-fly-keys setup
;;; Also using a custom xah-fly-keys package
(require 'xah-fly-keys)

;; required setup
(xah-fly-keys-set-layout 'qwerty)


;;; Custom keybinds

;; main key map
;; works really well since the xah-fly-keys engine rewrite
(define-key xah-fly-command-map (kbd "+") 'text-scale-increase)
(define-key xah-fly-command-map (kbd "=") 'text-scale-increase)
(define-key xah-fly-command-map (kbd "-") 'text-scale-decrease)
(define-key xah-fly-command-map (kbd "r") 'iy-go-to-char)
(define-key xah-fly-command-map (kbd "R") 'iy-go-to-char-backward)


;; leader key map
(define-key xah-fly-leader-key-map (kbd "m") 'major-mode-hydra) ;; too much power
(define-key xah-fly-leader-key-map (kbd "7") 'magit-status)
(define-key xah-fly-leader-key-map (kbd "f") 'counsel-switch-buffer)
(define-key xah-fly-leader-key-map (kbd "p") 'projectile-command-map)
(define-key xah-fly-leader-key-map (kbd "q") 'exchange-point-and-mark)
(define-key xah-fly-leader-key-map (kbd "s") 'multiple-cursors-hydra/body)
(define-key xah-fly-leader-key-map (kbd "u") 'delete-region)
(define-key xah-fly-leader-key-map (kbd "y") 'swiper-thing-at-point)
(define-key xah-fly-leader-key-map (kbd "z") 'avy-goto-char)


;; hydra key map
(define-prefix-command 'xah-fly-hydra-keymap)
(define-key xah-fly-leader-key-map (kbd "/") xah-fly-hydra-keymap)
(define-key xah-fly-hydra-keymap (kbd "b") 'buffer-move-hydra/body)
(define-key xah-fly-hydra-keymap (kbd "SPC") 'multiple-cursors-hydra/body)
(define-key xah-fly-hydra-keymap (kbd "s") 'counsel-spotify-hydra/body)
(define-key xah-fly-hydra-keymap (kbd "t") 'transpose-hydra/body)
(define-key xah-fly-hydra-keymap (kbd "f") 'smerge-hydra/body)

;; apps key map
(define-prefix-command 'xah-fly-apps-keymap)
(define-key xah-fly-leader-key-map (kbd "g") xah-fly-apps-keymap)
(define-key xah-fly-apps-keymap (kbd "g") #'hydra-cljr-help-menu/body)


;; dot key map
;; (define-key xah-fly-dot-keymap (kbd "a") 'flyspell-correct-at-point)
(define-key xah-fly-dot-keymap (kbd "SPC") 'counsel-rg-thing-at-point)
(define-key xah-fly-dot-keymap (kbd "RET") 'counsel-rg)
(define-key xah-fly-dot-keymap (kbd "a") 'browse-url-dwim-guess)
(define-key xah-fly-dot-keymap (kbd "b") 'revert-buffer)
(define-key xah-fly-dot-keymap (kbd "d") 'deadgrep)
(define-key xah-fly-dot-keymap (kbd "e") 'er/expand-region)
(define-key xah-fly-dot-keymap (kbd "f") 'emmet-expand-line)
(define-key xah-fly-dot-keymap (kbd "g") 'parrot-rotate-next-word-at-point)
(define-key xah-fly-dot-keymap (kbd "h") 'parrot-rotate-prev-word-at-point)
(define-key xah-fly-dot-keymap (kbd "l") 'mc/mark-all-like-this-dwim)
(define-key xah-fly-dot-keymap (kbd "n") 'web-mode-navigate)
(define-key xah-fly-dot-keymap (kbd "p") 'projectile-command-map)
(define-key xah-fly-dot-keymap (kbd "s") 'browse-url-dwim-search)
(define-key xah-fly-dot-keymap (kbd "t") 'untabify)
(define-key xah-fly-dot-keymap (kbd "y") 'undo-tree-redo)
;;
;; keybinds for git commit
(defun setup-commit-keybinds ()
  (define-key xah-fly-dot-keymap (kbd ";") 'with-editor-finish)
  (define-key xah-fly-dot-keymap (kbd "k") 'with-editor-cancel))
(defun unsetup-commit-keybinds ()
  (define-key xah-fly-dot-keymap (kbd ";") nil)
  (define-key xah-fly-dot-keymap (kbd "k") nil))

(add-hook 'git-commit-setup-hook 'setup-commit-keybinds)
(add-hook 'with-editor-post-cancel-hook 'unsetup-commit-keybinds)
(add-hook 'with-editor-post-finish-hook 'unsetup-commit-keybinds)


;; w key map
(define-key xah-fly-w-keymap (kbd "SPC") 'eval-and-replace)
(define-key xah-fly-w-keymap (kbd "RET") 'eval-print-last-sexp)
(define-key xah-fly-w-keymap (kbd "x") 'server-shutdown)


;; h key map
(define-key xah-fly-h-keymap (kbd "j") 'helpful-callable)
(define-key xah-fly-h-keymap (kbd "k") 'helpful-at-point)
(define-key xah-fly-h-keymap (kbd "l") 'helpful-variable)
(define-key xah-fly-h-keymap (kbd "v") 'helpful-key)

;; e key map
(define-key xah-fly-e-keymap (kbd "'") 't--insert-right-single-quotation-mark)

;; c key map
(define-key xah-fly-c-keymap (kbd "k") 'crux-rename-file-and-buffer)
(define-key xah-fly-c-keymap (kbd "u") 'crux-delete-file-and-buffer)


;;; NOT REALLY USEFUL SINCE ERGODOX, but still useful for laptop
;; send C-g when pressing ESC
(when (getenv "IS_LAPTOP")
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))


(xah-fly-keys 1)


(provide 't--xah-fly-keys-setup)
