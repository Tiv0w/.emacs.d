
;; xah-fly-keys setup, and using preconfigured xah-fly
(require 'xah-fly-keys-custom)

;; required setup
(xah-fly-keys-set-layout "qwerty")

;; custom keybinds
(define-key xah-fly-key-map (kbd "n") 'isearch-forward-regexp)
(define-key xah-fly-key-map (kbd "a") 'counsel-M-x)

;;;;;;;;;;;;;;;;;;;;
;; too much power ;;
;;;;;;;;;;;;;;;;;;;;
(define-key xah-fly-leader-key-map (kbd "m") 'major-mode-hydra)
;;;;;;;;;;;;;;;;;;;;

(define-key xah-fly-leader-key-map (kbd "f") 'counsel-switch-buffer)
(define-key xah-fly-leader-key-map (kbd "u") 'delete-region)
(define-key xah-fly-leader-key-map (kbd "z") 'avy-goto-char)

(define-key xah-fly-hydra-keymap (kbd "j") 'multiple-cursors-hydra/body)

;; (define-key xah-fly-dot-keymap (kbd "a") 'flyspell-correct-at-point)
(define-key xah-fly-dot-keymap (kbd "b") 'revert-buffer)
(define-key xah-fly-dot-keymap (kbd "d") 'deadgrep)
(define-key xah-fly-dot-keymap (kbd "e") 'er/expand-region)
(define-key xah-fly-dot-keymap (kbd "i") 'dumb-jump-go)
(define-key xah-fly-dot-keymap (kbd "j") 'projectile-command-map)
(define-key xah-fly-dot-keymap (kbd "l") 'mc/mark-all-like-this-dwim)
(define-key xah-fly-dot-keymap (kbd "n") 'web-mode-navigate)
(define-key xah-fly-dot-keymap (kbd "o") 'dumb-jump-go-prompt)
(define-key xah-fly-dot-keymap (kbd "t") 'untabify)
(define-key xah-fly-dot-keymap (kbd "u") 'dumb-jump-back)

(define-key xah-fly-w-keymap (kbd "x") 'server-shutdown)

;; send C-g when pressing ESC
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(xah-fly-keys 1)

(provide 'xah-fly-keys-setup)
