
;; xah-fly-keys setup, and using preconfigured xah-fly
(require 'xah-fly-keys-custom)

;; required setup
(xah-fly-keys-set-layout "qwerty")

;; custom keybinds
(define-key xah-fly-leader-key-map (kbd "f") 'counsel-switch-buffer)
(define-key xah-fly-key-map (kbd "8") 'er/expand-region)

(define-key xah-fly-dot-keymap (kbd "e") 'er/expand-region)
(define-key xah-fly-dot-keymap (kbd "j") 'mc/mark-all-like-this-dwim)
(define-key xah-fly-dot-keymap (kbd "u") 'mc/mark-next-like-this)
(define-key xah-fly-dot-keymap (kbd "i") 'mc/mark-previous-like-this)
(define-key xah-fly-dot-keymap (kbd "k") 'mc/edit-lines)
(define-key xah-fly-dot-keymap (kbd "w") 'smerge-next)
(define-key xah-fly-dot-keymap (kbd "q") 'smerge-prev)
(define-key xah-fly-dot-keymap (kbd "a") 'smerge-keep-current)

;; send C-g when pressing ESC
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(xah-fly-keys 1)

(provide 'xah-fly-keys-setup)
