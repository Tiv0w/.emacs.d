
;; xah-fly-keys setup, and using preconfigured xah-fly
(require 'xah-fly-keys-custom)

;; required setup
(xah-fly-keys-set-layout "qwerty")

;; custom keybinds

;; main key map
;; doesn't work well
(define-key xah-fly-key-map (kbd "n") 'isearch-forward-regexp)
(define-key xah-fly-key-map (kbd "a") 'counsel-M-x)

;; leader key map

;;;;;;;;;;;;;;;;;;;;
;; too much power ;;
(define-key xah-fly-leader-key-map (kbd "m") 'major-mode-hydra)
;;;;;;;;;;;;;;;;;;;;
(define-key xah-fly-leader-key-map (kbd "7") 'magit-status)
(define-key xah-fly-leader-key-map (kbd "f") 'counsel-switch-buffer)
(define-key xah-fly-leader-key-map (kbd "p") 'projectile-command-map)
(define-key xah-fly-leader-key-map (kbd "q") 'exchange-point-and-mark)
(define-key xah-fly-leader-key-map (kbd "s") 'multiple-cursors-hydra/body)
(define-key xah-fly-leader-key-map (kbd "u") 'delete-region)
(define-key xah-fly-leader-key-map (kbd "z") 'avy-goto-char)

;; hydra key map
(define-key xah-fly-hydra-keymap (kbd "b") 'buffer-move-hydra/body)
(define-key xah-fly-hydra-keymap (kbd "SPC") 'multiple-cursors-hydra/body)
(define-key xah-fly-hydra-keymap (kbd "s") 'counsel-spotify-hydra/body)
(define-key xah-fly-hydra-keymap (kbd "t") 'transpose-hydra/body)

;; dot key map
;; (define-key xah-fly-dot-keymap (kbd "a") 'flyspell-correct-at-point)
(define-key xah-fly-dot-keymap (kbd "SPC") 'projectile-command-map)
(define-key xah-fly-dot-keymap (kbd "b") 'revert-buffer)
(define-key xah-fly-dot-keymap (kbd "d") 'deadgrep)
(define-key xah-fly-dot-keymap (kbd "e") 'er/expand-region)
(define-key xah-fly-dot-keymap (kbd "g") 'parrot-rotate-next-word-at-point)
(define-key xah-fly-dot-keymap (kbd "h") 'parrot-rotate-prev-word-at-point)
(define-key xah-fly-dot-keymap (kbd "i") 'dumb-jump-go)
(define-key xah-fly-dot-keymap (kbd "l") 'mc/mark-all-like-this-dwim)
(define-key xah-fly-dot-keymap (kbd "n") 'web-mode-navigate)
(define-key xah-fly-dot-keymap (kbd "o") 'dumb-jump-go-prompt)
(define-key xah-fly-dot-keymap (kbd "t") 'untabify)
(define-key xah-fly-dot-keymap (kbd "u") 'dumb-jump-back)
;; keybinds for git commit
(define-key xah-fly-dot-keymap (kbd ";") 'with-editor-finish)
(define-key xah-fly-dot-keymap (kbd "k") 'with-editor-cancel)

;; w key map
(define-key xah-fly-w-keymap (kbd "SPC") 'eval-and-replace)
(define-key xah-fly-w-keymap (kbd "x") 'server-shutdown)

;; h key map
(define-key xah-fly-h-keymap (kbd "j") 'helpful-callable)
(define-key xah-fly-h-keymap (kbd "k") 'helpful-at-point)
(define-key xah-fly-h-keymap (kbd "l") 'helpful-variable)
(define-key xah-fly-h-keymap (kbd "v") 'helpful-key)

;; send C-g when pressing ESC
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(xah-fly-keys 1)

(provide 'xah-fly-keys-setup)
