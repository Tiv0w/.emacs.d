;;; elisp/xah-fly-keys-setup.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; My own xah-fly-keys setup
;;; Also using a custom xah-fly-keys package

;;; Code:

(require 'xah-fly-keys)

;; required setup
(xah-fly-keys-set-layout 'qwerty)


;;; Custom keybinds

;; main key map
;; works really well since the xah-fly-keys engine rewrite
(define-key xah-fly-command-map (kbd "+") 'text-scale-increase)
(define-key xah-fly-command-map (kbd "=") 'text-scale-increase)
(define-key xah-fly-command-map (kbd "-") 'text-scale-decrease)
(define-key xah-fly-command-map (kbd "E") 'embrace-commander)
(define-key xah-fly-command-map (kbd "g") 'avy-goto-char-timer)
(define-key xah-fly-command-map (kbd "G") 'avy-goto-word-or-subword-1)
(define-key xah-fly-command-map (kbd "r") 'iy-go-to-char)
(define-key xah-fly-command-map (kbd "R") 'iy-go-to-char-backward)
(define-key xah-fly-command-map (kbd "<") 'ace-window)
(define-key xah-fly-command-map (kbd "n") 'isearch-forward)
(define-key xah-fly-command-map (kbd "Z") 'crux-duplicate-and-comment-current-line-or-region)
(define-key xah-fly-command-map (kbd "C-SPC") 'corfu-complete)
(define-key xah-fly-command-map (kbd "A") 'execute-extended-command-for-buffer)


;; leader key map
(define-key xah-fly-leader-key-map (kbd "m") 'major-mode-hydra) ;; too much power
(define-key xah-fly-leader-key-map (kbd "'") 'flyspell-correct-wrapper)
(define-key xah-fly-leader-key-map (kbd "\"") 'flycheck-grammalecte-correct-error-at-point)
(define-key xah-fly-leader-key-map (kbd "7") 'magit-status)
(define-key xah-fly-leader-key-map (kbd "f") 'switch-to-buffer)
(define-key xah-fly-leader-key-map (kbd "p") 'projectile-command-map)
(define-key xah-fly-leader-key-map (kbd "q") 'exchange-point-and-mark)
(define-key xah-fly-leader-key-map (kbd "s") 'multiple-cursors-hydra/body)
(define-key xah-fly-leader-key-map (kbd "u") 'delete-region)
(define-key xah-fly-leader-key-map (kbd "y") 'consult-line-thing-at-point)
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
(define-key xah-fly-dot-keymap (kbd "SPC") 'consult-ripgrep-thing-at-point)
(define-key xah-fly-dot-keymap (kbd "RET") 'consult-ripgrep)
(define-key xah-fly-dot-keymap (kbd "a") 'browse-url-dwim-guess)
(define-key xah-fly-dot-keymap (kbd "b") 'revert-buffer)
(define-key xah-fly-dot-keymap (kbd "d") 'deadgrep)
(define-key xah-fly-dot-keymap (kbd "e") 'er/expand-region)
(define-key xah-fly-dot-keymap (kbd "f") 'emmet-expand-line)
(define-key xah-fly-dot-keymap (kbd "g") 'parrot-rotate-next-word-at-point)
(define-key xah-fly-dot-keymap (kbd "h") 'parrot-rotate-prev-word-at-point)
(define-key xah-fly-dot-keymap (kbd "j") 'makefile-executor-execute-project-target)
(define-key xah-fly-dot-keymap (kbd "l") 'mc/mark-all-like-this-dwim)
(define-key xah-fly-dot-keymap (kbd "n") 'web-mode-navigate)
(define-key xah-fly-dot-keymap (kbd "p") 'projectile-command-map)
(define-key xah-fly-dot-keymap (kbd "q") 'flyspell-correct-word-before-point)
(define-key xah-fly-dot-keymap (kbd "r") 'iedit-mode)
(define-key xah-fly-dot-keymap (kbd "s") 'browse-url-dwim-search)
(define-key xah-fly-dot-keymap (kbd "t") 'untabify-buffer)
(define-key xah-fly-dot-keymap (kbd "w") 'flyspell-auto-correct-word)
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
(define-key xah-fly-w-keymap (kbd "j") 'server-edit)
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

;; t key map
(define-key xah-fly-t-keymap (kbd "j") 'xah-close-current-buffer)
(define-key xah-fly-t-keymap (kbd "z") 'repeat-complex-command)

;; n key map
(define-key xah-fly-n-keymap (kbd "l") 'narrow-or-widen-dwim)
(define-key xah-fly-n-keymap (kbd "V") 'lacarte-execute-menu-command)
(define-key xah-fly-n-keymap (kbd "1") 'profiler-start)
(define-key xah-fly-n-keymap (kbd "2") 'profiler-stop)

;; comma key map
(define-key xah-fly-comma-keymap (kbd "k") 'xref-find-definitions)
(define-key xah-fly-comma-keymap (kbd "l") 'xref-go-back)
;; LSP
(define-key xah-fly-comma-keymap (kbd "o") 'lsp-organize-imports)
(define-key xah-fly-comma-keymap (kbd "a") 'lsp-execute-code-action)
(define-key xah-fly-comma-keymap (kbd "d") 'lsp-ui-doc-glance)
(define-key xah-fly-comma-keymap (kbd "D") 'lsp-ui-doc-toggle)
(define-key xah-fly-comma-keymap (kbd "r") 'lsp-rename)
(define-key xah-fly-comma-keymap (kbd "R") 'lsp-iedit-highlights)
;; (define-key xah-fly-comma-keymap (kbd "L") lsp-command-map)
;; (setq lsp-keymap-prefix " wL")

;;; NOT REALLY USEFUL SINCE ERGODOX, but still useful for laptop
;; send C-g when pressing ESC
(when (getenv "IS_LAPTOP")
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; (define-key xah-fly-command-map (kbd "5") 'keyboard-escape-quit)
;; (define-key xah-fly-insert-map (kbd "<escape>") 'keyboard-escape-quit)

(add-hook 'calc-start-hook #'xah-fly-insert-mode-activate)
(advice-add 'calcDigit-start :after #'xah-fly-insert-mode-activate)

(setq xah-fly-M-x-command 'execute-extended-command)

(xah-fly-keys 1)

(defun t--term-cursor-change ()
  "Change the cursor shape in supported terminals to match the GUI behavior."
  (unless (display-graphic-p)
    (if xah-fly-insert-state-p
        (send-string-to-terminal "\e[6 q") ; Vertical solid bar
      (send-string-to-terminal "\e[2 q")))) ; Solid block
(add-hook 'xah-fly-command-mode-activate-hook #'t--term-cursor-change)
(add-hook 'xah-fly-insert-mode-activate-hook #'t--term-cursor-change)

(if (daemonp)
    (add-hook 'server-after-make-frame-hook (lambda () (xah-fly-keys 1))))

(provide 't--xah-fly-keys-setup)
;;; t--xah-fly-keys-setup.el ends here
