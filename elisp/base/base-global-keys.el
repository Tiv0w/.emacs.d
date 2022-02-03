;;; elisp/base/base-global-keys.el -*- lexical-binding: t; -*-
;; Add your keys here, as such

;; (global-set-key (kbd "[SHORTCUT]") '[FUNCTION])

;; https://github.com/technomancy/better-defaults inspired
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key [f7] 'treemacs)

(provide 'base-global-keys)
