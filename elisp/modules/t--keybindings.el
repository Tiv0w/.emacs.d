;;; elisp/modules/t--keybindings.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; My own modal editing setup
;;; Also using a custom xah-fly-keys package

;;; Code:
(use-package ryo-modal
  :commands ryo-modal-mode
  :bind ("C-c SPC" . ryo-modal-mode)
  :config
  (setq ryo-modal-cursor-type 'box
        ryo-modal-cursor-color nil)

  (ryo-modal-keys
   ("a" execute-extended-command)
   ("A" execute-extended-command-for-buffer)
   ("b" xah-toggle-letter-case)
   ("c" xah-copy-line-or-region)
   ("d" delete-backward-char)
   ("e" backward-kill-word)
   ("f" ryo-modal-mode)
   ("g" avy-goto-word-or-subword-1)
   ("h" xah-beginning-of-line-or-block)
   ;; ("<escape>" ryo-modal-mode)
   ("i" previous-line)
   ("j" backward-char)
   ("k" next-line)
   ("l" forward-char)
   ("m" xah-backward-left-bracket)
   ("n" isearch-forward)
   ("o" forward-word)
   ("p" xah-insert-space-before)
   ("r" iy-go-to-char)
   ("s" open-line)
   ("t" set-mark-command)
   ("u" backward-word)
   ("v" xah-paste-or-paste-previous)
   ("w" xah-shrink-whitespaces)
   ("x" xah-cut-line-or-region)
   ("y" undo)
   ("z" xah-comment-dwim)
   (";" xah-end-of-line-or-block)

   ("SPC"
    ((";" save-buffer)
     ("7" magit-status)
     ("a" mark-whole-buffer)
     ("f" switch-to-buffer)
     ("h" beginning-of-buffer)
     ("n" end-of-buffer))
    :name "+leader"))

  ;; (ryo-modal-keys
  ;;  ;; First argument to ryo-modal-keys may be a list of keywords.
  ;;  ;; These keywords will be applied to all keybindings.
  ;;  (:norepeat t)
  ;;  ("0" "M-0")
  ;;  ("1" "M-1")
  ;;  ("2" "M-2")
  ;;  ("3" "M-3")
  ;;  ("4" "M-4")
  ;;  ("5" "M-5")
  ;;  ("6" "M-6")
  ;;  ("7" "M-7")
  ;;  ("8" "M-8")
  ;;  ("9" "M-9"))
  )

;;; t--keybindings.el ends here
