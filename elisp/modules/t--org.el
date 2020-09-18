;;; elisp/modules/t--org.el -*- lexical-binding: t; -*-
;;; Commentary:
; Org-mode setup.

;;; Code:

(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org")
        org-todo-keywords '((sequence "TODO" "DOING" "TEST" "DONE"))
        org-log-done 'time)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :mode-hydra
  (org-mode
   (:title "Org" :color blue :quit-key "q")
   ("Headlines"
    (("j" org-promote-subtree "promote" :color amaranth)
     ("l" org-demote-subtree "demote" :color amaranth))
    "TODO"
    (("t" org-todo "cycle state" :color amaranth)
     ("y" org-insert-todo-subheading "insert"))
    "Clock"
    (("i" org-clock-in "in")
     ("o" org-clock-out "out")
     ("e" org-clock-modify-effort-estimate "update effort" :color amaranth)
     ("p" org-clock-update-time-maybe "compute time"))
    "Babel"
    (("f" org-babel-execute-maybe "exec-maybe")))))

(use-package org-projectile
  :bind
  (("C-c n p" . org-projectile-project-todo-completing-read)
   ("C-c c" . org-capture))
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-superstar
  :after org
  :config
  (setq org-superstar-headline-bullets-list '("ᤃ" "ᤖ" "ᤀ" "ᤂ" "ᤑ")
        org-superstar-item-bullet-alist '((?* . ?•)
                                          (?+ . ?➤)
                                          (?- . ?➾)))
  (add-hook 'org-mode-hook
            (lambda ()
              (org-superstar-mode t))))

(require 't--org-babel)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (awk . t)
   (dot . t)
   (gnuplot . t)
   (js . t)
   (restclient . t)
   (shell . t)
   (typescript . t)
   ))

(use-package org-jira
  :disabled
  :defer t
  :after org
  :hook (org-mode . org-jira-mode)
  :config
  (setq org-jira-working-dir (concat user-emacs-directory "org-jira")
        jiralib-url "how wow this data is obfuscated"))




(provide 't--org)
