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
   ("TODO"
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
  (setq org-superstar-headline-bullets-list '("ᤀ" "ᤂ" "ᤃ" "ᤑ" "ᤖ")
	org-superstar-item-bullet-alist '((?* . ?•)
					  (?+ . ?➤)
					  (?- . ?➾)))
  (add-hook 'org-mode-hook
            (lambda ()
              (org-superstar-mode t))))

(require 't--org-babel)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
   (restclient . t)
   (js . t)))


(provide 't--org)
