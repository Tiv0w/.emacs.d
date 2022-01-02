;;; elisp/modules/t--org.el -*- lexical-binding: t; -*-
;;; Commentary:
; Org-mode setup.

;;; Code:

(use-package org
  :init
  (setq org-directory "~/org-files")
  :config
  (setq
   org-adapt-indentation            nil
   org-confirm-babel-evaluate       nil
   org-default-notes-file           (concat org-directory "/todo.org")
   org-edit-src-content-indentation 0
   org-format-latex-options         (plist-put org-format-latex-options :scale 1.6)
   org-highlight-latex-and-related  '(native)
   org-latex-compiler               "pdflatex"
   org-list-allow-alphabetical      t
   org-log-done                     'time
   org-plantuml-exec-mode           'plantuml
   org-src-fontify-natively         t
   org-src-tab-acts-natively        t
   org-startup-with-inline-images   t
   org-todo-keywords                '((sequence "TODO" "DOING" "TEST" "DONE")))
  ;; Highlight broken file links
  (org-link-set-parameters
   "file"
   :face (lambda (path)
           (if (or (file-remote-p path)
                   (file-exists-p path))
               'org-link
             'error)))
  (setq org-capture-templates
        '(("i"
           "Ideas"
           entry
           (file+headline "ideas.org" "List")
           "** %?\n%i")))
  (add-hook 'org-mode-hook (lambda ()
                               (modify-syntax-entry ?< "w" org-mode-syntax-table)
                               (modify-syntax-entry ?> "w" org-mode-syntax-table)))
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
     ("p" org-clock-update-time-maybe "compute time"))
    "Babel"
    (("SPC" org-babel-execute-src-block "exec"))
    "Display"
    (("w" org-toggle-inline-images "inline images")
     ("v" org-latex-preview "inline LaTeX"))
    "Export"
    (("d" org-export-dispatch "menu")
     ("s" org-latex-export-to-pdf "-> PDF")))))

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
   (clojure . t)
   (dot . t)
   (gnuplot . t)
   (js . t)
   (plantuml . t)
   (restclient . t)
   (shell . t)
   (typescript . t)
   ))

(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package aas
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'org-mode
                    "AA"      "∀"
                    ";->"     "→"
                    "EE"      "∃"
                    ";&&"     "∧"
                    ";||"     "∨"
                    ";!"      "¬"
                    ";="      "⊢"
                    ";square" "□"
                    ";top"    "⊤"
                    ";bot"    "⊥"
                    ";d"      "δ"
                    ";D"      "Δ"
                    ";a"      "α"
                    ";A"      "Α"
                    ";g"      "γ"
                    ";G"      "Γ"
                    ";s"      "σ"
                    ";S"      "Σ"))

(use-package org-jira
  :disabled
  :defer t
  :after org
  :hook (org-mode . org-jira-mode)
  :config
  (setq org-jira-working-dir (concat user-emacs-directory "org-jira")
        jiralib-url "how wow this data is obfuscated"))

(use-package cdlatex
  :commands (org-cdlatex-mode))


(set-pretty-symbols! 'org-mode
    :name          "#+name:"
    :src_block     "#+begin_src"
    :src_block_end "#+end_src"
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :language      "#+language:"
    :author        "#+author:"
    :email         "#+email:"
    :date          "#+date:"
    :options       "#+options:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :quote         "#+begin_quote"
    :quote_end     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :properties    ":PROPERTIES:"
    :end           ":END:")


(provide 't--org)
