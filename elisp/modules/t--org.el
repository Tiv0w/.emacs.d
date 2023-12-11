;;; elisp/modules/t--org.el -*- lexical-binding: t; -*-
;;; Commentary:
; Org-mode setup.

;;; Code:

(use-package org
  :pin gnu
  :init
  (setq org-directory "~/org-files"
	org-modules '(ol-bibtex))
  :config
  (setq
   org-adapt-indentation            nil
   org-confirm-babel-evaluate       nil
   org-default-notes-file           (concat org-directory "/todo.org")
   org-edit-src-content-indentation 0
   org-format-latex-options         (plist-put org-format-latex-options :scale 1.6)
   org-highlight-latex-and-related  '(native)
   org-latex-compiler               "lualatex"
   org-latex-listings               'minted
   org-latex-pdf-process
   '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
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
  (defun t--org-inhibit-mode-hooks-advice (orig-fn datum name &optional initialize &rest args)
    "Prevent potentially expensive mode hooks in `org-babel-do-in-edit-buffer' ops."
    (apply orig-fn datum name
           (if (and (eq org-src-window-setup 'switch-invisibly)
                    (functionp initialize))
               ;; org-babel-do-in-edit-buffer is used to execute quick, one-off
               ;; logic in the context of another major mode. Initializing this
               ;; major mode can be terribly expensive (particular its mode
               ;; hooks), so we inhibit them.
               (lambda ()
                 (delay-mode-hooks (funcall initialize)))
             initialize)
           args))
  (advice-add 'org-src--edit-element :around #'t--org-inhibit-mode-hooks-advice)
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
     ("b" org-beamer-export-to-pdf "Beamer PDF")
     ("s" org-latex-export-to-pdf "-> PDF")))))

(use-package lsp-ltex
  :hook (org-mode . (lambda ()
		      (require 'lsp-ltex)
		      (lsp-deferred)))
  :init
  (setq lsp-ltex-version "16.0.0"))

(use-package org-projectile
  :bind
  (("C-c n p" . org-projectile-project-todo-completing-read)
   ("C-c c" . org-capture))
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  ;; (setq org-superstar-headline-bullets-list '("ᤃ" "ᤖ" "ᤀ" "ᤂ" "ᤑ")
  (setq org-superstar-headline-bullets-list '("◉" "◈" "⁜" "⚜" "⬡")
        org-superstar-item-bullet-alist '((?* . ?•)
                                          (?+ . ?➤)
                                          (?- . ?➾))))

(use-package ox-md
  :ensure nil
  :after org
  :config
  (add-to-list 'org-export-backends 'md))

(use-package ox-beamer
  :ensure nil
  :after org
  :config
  (add-to-list 'org-export-backends 'beamer))

(require 't--org-babel)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;; (awk . t)
   ;; (clojure . t)
   (dot . t)
   ;; (gnuplot . t)
   ;; (js . t)
   (latex . t)
   (plantuml . t)
   (python . t)
   ;; (restclient . t)
   (shell . t)
   ;; (typescript . t)
   ))

(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package aas
  :disabled
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'org-mode
                    "AA"      "∀"
                    ";->"     "→"
                    "Ad"    "↓"
                    "Aup"   "↑"
                    "Aud"   "⭥"
                    "EE"      "∃"
                    ";&&"     "∧"
                    ";||"     "∨"
                    ";!"      "¬"
                    ";="      "⊢"
                    ;; ";square" "□"
                    ";top"    "⊤"
                    ";bot"    "⊥"
                    ";d"      "δ"
                    ";D"      "Δ"
                    ";aa"      "α"
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

(use-package gnuplot
  :ensure-system-package gnuplot)

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
