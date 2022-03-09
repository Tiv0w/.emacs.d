;;; elisp/base/base-functions.el -*- lexical-binding: t; -*-

;; Add your custom functions here

;; (defun something
;;    (do-something))


;; function to shutdown emacs server instance
(defun server-shutdown (&optional no-confirm)
  "Save buffers, Quit, and Shutdown (kill) server.
If `no-confirm' is set to t, don't ask."
  (interactive)
  (save-some-buffers)
  (if no-confirm
    (kill-emacs)
    (when (y-or-n-p "Really shutdown the server ?")
      (kill-emacs))))


;; eval-and-replace
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;; neotree intergration with projectile
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (linum-mode nil)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;; untabify buffer
(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((org-src-edit-buffer-p) (org-edit-src-exit))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun load-theme--disable-old-theme(theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)


(defun t--add-to-list-multiple (list to-add)
  "Adds multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))


(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))


(defun counsel-rg-thing-at-point ()
  "`counsel-rg' with `ivy-thing-at-point'."
  (interactive)
  (let ((thing (ivy-thing-at-point)))
    (when (use-region-p)
      (deactivate-mark))
    (counsel-rg (regexp-quote thing))))

(defun sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t)) ;; To make `end-of-line' and etc. to ignore fields.
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))


(defun t--insert-right-single-quotation-mark ()
  "Insert a RIGHT SINGLE QUOTATION MARK character."
  (interactive)
  (insert "’"))


(unless (package-installed-p 'major-mode-hydra)
  (package-install 'major-mode-hydra))
(require 'pretty-hydra)

(pretty-hydra-define multiple-cursors-hydra
  (:title "Multiple-cursors" :color amaranth :quit-key "q")
  ("Mark"
   (("l" mc/mark-next-like-this "next")
    ("j" mc/mark-previous-like-this "previous")
    ("m" mc/mark-more-like-this-extended "more-like-this-extended" :color blue))
   "Unmark"
   (("h" mc/unmark-next-like-this "next")
    (";" mc/unmark-previous-like-this "previous"))
   "Multiple occurrences"
   (("e" mc/edit-lines "edit-lines" :color blue)
    ("a" mc/mark-all-like-this-dwim "mark-all-like-this-dwim"))
   "Special"
   (("t" mc/mark-sgml-tag-pair "mark-sgml-tag-pair" :color blue)
    ("f" mc/insert-numbers "insert-numbers" :color blue)
    ("g" mc/insert-letters "insert-letters" :color blue))
   "Special 2"
   (("s" mc/sort-regions "sort-regions" :color blue)
    ("r" mc/reverse-regions "reverse-regions" :color blue))))


(pretty-hydra-define transpose-hydra
  (:title "Transpose" :color pink :quit-key "q")
  ("Normal"
   (("s" transpose-chars "chars")
    ("d" transpose-words "words")
    ("f" transpose-lines "lines")
    ("a" transpose-sentences "sentences")
    ("g" transpose-paragraphs "paragraphs"))
   "Sexps"
   (("w" transpose-sexps "normal")
    ("e" sp-transpose-sexp "sp")
    ("r" sp-transpose-hybrid-sexp "sp hybrid"))))


(pretty-hydra-define smerge-hydra
  (:title "Smerge" :color pink :quit-key "q")
  ("Navigation"
   (("o" smerge-next "next")
    ("u" smerge-prev "prev"))
   "Smerging"
   (("f" smerge-keep-current "current")
    ("e" smerge-keep-mine "mine/upper")
    ("w" smerge-keep-other "other/lower")
    ("a" smerge-keep-all "all"))))


(provide 'base-functions)
