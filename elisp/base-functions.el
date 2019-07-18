;; Add your custom functions here

;; (defun something
;;    (do-something))

;; function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

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

(defun load-theme--disable-old-theme(theme &rest args)
  "Disable current theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'load-theme--disable-old-theme)


;; Transparency support ?
;; Works well on Emacs 26.2 built with toolkit=lucid on Manjaro Gnome
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

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


(provide 'base-functions)
