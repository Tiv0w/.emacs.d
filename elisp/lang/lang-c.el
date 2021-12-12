;;; elisp/lang/lang-c.el -*- lexical-binding: t; -*-

(defun t--c-or-c++-mode ()
  (let ((base (file-name-sans-extension (buffer-file-name (buffer-base-buffer)))))
    (if (file-exists-p (or (concat base ".cpp")
                           (concat base ".cc")
                           (concat base ".cxx")
                           (concat base ".c++")))
        (c++-mode)
      (c-mode))))

(defun t--jump-to-h-or-c/cpp (&optional arg)
  (interactive "i")
  (let ((base (file-name-sans-extension (buffer-file-name (buffer-base-buffer))))
        (extension (file-name-extension (buffer-file-name (buffer-base-buffer)))))
    (if (or (string-equal extension "cpp")
            (string-equal extension "c"))
        (find-file (concat base ".h"))
      (if (eq major-mode 'c-mode)
          (find-file (concat base ".c"))
        (find-file (concat base ".cpp"))))))

;; C-IDE based on https://github.com/tuhdo/emacs-c-ide-demo
(use-package cc-mode
  :mode ("\\.h\\'" . t--c-or-c++-mode)
  :config
  ;; Available C style:
  ;; "gnu": The default style for GNU projects
  ;; "k&r": What Kernighan and Ritchie, the authors of C used in their book
  ;; "bsd": What BSD developers use, aka "Allman style" after Eric Allman.
  ;; "whitesmith": Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
  ;; "stroustrup": What Stroustrup, the author of C++ used in his book
  ;; "ellemtel": Popular C++ coding standards as defined by "Programming in C++, Rules and Recommendations," Erik Nyquist and Mats Henricson, Ellemtel
  ;; "linux": What the Linux developers use for kernel development
  ;; "python": What Python developers use for extension modules
  ;; "java": The default style for java-mode (see below)
  ;; "user": When you want to define your own style
  (setq c-default-style "cc-mode") ;; set style to "cc-mode"
  (setq gdb-many-windows t ;; use gdb-many-windows by default
        gdb-show-main t))

(major-mode-hydra-define (c++-mode c-mode)
  (:title "C/C++" :color blue :quit-key "q")
  ("Navigation"
   (("m" t--jump-to-h-or-c/cpp "jump header/source")
    ("," eglot-find-declaration "jump to def")
    ("." eglot-find-implementation "jump to impl"))
   "Editing"
   (("s" eglot-rename "rename symbol")
    ("f" eglot-code-actions "code actions"))
   "Misc"
   (("b" eglot-format-buffer "format buffer"))))

(use-package irony
  :hook ((c++-mode c-mode) . irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :after company
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package flycheck
  :hook ((c++-mode c-mode) . flycheck-mode)
  :config
  (use-package flycheck-irony
    :after flycheck
    :hook (flycheck-mode . flycheck-irony-setup)))

(use-package irony-eldoc
  :hook (irony-mode . irony-eldoc))

(use-package yafolding
  :hook (c-mode-common . yafolding-mode))

(use-package basic-c-compile)

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package makefile-executor
  :hook (makefile-mode . makefile-executor-mode))

(use-package ggtags
  :disabled
  :hook c-mode
  :config
  (ggtags-mode 1)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1))))

  (dolist (map (list ggtags-mode-map))
    (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
    (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
    (define-key map (kbd "C-c g r") 'ggtags-find-reference)
    (define-key map (kbd "C-c g f") 'ggtags-find-file)
    (define-key map (kbd "C-c g c") 'ggtags-create-tags)
    (define-key map (kbd "C-c g u") 'ggtags-update-tags)
    (define-key map (kbd "M-.")     'ggtags-find-tag-dwim)
    (define-key map (kbd "M-,")     'pop-tag-mark)
    (define-key map (kbd "C-c <")   'ggtags-prev-mark)
    (define-key map (kbd "C-c >")   'ggtags-next-mark)))

;; https://github.com/syohex/emacs-counsel-gtags
(use-package counsel-gtags
  :disabled
  :config
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook counsel-gtags-mode)

  (with-eval-after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
    ;;(define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    ;;(define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-pop-stack)))

;; (defun alexott/cedet-hook ()
;;   (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
;;   (local-set-key (kbd "C-c C-s") 'semantic-ia-show-summary))


(set-pretty-symbols! '(c-mode c++-mode cc-mode)
  ;; Functional
  ;; :def "void "
  ;; Types
  :null "NULL"
  :true "true"
  :false "false"
  :int "int"
  :float "float"
  :str "std::string"
  :bool "bool"
  ;; Flow
  :not "!"
  :and "&&"
  :or "||"
  :for "for"
  :return "return"
  :yield "#require")


(use-package glsl-mode)

(provide 'lang-c)
