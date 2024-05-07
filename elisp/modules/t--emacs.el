;;; elisp/modules/t--emacs.el -*- lexical-binding: t; -*-
;;; Commentary:
; These packages are relative to some Emacs built-in features.

;;; Dired
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-rainbow
  :disabled
  :after dired
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

(use-package ediff
  :commands ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package emacs-everywhere
  :ensure-system-package ((xprop . xorg-xprop)
                          (xwininfo . xorg-xwininfo)
                          xclip
                          xdotool)
  :commands emacs-everywhere
  :init
  (setq emacs-everywhere-frame-name-format "emacs-everywhere"
        emacs-everywhere-mode-initial-map nil))

(use-package helpful
  :commands (helpful-callable
             helpful-at-point
             helpful-variable
             helpful-key))

(use-package ibuffer-vc
  :after ibuffer
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                vc-relative-file)))
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package nerd-icons-ibuffer
  :after ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package lacarte
  :load-path (lambda () (concat user-emacs-directory "elisp/extlisp/lacarte.el"))
  :commands (lacarte-execute-menu-command))

(use-package profiler
  :ensure nil
  :bind
  (("C-c t p" . profiler-start)
   ("C-c t s" . profiler-stop)))

(use-package paradox
  :commands paradox-list-packages)

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name
                           (concat user-emacs-directory "private/cache/recentf"))
        recentf-max-saved-items 25)
  (recentf-mode 1))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package shell-command+
  :commands (shell-command+))

(use-package casual
  :bind (:map calc-mode-map ("M" . 'casual-main-menu)))

;;; Taken from
;;; endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
;; Fix ANSI colors in compilation-mode
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'endless/colorize-compilation)

(defun t--ansi-colorize ()
  "ANSI-colorize buffer."
  (interactive)
  (if (region-active-p)
      (ansi-color-apply-on-region (region-beginning) (region-end))
    (ansi-color-apply-on-region (point-min) (point-max))))

(provide 't--emacs)
