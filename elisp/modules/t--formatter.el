;;; elisp/modules/t--formatter.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Formatter setup.
;; Wraps and configure apheleia.


;;;###autoload
(cl-defun t--set-formatter (name args &key modes)
  "Define (or modify) a formatter named NAME.

Supported keywords: :modes

NAME is a symbol that identifies this formatter.

FORMATTER can be a symbol referring to another formatter, a function, string or
nested list.

For more information on how to structure the list to be compatible, see
`apheleia--run-formatter-function'.

MODES is a major mode, a list thereof, or a list of two-element sublists with
the structure: (MAJOR-MODE FORM). FORM is evaluated when the buffer is formatted
and its return value serves two purposes:

  1. It is a predicate for this formatter. Assuming the MAJOR-MODE matches the
     current mode, if FORM evaluates to nil, the formatter is skipped.
  2. It's return value is made available to FORMATTER if it is a function or
     list of shell arguments via the `mode-result' variable.

Basic examples:
  (t--set-formatter \\='asmfmt \"asmfmt\" :modes \\='(asm-mode nasm-mode))
  (t--set-formatter \\='black \"black -q -\")
  (t--set-formatter \\='html-tidy \"tidy -q -indent\" :modes \\='(html-mode web-mode))"
  (cl-check-type name symbol)
  (with-eval-after-load 'apheleia
    (unless (null args)
      (let ((formatter (cond ((listp args) `(,@args))
                             (t args))))
        (setf (alist-get name apheleia-formatters) formatter)))
    (when modes
      (dolist (mode modes)
        (setf (alist-get mode apheleia-mode-alist) name)))))


(use-package apheleia
  :defer t
  :config
  (push '(vfmt . ("v" "fmt" "-w")) apheleia-formatters)
  (push '(v-mode . vfmt) apheleia-mode-alist)
  (push '(sqlfluff . ("sqlfluff" "fix" "--disable-progress-bar" "-")) apheleia-formatters)
  (push '(sql-mode . sqlfluff) apheleia-mode-alist)
  (if (executable-find "scalafmt-native")
      (push '(scalafmt . ("scalafmt-native" "--stdin" "--non-interactive" "--quiet" "--stdout")) apheleia-formatters)
    (progn
      (warn "scalafmt-native is not present on the path, reverting to scalafmt jar.")
      (push '(scalafmt . ("scalafmt" "--stdin" "--non-interactive" "--quiet" "--stdout")) apheleia-formatters)))
  (push '(scala-mode . scalafmt) apheleia-mode-alist)
  (push '(gdformat . ("gdformat" "-")) apheleia-formatters)
  (push '(gdscript-mode . gdformat) apheleia-mode-alist))

(provide 't--formatter)
