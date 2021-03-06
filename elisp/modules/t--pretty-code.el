;;; elisp/pretty-code.el -*- lexical-binding: t; -*-

;; (require 'prettify-utils)

(add-hook 'prog-mode-hook 'prettify-symbols-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stolen from Doom Emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defvar +pretty-code-symbols
  '(;; Org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    :quote         "“"
    :quote_end     "”"
    :checkbox      "☐"
    :pending       "◼"
    :checkedbox    "☑"
    :list_property "∷"
    :em_dash       "—"
    :ellipses      "…"
    :title         "𝙏"
    :subtitle      "𝙩"
    :language      "𝙇"
    :author        "𝘼"
    :email         "𝙀"
    :date          "𝘿"
    :options       "⌥"
    :latex_class   "Ⓒ"
    :latex_header  "⇥"
    :beamer_header "↠"
    :caption       "☰"
    :header        "›"
    :results       "➥"
    :begin_export  "⯮"
    :end_export    "⯬"
    :properties    "⚙"
    :end           "∎"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℕ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    ;; Flow
    :not           "¬"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•")
  "Options plist for `set-pretty-symbols!'.
This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

;;;###autoload
(defvar +pretty-code-symbols-alist '((t))
  "An alist containing a mapping of major modes to its value for
`prettify-symbols-alist'.")

;;;###autodef
(defun +pretty-code--correct-symbol-bounds (ligature-alist)
  "Prepend non-breaking spaces to a ligature.
This way `compose-region' (called by `prettify-symbols-mode') will use the
correct width of the symbols instead of the width measured by `char-width'."
  (let ((len (length (car ligature-alist)))
        (acc (list   (cdr ligature-alist))))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc))
            len (1- len)))
    (cons (car ligature-alist) acc)))

;;;###autodef
(defun set-pretty-symbols! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.
  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in `+pretty-code-symbols',
and whose values are strings representing the text to be replaced with that
symbol. If the car of PLIST is nil, then unset any pretty symbols previously
defined for MODES.
The following properties are special:
  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+pretty-code-symbols'.
  :merge BOOL
    If non-nil, merge with previously defined `prettify-symbols-alist',
    otherwise overwrite it.
For example, the rule for emacs-lisp-mode is very simple:
  (set-pretty-symbols! 'emacs-lisp-mode
    :lambda \"lambda\")
This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+pretty-code-symbols'.
Pretty symbols can be unset for emacs-lisp-mode with:
  (set-pretty-symbols! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (doom-enlist modes))
        (delq (assq mode +pretty-code-symbols-alist)
              +pretty-code-symbols-alist))
    (let (results merge key)
      (while plist
        (pcase (setq key (pop plist))
          (:merge (setq merge (pop plist)))
          (:alist (setq results (append (pop plist) results)))
          (_
           (when-let* ((char (plist-get +pretty-code-symbols key)))
             (push (cons (pop plist) char) results)))))
      (dolist (mode (doom-enlist modes))
        (unless merge
          (delq (assq mode +pretty-code-symbols-alist)
                +pretty-code-symbols-alist))
        (push (cons mode results) +pretty-code-symbols-alist)))))


(defun +pretty-code|init-pretty-symbols ()
  "Enable `prettify-symbols-mode'.
If in fundamental-mode, or a mode derived from special, comint, eshell or term
modes, this function does nothing.
Otherwise it builds `prettify-code-symbols-alist' according to
`+pretty-code-symbols-alist' for the current major-mode."
  (setq prettify-symbols-alist
        (append (cdr (assq major-mode +pretty-code-symbols-alist))
                (default-value 'prettify-symbols-alist)))
  (when prettify-symbols-mode
    (prettify-symbols-mode -1))
  (prettify-symbols-mode +1))

(add-hook 'after-change-major-mode-hook #'+pretty-code|init-pretty-symbols)


(provide 't--pretty-code)
