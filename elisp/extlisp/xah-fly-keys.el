;;; xah-fly-keys.el --- ergonomic modal keybinding minor mode. -*- coding: utf-8; lexical-binding: t; byte-compile-dynamic: t; -*-

;; Copyright © 2013-2020, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 11.5.20200427065720
;; Created: 10 Sep 2013
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, emulations, vim, ergoemacs
;; License: GPL v3
;; Homepage: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; xah-fly-keys is a efficient keybinding for emacs. (more efficient than vim)

;; It is a modal mode like vi, but key choices are based on statistics of command call frequency.

;; --------------------------------------------------
;; MANUAL INSTALL

;; put the file xah-fly-keys.el in ~/.emacs.d/lisp/
;; create the dir if doesn't exist.

;; put the following in your emacs init file:

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout 'qwerty) ; required

;; possible layout values:

;; 'azerty
;; 'azerty-be
;; 'colemak
;; 'colemak-mod-dh
;; 'dvorak
;; 'programer-dvorak
;; 'qwerty
;; 'qwerty-abnt
;; 'qwerty-no (qwerty Norwegian)
;; 'qwertz
;; 'workman
;; 'norman

;; For backward compatibility, strings with the above names are also accepted.

;; (xah-fly-keys 1)

;; --------------------------------------------------
;; HOW TO USE

;; M-x xah-fly-keys to toggle the mode on/off.

;; Important command/insert mode switch keys:

;; xah-fly-command-mode-activate (press 【<home>】 or 【F8】 or 【Alt+Space】 or 【menu】)

;; xah-fly-insert-mode-activate  (when in command mode, press qwerty letter key f. (Dvorak key u))

;; When in command mode:
;; 【f】 (or Dvorak 【u】) activates insertion mode.
;; 【Space】 is a leader key. For example, 【SPACE r】 (Dvorak 【SPACE p】) calls query-replace. Press 【SPACE C-h】 to see the full list.
;; 【Space Space】 also activates insertion mode.
;; 【Space Enter】 calls execute-extended-command.
;; 【a】 calls execute-extended-command or smex or helm or counsel (if they are installed).

;; The leader key sequence basically replace ALL emacs commands that starts with C-x key.

;; When using xah-fly-keys, you don't need to press Control or Meta, with the following exceptions:

;; C-c for major mode commands.
;; C-g for cancel.
;; C-q for quoted-insert.
;; C-h for getting a list of keys following a prefix/leader key.

;; Leader key

;; You NEVER need to press Ctrl+x

;; Any emacs command that has a keybinding starting with C-x, has also a key sequence binding in xah-fly-keys. For example,
;; 【C-x b】 switch-to-buffer is 【SPACE f】 (Dvorak 【SPACE u】)
;; 【C-x C-f】 find-file is 【SPACE i e】 (Dvorak 【SPACE c .】)
;; 【C-x n n】 narrow-to-region is 【SPACE l l】 (Dvorak 【SPACE n n】)
;; The first key we call it leader key. In the above examples, the SPACE is the leader key.

;; When in command mode, the 【SPACE】 is a leader key.

;; globally, the leader key is the 【f9】 key. 【f9】 is leader key regardless it's in command mode or insert mode.

;; the following standard keys with Control are supported:

 ;; 【Ctrl+tab】 'xah-next-user-buffer
 ;; 【Ctrl+shift+tab】 'xah-previous-user-buffer
 ;; 【Ctrl+v】 paste
 ;; 【Ctrl+w】 close
 ;; 【Ctrl+z】 undo
 ;; 【Ctrl+n】 new
 ;; 【Ctrl+o】 open
 ;; 【Ctrl+s】 save
 ;; 【Ctrl+shift+s】 save as
 ;; 【Ctrl+shift+t】 open last closed
 ;; 【Ctrl++】 'text-scale-increase
 ;; 【Ctrl+-】 'text-scale-decrease

;; To disable both Control and Meta shortcut keys, add the following lines to you init.el before (require 'xah-fly-keys):
;; (setq xah-fly-use-control-key nil)
;; (setq xah-fly-use-meta-key nil)

;; I highly recommend setting 【capslock】 to send 【Home】. So that it acts as `xah-fly-command-mode-activate'.
;; see
;; How to Make the CapsLock Key do Home Key
;; http://ergoemacs.org/misc/capslock_do_home_key.html

;; If you have a bug, post on github.

;; For detail about design and other info, see home page at
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.


;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs
(require 'ido) ; in emacs



(defgroup xah-fly-keys nil
  "Ergonomic modal keybinding minor mode."
  :group 'keyboard)

(defvar xah-fly-command-mode-activate-hook nil "Hook for `xah-fly-command-mode-activate'")
(defvar xah-fly-insert-mode-activate-hook nil "Hook for `xah-fly-insert-mode-activate'")

(defcustom xah-fly-use-control-key t
  "If nil, do not bind any control key. When t, standard keys for open, close, paste, are bound."
  :type 'boolean
  :group 'xah-fly-keys)
(defcustom xah-fly-use-meta-key t
  "If nil, do not bind any meta key."
  :type 'boolean
  :group 'xah-fly-keys)
(defcustom xah-fly-use-isearch-arrows t
  "If nil, do not bind the arrow keys to move between matches in Isearch."
  :type 'boolean
  :group 'xah-fly-keys)


;; cursor movement

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous paragraph.

• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "move")
            (progn
              (skip-chars-backward "\n\t ")
              ;; (forward-char )
              )
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next paragraph.

• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.

URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version 2018-06-04"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "move" ))
    (end-of-line)))

(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>＜＞（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defvar xah-punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq xah-punctuation-regex "[!\?\"\.,`'#$%&*+:;=@^|~]+")

(defun xah-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `xah-punctuation-regex'

URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2017-06-26"
  (interactive "p")
  (re-search-forward xah-punctuation-regex nil t n))

(defun xah-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'

URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2017-06-26"
  (interactive "p")
  (re-search-backward xah-punctuation-regex nil t n))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun xah-forward-equal-quote ()
  "Move cursor to the next occurrence of 「='」 or 「=\"」, with or without space.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-05-05"
  (interactive)
  (re-search-forward "=[ \n]*\\('+\\|\\\"+\\)" nil t))

(defun xah-forward-equal-sign ()
  "Move cursor to the next occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (re-search-forward "=+" nil t))

(defun xah-backward-equal-sign ()
  "Move cursor to previous occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (when (re-search-backward "=+" nil t)
    (while (search-backward "=" (- (point) 1) t)
      (left-char))))

(defun xah-forward-comma-sign ()
  "Move cursor to the next occurrence of comma 「,」.
Version 2016-01-19"
  (interactive)
  (re-search-forward ",+" nil t))

(defun xah-backward-comma-sign ()
  "Move cursor to previous occurrence of comma sign 「,」.
Version 2016-01-19"
  (interactive)
  (when (re-search-backward ",+" nil t)
    (while (search-backward "," (- (point) 1) t)
      (left-char))))

(defun xah-forward-quote ()
  "Move cursor to the next occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (if (re-search-forward "\\\"+" nil t)
      t
    (progn
      (message "No more quotes after cursor..")
      nil)))

(defun xah-forward-quote-twice ()
  "Call `xah-forward-quote' twice.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (when (xah-forward-quote)
    (xah-forward-quote)))

(defun xah-forward-quote-smart ()
  "Move cursor to the current or next string quote.
Place cursor at the position after the left quote.
Repeated call will find the next string.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (let (($pos (point)))
    (if (nth 3 (syntax-ppss))
        (progn
          (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
          (forward-sexp)
          (re-search-forward "\\\"" nil t))
      (progn (re-search-forward "\\\"" nil t)))
    (when (<= (point) $pos)
      (progn (re-search-forward "\\\"" nil t)))))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (if (re-search-backward "\\\"+" nil t)
      (when (char-before) ; isn't nil, at beginning of buffer
        (while (char-equal (char-before) (char-after))
          (left-char)
          t))
    (progn
      (message "No more quotes before cursor.")
      nil)))

(defun xah-forward-dot-comma ()
  "Move cursor to the next occurrence of 「.」 「,」 「;」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (re-search-forward "\\.+\\|,+\\|;+" nil t))

(defun xah-backward-dot-comma ()
  "Move cursor to the previous occurrence of 「.」 「,」 「;」
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (re-search-backward "\\.+\\|,+\\|;+" nil t))

;; (defun goto-point-min ()
;;   "Goto the beginning of buffer.
;; This is different from `beginning-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-min))
;; )

;; (defun goto-point-max ()
;;   "Goto the end of buffer.
;; This is different from `end-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-max))
;; )

;; (defun xah-forward-space ()
;;   "Move cursor to the next occurrence of white space."
;;   (interactive)
;;   (re-search-forward "[ \t\n]+" nil t))

;; (defun xah-backward-space ()
;;   "Move cursor to the next occurrence of white space."
;;   (interactive)
;;   ;; (skip-chars-backward "^ \t\n")
;;   ;; (re-search-backward "[ \t\n]+" nil t)
;;   (posix-search-backward "[ \t\n]+" nil t)
;;   )


;; editing commands

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2019-10-30"
  (interactive)
  (let ((inhibit-field-text-motion nil))
    (if current-prefix-arg
        (progn
          (copy-region-as-kill (point-min) (point-max)))
      (if (region-active-p)
          (progn
            (copy-region-as-kill (region-beginning) (region-end)))
        (if (eq last-command this-command)
            (if (eobp)
                (progn )
              (progn
                (kill-append "\n" nil)
                (kill-append
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                 nil)
                (progn
                  (end-of-line)
                  (forward-char))))
          (if (eobp)
              (if (eq (char-before) 10 )
                  (progn )
                (progn
                  (copy-region-as-kill (line-beginning-position) (line-end-position))
                  (end-of-line)))
            (progn
              (copy-region-as-kill (line-beginning-position) (line-end-position))
              (end-of-line)
              (forward-char))))))))

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (region-active-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-all-or-region ()
  "Put the whole buffer content to `kill-ring', or text selection if there's one.
Respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (region-active-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (message "Text selection copied."))
    (progn
      (kill-new (buffer-string))
      (message "Buffer content copied."))))

(defun xah-cut-all-or-region ()
  "Cut the whole buffer content to `kill-ring', or text selection if there's one.
Respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (region-active-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
    (progn
      (kill-new (buffer-string))
      (delete-region (point-min) (point-max)))))

(defun xah-copy-all ()
  "Put the whole buffer content into the `kill-ring'.
(respects `narrow-to-region')
Version 2016-10-06"
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
Respects `narrow-to-region'.
Version 2017-01-03"
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))

(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.

When `universal-argument' is called first with a number arg, paste that many times.

URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
Version 2017-07-25"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes (_ (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

(defun xah-show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer named *copy history*.

URL `http://ergoemacs.org/emacs/emacs_show_kill_ring.html'
Version 2019-12-02"
  (interactive)
  (let (($buf (generate-new-buffer "*copy history*")))
    (progn
      (switch-to-buffer $buf)
      (funcall 'fundamental-mode)
      (dolist (x kill-ring )
        (insert x "\n\nhh=============================================================================\n\n"))
      (goto-char (point-min)))))

(defun xah-kill-word ()
  "Like `kill-word', but delete selection first if there's one.
Version 2018-08-31"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (kill-word 1))

(defun xah-backward-kill-word ()
  "Like `backward-kill-word', but delete selection first if there's one.
Version 2018-08-31"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (backward-kill-word 1))

(defun xah-delete-backward-char-or-bracket-text ()
  "Delete backward 1 character, but if it's a \"quote\" or bracket ()[]{}【】「」 etc, delete bracket and the inner text, push the deleted text to `kill-ring'.

What char is considered bracket or quote is determined by current syntax table.

If `universal-argument' is called first, do not delete inner text.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    (cond
     ((looking-back "\\s)" 1)
      (if current-prefix-arg
          (xah-delete-backward-bracket-pair)
        (xah-delete-backward-bracket-text)))
     ((looking-back "\\s(" 1)
      (progn
        (backward-char)
        (forward-sexp)
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     ((looking-back "\\s\"" 1)
      (if (nth 3 (syntax-ppss))
          (progn
            (backward-char)
            (xah-delete-forward-bracket-pairs (not current-prefix-arg)))
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     (t
      (delete-char -1)))))

(defun xah-delete-backward-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor, including the inner text.

This command assumes the left of cursor is a right bracket, and there's a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-09-21"
  (interactive)
  (progn
    (forward-sexp -1)
    (mark-sexp)
    (kill-region (region-beginning) (region-end))))

(defun xah-delete-backward-bracket-pair ()
  "Delete the matching brackets/quotes to the left of cursor.
After call, mark is set at the matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the left of point is a right bracket, and there is a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (let (( $p0 (point)) $p1)
    (forward-sexp -1)
    (setq $p1 (point))
    (goto-char $p0)
    (delete-char -1)
    (goto-char $p1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- $p0 2))))

(defun xah-delete-forward-bracket-pairs ( &optional @delete-inner-text-p)
  "Delete the matching brackets/quotes to the right of cursor.
If @delete-inner-text-p is true, also delete the inner text.

After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the char to the right of point is a left bracket or quote, and have a matching one after.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if @delete-inner-text-p
      (progn
        (mark-sexp)
        (kill-region (region-beginning) (region-end)))
    (let (($pt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char $pt)
      (delete-char 1))))

(defun xah-change-bracket-pairs ( @from-chars @to-chars)
  "Change bracket pairs from one type to another.

For example, change all parenthesis () to square brackets [].

Works on selected text, or current text block.

When called in lisp program, @from-chars or @to-chars is a string of bracket pair. eg \"(paren)\",  \"[bracket]\", etc.
The first and last characters are used. (the middle is for convenience in ido selection.)
If the string contains “,2”, then the first 2 chars and last 2 chars are used, for example  \"[[bracket,2]]\".
If @to-chars is equal to string “none”, the brackets are deleted.

URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'
Version 2019-02-12"
  (interactive
   (let (($bracketsList
          '("(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "\"ascii quote\""
            "[[double square,2]]"
            "“curly quote”"
            "‘single quote’"
            "‹angle quote›"
            "«double angle quote»"
            "「corner」"
            "『white corner』"
            "【LENTICULAR】"
            "〖white LENTICULAR〗"
            "〈angle〉"
            "《double angle》"
            "〔TORTOISE〕"
            "〘WHITE TORTOISE SHELL〙"
            "⦅white paren⦆"
            "〚white square〛"
            "⦃white curly⦄"
            "〈angle〉"
            "⦑ANGLE WITH DOT⦒"
            "⧼CURVED ANGLE⧽"
            "⟦math square⟧"
            "⟨math angle⟩"
            "⟪math DOUBLE ANGLE⟫"
            "⟮math FLATTENED PARENTHESIS⟯"
            "⟬math WHITE TORTOISE SHELL⟭"
            "❛HEAVY SINGLE QUOTATION MARK ORNAMENT❜"
            "❝HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT❞"
            "❨MEDIUM LEFT PARENTHESIS ORNAMENT❩"
            "❪MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT❫"
            "❴MEDIUM LEFT CURLY ORNAMENT❵"
            "❬MEDIUM LEFT-POINTING ANGLE ORNAMENT❭"
            "❮HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT❯"
            "❰HEAVY LEFT-POINTING ANGLE ORNAMENT❱"
            "none"
            )))
     (list
      (ido-completing-read "Replace this:" $bracketsList )
      (ido-completing-read "To:" $bracketsList ))))
  (let ( $p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil)
               $fromLeft
               $fromRight
               $toLeft
               $toRight)
          (cond
           ((string-match ",2" @from-chars  )
            (progn
              (setq $fromLeft (substring @from-chars 0 2))
              (setq $fromRight (substring @from-chars -2))))
           (t
            (progn
              (setq $fromLeft (substring @from-chars 0 1))
              (setq $fromRight (substring @from-chars -1)))))
          (cond
           ((string-match ",2" @to-chars)
            (progn
              (setq $toLeft (substring @to-chars 0 2))
              (setq $toRight (substring @to-chars -2))))
           ((string-match "none" @to-chars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring @to-chars 0 1))
              (setq $toRight (substring @to-chars -1)))))
          (cond
           ((string-match "markdown" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "tilde" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "ascii quote" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "equal" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "=\\([^=]+?\\)=" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           (t (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromLeft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toLeft "FIXEDCASE" "LITERAL")))
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromRight nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toRight "FIXEDCASE" "LITERAL")))))))))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2019-11-24"
  (interactive)
  (let ( (deactivate-mark nil) $p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

;; test case
;; test_case some
;; test-case
;; tes▮t-case

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-12-22"
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(defun xah-upcase-sentence ()
  "Upcase first letters of sentences of current block or selection.

URL `http://xahlee.info/emacs/emacs/emacs_upcase_sentence.html'
Version: 2020-12-08 2020-12-24 2021-08-13"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn
              (setq $p1 (point))
              (re-search-forward "\n[ \t]*\n"))
          (setq $p1 (point)))
        (progn
          (re-search-forward "\n[ \t]*\n" nil "move")
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ((case-fold-search nil))

          ;; after period or question mark or exclamation
          (goto-char (point-min))
          (while (re-search-forward "\\(\\.\\|\\?\\|!\\)[ \n]+ *\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))

          ;; after a blank line, after a bullet, or beginning of buffer
          (goto-char (point-min))
          (while (re-search-forward "\\(\\`\\|• \\|\n\n\\)\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))

          ;; for HTML. first letter after tag
          (goto-char (point-min))
          (while (re-search-forward "\\(<p>\n?\\|<li>\\|<td>\n?\\|<figcaption>\n?\\)\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))

          (goto-char (point-min)))))))

(defun xah-title-case-region-or-line (@begin @end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, @begin @end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version: 2018-04-02"
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun xah-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version: 2019-06-13"
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor .

Shrink neighboring spaces, then newlines, then spaces again, leaving one space or newline at each step, till no more white space.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version: 2014-10-21 2021-11-26 2021-11-30"
  (interactive)
  (let* (($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9))))
    (skip-chars-backward " \n\t　")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t　")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t)
      (setq $eol-count (1+ $eol-count)))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if (> (- $p2 $p1) 1)
          (progn
            (delete-horizontal-space) (insert " "))
        (progn (delete-horizontal-space))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (goto-char $p2)
          (search-backward "\n")
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here"))))))

(defun xah-toggle-read-novel-mode ()
  "Setup current frame to be suitable for reading long novel/article text.

• Set frame width to 70
• Line wrap at word boundaries.
• Line spacing is increased.
• Proportional width font is used.

Call again to toggle back.
URL `http://ergoemacs.org/emacs/emacs_novel_reading_mode.html'
Version 2019-01-30"
  (interactive)
  (if (eq (frame-parameter (selected-frame) 'width) 70)
      (progn
        (set-frame-parameter (selected-frame) 'width 106)
        (variable-pitch-mode 0)
        (setq line-spacing nil)
        (setq word-wrap nil))
    (progn
      (set-frame-parameter (selected-frame) 'width 70)
      (variable-pitch-mode 1)
      (setq line-spacing 0.5)
      (setq word-wrap t)))
  (redraw-frame (selected-frame)))

(defun xah-fill-or-unfill ()
  "Reformat current block or selection to short/long line.
First call will break into multiple short lines. Repeated call toggles between short and long lines.
This commands calls `fill-region' to do its work. Set `fill-column' for short line length.

URL `http://xahlee.info/emacs/emacs/modernization_fill-paragraph.html'
Version: 2020-11-22 2021-08-13"
  (interactive)
  ;; This command symbol has a property “'longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ($isLongline (if (eq last-command this-command) (get this-command 'longline-p) t))
         (deactivate-mark nil)
         $p1 $p2 )
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (if $isLongline
        (fill-region $p1 $p2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region $p1 $p2)))
    (put this-command 'longline-p (not $isLongline))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://xahlee.info/emacs/emacs/emacs_unfill-paragraph.html'
Version: 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun xah-unfill-region (Begin End)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://xahlee.info/emacs/emacs/emacs_unfill-paragraph.html'
Version: 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region Begin End)))

(defun xah-reformat-lines ( &optional @length)
  "Reformat current text block into 1 long line or multiple short lines.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

When the command is called for the first time, it checks the current line's length to decide to go into 1 line or multiple lines. If current line is short, it'll reformat to 1 long lines. And vice versa.

Repeated call toggles between formatting to 1 long line and multiple lines.

If `universal-argument' is called first, use the number value for min length of line. By default, it's 70.

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2019-06-09"
  (interactive)
  ;; This command symbol has a property “'is-longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let* (
         (@length (if @length
                      @length
                    (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column )))
         (is-longline-p
          (if (eq last-command this-command)
              (get this-command 'is-longline-p)
            (> (- (line-end-position) (line-beginning-position)) @length)))
         ($blanks-regex "\n[ \t]*\n")
         $p1 $p2
         )
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward $blanks-regex nil "move")
            (progn (re-search-forward $blanks-regex)
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward $blanks-regex nil "move")
            (progn (re-search-backward $blanks-regex)
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (progn
      (if current-prefix-arg
          (xah-reformat-to-multi-lines $p1 $p2 @length)
        (if is-longline-p
            (xah-reformat-to-multi-lines $p1 $p2 @length)
          (xah-reformat-whitespaces-to-one-space $p1 $p2)))
      (put this-command 'is-longline-p (not is-longline-p)))))

(defun xah-reformat-whitespaces-to-one-space (@begin @end)
  "Replace whitespaces by one space.

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2017-01-11"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while
          (search-forward "\n" nil "move")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (search-forward "\t" nil "move")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (re-search-forward "  +" nil "move")
        (replace-match " ")))))

(defun xah-reformat-to-multi-lines ( &optional @begin @end @min-length)
  "Replace spaces by a newline at places so lines are not long.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

If `universal-argument' is called first, use the number value for min length of line. By default, it's 70.

URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2018-12-16"
  (interactive)
  (let (
        $p1 $p2
        ($blanks-regex "\n[ \t]*\n")
        ($minlen (if @min-length
                     @min-length
                   (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column))))
    (if (and  @begin @end)
        (setq $p1 @begin $p2 @end)
      (if (region-active-p)
          (progn (setq $p1 (region-beginning) $p2 (region-end)))
        (save-excursion
          (if (re-search-backward $blanks-regex nil "move")
              (progn (re-search-forward $blanks-regex)
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (if (re-search-forward $blanks-regex nil "move")
              (progn (re-search-backward $blanks-regex)
                     (setq $p2 (point)))
            (setq $p2 (point))))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while
            (re-search-forward " +" nil "move")
          (when (> (- (point) (line-beginning-position)) $minlen)
            (replace-match "\n" )))))))

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.

URL `http://ergoemacs.org/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_comment_by_line.html'
Version: 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

(defun xah-quote-lines (Begin End QuoteL QuoteR Sep)
  "Add quotes/brackets and separator (comma) to lines.
Act on current block or selection.

For example,

 cat
 dog
 cow

becomes

 \"cat\",
 \"dog\",
 \"cow\",

or

 (cat)
 (dog)
 (cow)

In lisp code, QuoteL QuoteR Sep are strings.

URL `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
Version: 2020-06-26 2021-07-21 2021-08-15 2021-09-15"
  (interactive
   (let* (($bds (xah-get-bounds-of-block-or-region))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($brackets
          '(
            "\"double\""
            "'single'"
            "(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "“curly double”"
            "‘curly single’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "none"
            "other"
            )) $bktChoice $sep $sepChoice $quoteL $quoteR)
     (setq $bktChoice (ido-completing-read "Quote to use:" $brackets))
     (setq $sepChoice (ido-completing-read "line separator:" '("," ";" "none" "other")))
     (cond
      ((string-equal $bktChoice "none")
       (setq $quoteL "" $quoteR ""))
      ((string-equal $bktChoice "other")
       (let (($x (read-string "Enter 2 chars, for begin/end quote:")))
         (setq $quoteL (substring-no-properties $x 0 1)
               $quoteR (substring-no-properties $x 1 2))))
      (t (setq $quoteL (substring-no-properties $bktChoice 0 1)
               $quoteR (substring-no-properties $bktChoice -1))))
     (setq $sep
           (cond
            ((string-equal $sepChoice "none") "")
            ((string-equal $sepChoice "other") (read-string "Enter separator:"))
            (t $sepChoice)))
     (list $p1 $p2 $quoteL $quoteR $sep)))
  (let (($p1 Begin) ($p2 End) ($quoteL QuoteL) ($quoteR QuoteR) ($sep Sep))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert $quoteL)
            (end-of-line)
            (insert $quoteR $sep)
            (if (eq (point) (point-max))
                (throw 'EndReached t)
              (forward-char))))))))

(defun xah-escape-quotes (@begin @end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun xah-unescape-quotes (@begin @end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to lowline _.
If not in `dired', do nothing.

URL `http://xahlee.info/emacs/emacs/elisp_dired_rename_space_to_underscore.html'
Version: 2016-10-04 2020-03-03"
  (interactive)
  (require 'dired-aux)
  (if (eq major-mode 'dired-mode)
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        ;; (dired-next-line 1)
        (revert-buffer)
        )
    (user-error "Not in dired")))

(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.

URL `http://xahlee.info/emacs/emacs/elisp_dired_rename_space_to_underscore.html'
Version: 2016-10-04 2019-11-24"
  (interactive)
  (require 'dired-aux)
  (if (eq major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "-" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(defun xah-cycle-hyphen-lowline-space (&optional Begin End)
  "Cycle hyphen/lowline/space chars in selection or inside quote/bracket or line, in that order.
After this command is called, press - to repeat it.
The region to work on is by this order:
 1. if there is a selection, use that.
 2. If cursor is string quote or any type of bracket, and is within current line, work on that region.
 3. else, work on current line.

URL `http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html'
Version: 2019-02-12 2021-08-20"
  (interactive)
  ;; this function sets a property 'state. Possible values are 0 to length of $charArray.
  (let* ($p1
         $p2
         ($charArray ["-" "_" " "])
         ($n (length $charArray))
         ($regionWasActive-p (region-active-p))
         ($nowState (if (eq last-command this-command) (get 'xah-cycle-hyphen-lowline-space 'state) 0))
         ($changeTo (elt $charArray $nowState)))
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (let (($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
          (skip-chars-backward $skipChars (line-beginning-position))
          (setq $p1 (point))
          (skip-chars-forward $skipChars (line-end-position))
          (setq $p2 (point))
          (set-mark $p1))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward (elt $charArray (% (+ $nowState 2) $n)) (point-max) 1)
          (replace-match $changeTo t t))))
    (when (or (string-equal $changeTo " ") $regionWasActive-p)
      (goto-char $p2)
      (set-mark $p1)
      (setq deactivate-mark nil))
    (put 'xah-cycle-hyphen-lowline-space 'state (% (+ $nowState 1) $n)))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "-") 'xah-cycle-hyphen-lowline-space) $kmap)))

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2018-06-18"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))

;; (defun xah-delete-text-block ()
;;   "Delete current/next text block or selection, and also copy to `kill-ring'.

;; A “block” is text between blank lines.
;; The “current block” is the block the cursor is at.
;; If cursor is not on a block, deletes the next block.
;; If there's a text selection, just delete that region.

;; URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
;; Version 2016-10-10"
;;   (interactive)
;;   (if (use-region-p)
;;       (kill-region (region-beginning) (region-end))
;;     (progn
;;       (beginning-of-line)
;;       (if (re-search-forward "[[:graph:]]" (line-end-position) "move")
;;           (xah-delete-current-text-block)
;;         (when (re-search-forward "[[:graph:]]" )
;;           (xah-delete-current-text-block))))))

(defun xah-delete-current-text-block ()
  "Delete the current text block or selection, and copy to `kill-ring'.
A “block” is text between blank lines.

URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
Version 2017-07-09"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil 1)
            (setq $p1 (goto-char (match-end 0)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n+" nil 1)
        (setq $p2 (point))))
    (kill-region $p1 $p2)))

(defun xah-clear-register-1 ()
  "Clear register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2015-12-08"
  (interactive)
  (progn
    (copy-to-register ?1 (point-min) (point-min))
    (message "Cleared register 1.")))

(defun xah-copy-to-register-1 ()
  "Copy current line or selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2017-01-23"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (copy-to-register ?1 $p1 $p2)
    (message "Copied to register 1: [%s]." (buffer-substring-no-properties $p1 $p2))))

(defun xah-append-to-register-1 ()
  "Append current line or selection to register 1.
When no selection, append current line, with newline char.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_append.html'
Version: 2015-12-08 2020-09-08"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (append-to-register ?1 $p1 $p2)
    (with-temp-buffer (insert "\n")
                      (append-to-register ?1 (point-min) (point-max)))
    (message "Appended to register 1: [%s]." (buffer-substring-no-properties $p1 $p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2015-12-08"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun xah-copy-rectangle-to-kill-ring (Begin End)
  "Copy region as column (rectangle region) to `kill-ring'
See also: `kill-rectangle', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2016-07-17"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat 'identity (extract-rectangle Begin End) "\n")))

;; HHH___________________________________________________________________
;; insertion commands

(defun xah-insert-date ()
  "Insert current date time.
Insert date in this format: yyyy-mm-dd.
If `universal-argument' is called first, prompt for a format to use.
If there is selection, delete it first.

URL `http://xahlee.info/emacs/emacs/elisp_insert-date-time.html'
version 2020-09-07 2021-11-07"
  (interactive)
  (let (($style
         (if current-prefix-arg
             (string-to-number
              (substring
               (ido-completing-read
                "Style:"
                '(
                  "1 → 20180412224611"
                  "2 → 2018-04-12_224611"
                  "3 → 2018-04-12T22:46:11-07:00"
                  "4 → 2018-04-12 22:46:11-07:00"
                  "5 → 2018-04-12 Thursday"
                  "6 → Thursday, April 12, 2018"
                  "7 → Thu, Apr 12, 2018"
                  "8 → April 12, 2018"
                  "9 → Apr 12, 2018"
                  )) 0 1))
           0
           )))
    (when (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((= $style 0)
       ;; "2016-10-10"
       (format-time-string "%Y-%m-%d"))
      ((= $style 1)
       ;; "1 → 20180412224611"
       (replace-regexp-in-string ":" "" (format-time-string "%Y%m%d%T")))
      ((= $style 2)
       ;; "2 → 2018-04-12_224611"
       (replace-regexp-in-string ":" "" (format-time-string "%Y-%m-%d_%T")))
      ((= $style 3)
       ;; "3 → 2018-04-12T22:46:11-07:00"
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z"))))
      ((= $style 4)
       ;; "4 → 2018-04-12 22:46:11-07:00"
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z"))))
      ((= $style 5)
       ;; "5 → 2018-04-12 Thursday"
       (format-time-string "%Y-%m-%d %A"))
      ((= $style 6)
       ;; "6 → Thursday, April 12, 2018"
       (format-time-string "%A, %B %d, %Y"))
      ((= $style 7)
       ;; "7 → Thu, Apr 12, 2018"
       (format-time-string "%a, %b %d, %Y"))
      ((= $style 8)
       ;; "8 → April 12, 2018"
       (format-time-string "%B %d, %Y"))
      ((= $style 9)
       ;; "9 → Apr 12, 2018"
       (format-time-string "%b %d, %Y"))
      (t
       (format-time-string "%Y-%m-%d"))))))

(defun xah-insert-bracket-pair (LBracket RBracket &optional WrapMethod)
  "Insert brackets around selection, word, at point, and maybe move cursor in between.

 LBracket and RBracket are strings. WrapMethod must be either 'line or 'block. 'block means between empty lines.

• if there is a region, add brackets around region.
• If WrapMethod is 'line, wrap around line.
• If WrapMethod is 'block, wrap around block.
• if cursor is at beginning of line and its not empty line and contain at least 1 space, wrap around the line.
• If cursor is at end of a word or buffer, one of the following will happen:
 xyz▮ → xyz(▮)
 xyz▮ → (xyz▮)       if in one of the lisp modes.
• wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)

URL `http://xahlee.info/emacs/emacs/elisp_insert_brackets_by_pair.html'
Version: 2017-01-17 2021-08-12"
  (if (region-active-p)
      (progn
        (let ( ($p1 (region-beginning)) ($p2 (region-end)))
          (goto-char $p2) (insert RBracket)
          (goto-char $p1) (insert LBracket)
          (goto-char (+ $p2 2))))
    (let ($p1 $p2)
      (cond
       ((eq WrapMethod 'line)
        (setq $p1 (line-beginning-position) $p2 (line-end-position))
        (goto-char $p2)
        (insert RBracket)
        (goto-char $p1)
        (insert LBracket)
        (goto-char (+ $p2 (length LBracket))))
       ((eq WrapMethod 'block)
        (save-excursion
          (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
          (goto-char $p2)
          (insert RBracket)
          (goto-char $p1)
          (insert LBracket)
          (goto-char (+ $p2 (length LBracket)))))
       ( ;  do line. line must contain space
        (and
         (eq (point) (line-beginning-position))
         ;; (string-match " " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (not (eq (line-beginning-position) (line-end-position))))
        (insert LBracket )
        (end-of-line)
        (insert  RBracket))
       ((and
         (or ; cursor is at end of word or buffer. i.e. xyz▮
          (looking-at "[^-_[:alnum:]]")
          (eq (point) (point-max)))
         (not (or
               (string-equal major-mode "xah-elisp-mode")
               (string-equal major-mode "emacs-lisp-mode")
               (string-equal major-mode "lisp-mode")
               (string-equal major-mode "lisp-interaction-mode")
               (string-equal major-mode "common-lisp-mode")
               (string-equal major-mode "clojure-mode")
               (string-equal major-mode "xah-clojure-mode")
               (string-equal major-mode "scheme-mode"))))
        (progn
          (setq $p1 (point) $p2 (point))
          (insert LBracket RBracket)
          (search-backward RBracket )))
       (t (progn
            ;; wrap around “word”. basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese chars
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq $p1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq $p2 (point))
            (goto-char $p2)
            (insert RBracket)
            (goto-char $p1)
            (insert LBracket)
            (goto-char (+ $p2 (length LBracket)))))))))

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-square-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )

(defun xah-insert-double-curly-quote () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-curly-single-quote () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-single-angle-quote () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote () (interactive) (xah-insert-bracket-pair "« " " »") )
(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )
(defun xah-insert-corner-bracket () (interactive) (xah-insert-bracket-pair "「" "」" ) )
(defun xah-insert-white-corner-bracket () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket () (interactive) (xah-insert-bracket-pair "〔" "〕" ) )

(defun xah-insert-hyphen ()
  "Insert a HYPHEN-MINUS character."
  (interactive)
  (insert "-"))

(defun xah-insert-low-line ()
  "Insert a LOW LINE character."
  (interactive)
  (insert "_"))

(defun xah-insert-string-assignment ()
  "Insert =\"\""
  (interactive)
  (progn (insert "=\"\"")
         (left-char)))

(defun xah-insert-space-before ()
  "Insert space before cursor."
  (interactive)
  (insert " "))

(defun xah-insert-space-after ()
  "Insert space after cursor"
  (interactive)
  (insert " ")
  (left-char))

(defun xah-insert-formfeed ()
  "Insert a form feed char (codepoint 12)"
  (interactive)
  (insert "\n\u000c\n"))

(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.

URL `http://xahlee.info/emacs/emacs/emacs_form_feed_section_paging.html'
Version: 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))

(defun xah-insert-column-az ()
  "Insert letters A to Z vertically, similar to `rectangle-number-lines'.
The commpand will prompt for a start char, and number of chars to insert.
The start char can be any char in Unicode.

URL `http://xahlee.info/emacs/emacs/emacs_insert-alphabets.html'
Version: 2019-03-07"
  (interactive)
  (let (
        ($startChar (string-to-char (read-string "Start char: " "a")))
        ($howmany (string-to-number (read-string "How many: " "26")))
        ($colpos (- (point) (line-beginning-position))))
    (dotimes ($i $howmany )
      (progn
        (insert-char (+ $i $startChar))
        (forward-line)
        (beginning-of-line)
        (forward-char $colpos)))))

(defvar xah-unicode-list nil "Associative list of Unicode symbols. First element is a Unicode character, second element is a string used as key shortcut in `ido-completing-read'")
(setq xah-unicode-list
      '(
        ;; format: (str . nameOrFastKey)
        ("_" . "underscore" )
        ("•" . ".bullet" )
        ("→" . "tn")
        ("◇" . "3" )
        ("◆" . "4" )
        ("¤" . "2" )
        ("…" . "...ellipsis" )
        (" " . "nbsp" )
        ("、" . "," )
        ("⭑" . "9" )
        ("🎶" . "5" )
        ("—" . "-emdash" )
        ("＆" . "7fullwidthAmpersand" )
        ("↓" . "downArrow")
        ("←" . "leftArrow")
        ("↑" . "upArrow")
        ("👍" . "thumbUp")
        ("〚〛" . "whiteSquareBracket")
        ) )

(defun xah-insert-unicode ()
  "Insert a unicode from a custom list `xah-unicode-list'.
Version: 2021-01-05"
  (interactive)
  (let (
        ($str
         (ido-completing-read
          "Insert:" (mapcar
                     (lambda (x)
                       (format "%s %s" (car x) (cdr x))) xah-unicode-list))))
    (insert (car (split-string $str " " t)))))

;; HHH___________________________________________________________________
;; text selection

(defun xah-select-block ()
  "Select the current/next block plus 1 blankline.
If region is active, extend selection downward by block.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2019-12-26 2021-04-04 2021-08-13"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
        (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1))))

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2017-11-01 2021-03-19"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
          (let (($p1 (point)))
                (end-of-visual-line 1)
                (when (eq $p1 (point))
                  (end-of-visual-line 2)))
        (progn
          (forward-line 1)
          (end-of-line)))
    (if visual-line-mode
        (progn (beginning-of-visual-line)
               (set-mark (point))
               (end-of-visual-line))
      (progn
        (end-of-line)
        (set-mark (line-beginning-position))))))

(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when there is no selection,
• if cursor is on a any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.

when there is a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-02-04"
  (interactive)
  (if (region-active-p)
      (progn
        (let (($rb (region-beginning)) ($re (region-end)))
          (goto-char $rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq $rb (line-beginning-position))
            (progn
              (goto-char $rb)
              (let (($firstLineEndPos (line-end-position)))
                (cond
                 ((eq $re $firstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< $re $firstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> $re $firstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char $re)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char $re)
                        (end-of-line)))))
                 (t (error "logic error 42946" ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        ;; (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-syntax-forward "_w")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        ;; (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark))
       ;;
       ))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: '\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command select between any bracket chars, not the inner text of a bracket. For example, if text is

 (a(b)c▮)

 the selected char is “c”, not “a(b)c”.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-11-24 2021-07-11 2021-12-21"
  (interactive)
  (let (
        ($skipChars "^'\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
        $p1
        )
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))

;; HHH___________________________________________________________________
;; misc

(defun xah-user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it is not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
Version: 2016-06-18"
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   (t t)))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-p'.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-p))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-p'.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-p))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
“emacs buffer” here is buffer whose name starts with *.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
“emacs buffer” here is buffer whose name starts with *.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version: 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(defvar xah-recently-closed-buffers nil "a Alist of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")

(defcustom xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'."
  :type 'integer
  :group 'xah-fly-keys)

(declare-function minibuffer-keyboard-quit "delsel" ())
(declare-function org-edit-src-save "org-src" ())

(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• If the buffer is editing a source file in an `org-mode' file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2018-06-11 2021-07-01"
  (interactive)
  (let (($isOrgMode (string-match "^*Org Src" (buffer-name))))
    (if (active-minibuffer-window) ; if the buffer is minibuffer
        ;; (string-equal major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit)
      (progn
        ;; Offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (Because `kill-buffer' does not offer to save buffers that are not associated with files.)
        (when (and (buffer-modified-p)
                   (xah-user-buffer-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   $isOrgMode)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))
        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))
        (kill-buffer (current-buffer))))))

(defun xah-open-last-closed ()
  "Open the last closed file.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19"
  (interactive)
  (if (> (length xah-recently-closed-buffers) 0)
      (find-file (cdr (pop xah-recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(defun xah-open-recently-closed ()
  "Open recently closed file.
Prompt for a choice.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19 2021-10-27"
  (interactive)
  (find-file (ido-completing-read "Open:" (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers))))

(defun xah-list-recently-closed ()
  "List recently closed file.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19"
  (interactive)
  (let (($buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer $buf)
    (mapc (lambda ($f) (insert (cdr $f) "\n"))
          xah-recently-closed-buffers)))

(declare-function bookmark-maybe-load-default-file "bookmark" ())
(defvar bookmark-alist)
(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))

(defun xah-open-file-fast ()
  "Prompt to open a file from bookmark `bookmark-bmenu-list'.
This command is similar to `bookmark-jump', but use `ido-mode' interface, and ignore cursor position in bookmark.

URL `http://xahlee.info/emacs/emacs/emacs_hotkey_open_file_fast.html'
Version: 2019-02-26"
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let (($thisBookmark (ido-completing-read "Open bookmark:" (mapcar (lambda ($x) (car $x)) bookmark-alist))))
    (find-file (bookmark-get-filename $thisBookmark))))

(defvar xah-open-file-at-cursor-pre-hook nil "Hook for `xah-open-file-at-cursor'. Functions in the hook will be called in order, each given the path as arg. The first return non-nil, its value is given to `xah-open-file-at-cursor' as input. This is useful for transforming certain url into file path (your website url), so instead of opening in browser, it opens in emacs as file.")

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is selection, use it for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.

See also `xah-open-file-at-cursor-pre-hook'.

This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Version: 2020-10-17 2021-02-24 2021-08-14 2021-09-19 2021-10-16"
  (interactive)
  (let* (($input
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let (($p0 (point)) $p1 $p2
                  ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         $input2 $path
         )
    (setq $input2
          (if (> (length xah-open-file-at-cursor-pre-hook) 0)
              (let (($x (run-hook-with-args-until-success 'xah-open-file-at-cursor-pre-hook $input)))
                (if $x $x $input))
            $input))
    (setq $path (replace-regexp-in-string "^/C:/" "/" (replace-regexp-in-string "^file://" "" (replace-regexp-in-string ":\\'" "" $input2))))
    (if (string-match-p "\\`https?://" $path)
        (browse-url $path)
      (progn ; not starting “http://”
        (if (string-match "#" $path)
            (let (($fpath (substring $path 0 (match-beginning 0)))
                  ($fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char (point-min))
                    (search-forward $fractPart))
                (when (y-or-n-p (format "file does not exist: [%s]. Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (($fpath (match-string-no-properties 1 $path))
                    ($lineNum (string-to-number (match-string-no-properties 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char (point-min))
                      (forward-line (1- $lineNum)))
                  (when (y-or-n-p (format "file does not exist: [%s]. Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file does not exist: [%s]. Create?" $path))
                  (find-file $path))))))))))

(if (version<= emacs-version "26.0.50")
    (defalias 'xah-display-line-numbers-mode #'linum-mode)
  (defalias 'xah-display-line-numbers-mode #'global-display-line-numbers-mode))

(defvar xah-fly-M-x-command nil "Command to call for emacs `execute-extended-command' replacement, used by `xah-fly-M-x'. Value should be a lisp symbol.")

(setq xah-fly-M-x-command nil)

(defun xah-fly-M-x ()
  "Calls `execute-extended-command' or an alternative.
If `xah-fly-M-x-command' is non-nil, call it, else call one of the following, in order: `smex', `helm-M-x', `counsel-M-x', `execute-extended-command'.
Version: 2020-04-09 2021-02-24"
  (interactive)
  (command-execute
   (cond
    ((and (boundp 'xah-fly-M-x-command) xah-fly-M-x-command) xah-fly-M-x-command )
    ((fboundp 'smex) 'smex)
    ((fboundp 'helm-M-x) 'helm-M-x)
    ((fboundp 'counsel-M-x) 'counsel-M-x)
    (t 'execute-extended-command))
   nil
   nil
   :special))

;; HHH___________________________________________________________________

(defvar xah-run-current-file-before-hook nil "Hook for `xah-run-current-file'. Before the file is run.")

(defvar xah-run-current-file-after-hook nil "Hook for `xah-run-current-file'. After the file is run.")

(defun xah-run-current-go-file ()
  "Run or build current golang file.
To build, call `universal-argument' first.
Version: 2018-10-12"
  (interactive)
  (when (not (buffer-file-name)) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let* (
         ($outputb "*xah-run output*")
         ;; (resize-mini-windows nil)
         ($fname (buffer-file-name))
         ;; ($fSuffix (file-name-extension $fname))
         ($progName "go")
         $cmdStr)
    (setq $cmdStr (concat $progName " \""   $fname "\" &"))
    (if current-prefix-arg
        (progn
          (setq $cmdStr (format "%s build \"%s\" " $progName $fname)))
      (progn
        (setq $cmdStr (format "%s run \"%s\" &" $progName $fname))))
    (progn
      (message "running %s" $fname)
      (message "%s" $cmdStr)
      (shell-command $cmdStr $outputb )
      ;;
      )))

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*xah-run output*”.

The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, golang, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2020-03-05"
  (interactive)
  (let (
        ($outBuffer "*xah-run output*")
        (resize-mini-windows nil)
        ($suffixMap
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("hs" . "runhaskell")
           ("js" . "node")
           ("mjs" . "node --experimental-modules ")
           ("ts" . "tsc") ; TypeScript
           ("tsx" . "tsc")
           ("sh" . "bash")
           ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))
        $fname
        $fSuffix
        $progName
        $cmdStr)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fSuffix (file-name-extension $fname))
    (setq $progName (cdr (assoc $fSuffix $suffixMap)))
    (setq $cmdStr (concat $progName " \""   $fname "\" &"))
    (run-hooks 'xah-run-current-file-before-hook)
    (cond
     ((string-equal $fSuffix "el")
      (load $fname))
     ((or (string-equal $fSuffix "ts") (string-equal $fSuffix "tsx"))
      (if (fboundp 'xah-ts-compile-file)
          (progn
            (xah-ts-compile-file current-prefix-arg))
        (if $progName
            (progn
              (message "Running")
              (shell-command $cmdStr $outBuffer ))
          (error "No recognized program file suffix for this file."))))
     ((string-equal $fSuffix "go")
      (xah-run-current-go-file))
     ((string-equal $fSuffix "java")
      (progn
        (shell-command (format "javac %s" $fname) $outBuffer )
        (shell-command (format "java %s" (file-name-sans-extension
                                          (file-name-nondirectory $fname))) $outBuffer )))
     (t (if $progName
            (progn
              (message "Running")
              (shell-command $cmdStr $outBuffer ))
          (error "No recognized program file suffix for this file."))))
    (run-hooks 'xah-run-current-file-after-hook)))

(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1, in whole buffer or selection.
Respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version: 2017-09-22 2020-09-08"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil 1)
            (replace-match "\n\n")))))))

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or selection, respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version: 2017-09-22 2021-08-27"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil 1) (replace-match "\n"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil 1) (replace-match "\n\n"))
        (goto-char (point-max))
        (while (eq (char-before) ? ) (delete-char -1))))))

(defun xah-make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is in this format
 x.html~2018-05-15_133429~
 The last part is hour, minutes, seconds.
in the same dir. If such a file already exist, it is overwritten.
If the current buffer is not associated with a file, nothing's done.

URL `http://xahlee.info/emacs/emacs/elisp_make-backup.html'
Version: 2018-06-06 2020-12-18"
  (interactive)
  (let (($fname (buffer-file-name))
        ($dateTimeFormat "%Y%m%d_%H%M%S"))
    (if $fname
        (let (($backupName
               (concat $fname "~" (format-time-string $dateTimeFormat) "~")))
          (copy-file $fname $backupName t)
          (message (concat "Backup saved at: " $backupName)))
      (if (eq major-mode 'dired-mode)
          (progn
            (mapc (lambda ($x)
                    (let (($backupName
                           (concat $x "~" (format-time-string $dateTimeFormat) "~")))
                      (copy-file $x $backupName t)))
                  (dired-get-marked-files))
            (revert-buffer))
        (user-error "buffer not file nor dired")))))

(defun xah-make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `xah-make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done.

URL `http://xahlee.info/emacs/emacs/elisp_make-backup.html'
Version: 2015-10-14"
  (interactive)
  (if (buffer-file-name)
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))

(defun xah-delete-current-file-make-backup ()
  "Delete current file, makes a backup~, close the buffer.
If buffer is not a file, copy content to `kill-ring', delete buffer.

Backup filename is “‹name›~‹dateTimeStamp›~”. Existing file of the same name is overwritten. If buffer is not a file, the backup file name starts with “xx_”.

Call `xah-open-last-closed' to open the backup file.

URL `http://xahlee.info/emacs/emacs/elisp_delete-current-file.html'
Version: 2018-05-15 2021-08-31 2021-09-27"
  (interactive)
  (if (string-equal 'dired-mode major-mode)
      (message "In dired. Nothing is done.")
    (let* (($fname (buffer-file-name))
           ($backupPath
            (concat (if $fname $fname (format "%sxx" default-directory))
                    (format "~%s~" (format-time-string "%Y%m%dT%H%M%S")))))
      (if $fname
          (progn
            (save-buffer $fname)
            (copy-file $fname $backupPath t)
            (when (boundp 'xah-recently-closed-buffers)
              (push (cons nil $backupPath) xah-recently-closed-buffers))
            (message "Deleted. Backup at [%s]. Call `xah-open-last-closed' to open." $backupPath)
            (delete-file $fname))
        (progn
          (widen)
          (kill-new  (buffer-string))))
      (kill-buffer (current-buffer)))))

;; HHH___________________________________________________________________

(defun xah-search-current-word ()
  "Call `isearch' on current word or selection.
“word” here is A to Z, a to z, and hyphen [-] and lowline [_], independent of syntax table.

URL `http://xahlee.info/emacs/emacs/modernization_isearch.html'
Version: 2015-04-09"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))
    (setq mark-active nil)
    (when (< $p1 (point))
      (goto-char $p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

(declare-function w32-shell-execute "w32fns.c" (operation document &optional parameters show-flag)) ; (w32-shell-execute "open" default-directory)

(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, File Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2020-11-20 2021-01-31 2021-11-12 2021-11-22"
  (interactive)
  (let (($path (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if (buffer-file-name) (buffer-file-name) default-directory))))
    (cond
     ((string-equal system-type "windows-nt")
      ;; (shell-command (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory )))
      (let (($cmd (format "Explorer /select,%s"
                          (replace-regexp-in-string "/" "\\" $path t t)
                          ;; (shell-quote-argument (replace-regexp-in-string "/" "\\" $path t t ))
                          )))
        (shell-command $cmd)))
     ((string-equal system-type "darwin")
      (shell-command
       (concat "open -R " (shell-quote-argument $path))))
     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil)
            (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                 "/usr/bin/gvfs-open"
                               "/usr/bin/xdg-open")))
        (start-process "" nil openFileProgram (shell-quote-argument $path)))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      ))))

(defun xah-open-in-vscode ()
  "Open current file or dir in vscode.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2020-02-13 2021-01-18"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) (expand-file-name default-directory ))))
    (message "path is %s" $path)
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a Visual\\ Studio\\ Code.app %s" (shell-quote-argument $path))))
     ((string-equal system-type "windows-nt")
      ;; 2021-01-18 problem: if gnu findutils is installed, it installs a code.exe program, same name as vscode's executable. and usually in path before vscode.
;; vs code is usually at home dir
;; "C:\Users\joe\AppData\Local\Programs\Microsoft VS Code\bin\code.cmd"
      ;; the following is attemp to work around
      ;; (shell-command
      ;;  (format
      ;;   "PowerShell -Command Invoke-Expression \"%s\\%s\" %s"
      ;;   (getenv "HOMEPATH")
      ;;   "AppData\\Local\\Programs\\Microsoft VS Code\\Code.exe"
      ;;   (shell-quote-argument $path)))
      ;; (shell-command (concat "PowerShell -Command Start-Process Code.exe -filepath " (shell-quote-argument $path)))
      (shell-command (format "Code %s" (shell-quote-argument $path)))
      ;;
      )
     ((string-equal system-type "gnu/linux")
      (shell-command (format "code %s" (shell-quote-argument $path)))))))

(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2021-07-21"
  (interactive)
  (let ($fileList $doIt )
    (setq $fileList
          (if Fname
              (list Fname)
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
    (setq $doIt (if (<= (length $fileList) 5) t (y-or-n-p "Open more than 5 files? ")))
    (when $doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $fileList))
       ((string-equal system-type "darwin")
        (mapc (lambda ($fpath) (shell-command (concat "open " (shell-quote-argument $fpath)))) $fileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda ($fpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" $fpath))) $fileList))
       ((string-equal system-type "berkeley-unix")
        (mapc (lambda ($fpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" $fpath))) $fileList))))))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
On Microsoft Windows, it starts cross-platform PowerShell pwsh. You need to have it installed.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2020-11-21 2021-07-21"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (let ((process-connection-type nil)
          ($cmdstr
           (format "pwsh -Command Start-Process pwsh -WorkingDirectory %s" (shell-quote-argument default-directory))))
      ;; (start-process "" nil "powershell" "Start-Process" "powershell"  "-WorkingDirectory" default-directory)
      (shell-command $cmdstr)))
   ((string-equal system-type "darwin")
    (shell-command (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory )))))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))
   ((string-equal system-type "berkeley-unix")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))))

(defun xah-next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame.
If `universal-argument' is called first, do switch frame.
Version: 2017-01-27"
  (interactive)
  (if current-prefix-arg
      (other-frame 1)
    (if (one-window-p)
        (other-frame 1)
      (other-window 1))))

(defun xah-unsplit-window-or-next-frame ()
  "Unsplit window. If current frame has only one window, switch to next frame.
Version: 2017-01-29"
  (interactive)
  (if (one-window-p)
      (other-frame 1)
    (delete-other-windows)))

(defun xah-describe-major-mode ()
  "Display inline doc for current `major-mode'."
  (interactive)
  (describe-function major-mode))


;; key maps for conversion

(defvar xah--dvorak-to-azerty-kmap
  '(("." . "e")
    ("," . "z")
    ("'" . "a")
    (";" . "w")
    ("/" . "^") ; NOTE: this is a dead key
    ("[" . ")")
    ("]" . "=")
    ("=" . "$")
    ("-" . "ù")
    ("a" . "q")
    ("b" . "n")
    ("c" . "i")
    ("d" . "h")
    ("e" . "d")
    ("f" . "y")
    ("g" . "u")
    ("h" . "j")
    ("i" . "g")
    ("j" . "c")
    ("k" . "v")
    ("l" . "p")
    ("m" . ",")
    ("n" . "l")
    ("o" . "s")
    ("p" . "r")
    ("q" . "x")
    ("r" . "o")
    ("s" . "m")
    ("t" . "k")
    ("u" . "f")
    ("v" . ":")
    ("w" . ";")
    ("x" . "b")
    ("y" . "t")
    ("z" . "!")
    ("1" . "&")
    ("2" . "é")
    ("3" . "\"")
    ("4" . "'")
    ("5" . "(")
    ("6" . "-")
    ("7" . "è")
    ("8" . "_")
    ("9" . "ç")
    ("0" . "à")
    ("\\" . "*")
    ("`" . "²"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding AZERTY. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-azerty-be-kmap
  '(("." . "e")
    ("," . "z")
    ("'" . "a")
    (";" . "w")
    ("/" . "^") ; NOTE: this is a dead key
    ("[" . ")")
    ("]" . "-")
    ("=" . "$")
    ("-" . "ù")
    ("a" . "q")
    ("b" . "n")
    ("c" . "i")
    ("d" . "h")
    ("e" . "d")
    ("f" . "y")
    ("g" . "u")
    ("h" . "j")
    ("i" . "g")
    ("j" . "c")
    ("k" . "v")
    ("l" . "p")
    ("m" . ",")
    ("n" . "l")
    ("o" . "s")
    ("p" . "r")
    ("q" . "x")
    ("r" . "o")
    ("s" . "m")
    ("t" . "k")
    ("u" . "f")
    ("v" . ":")
    ("w" . ";")
    ("x" . "b")
    ("y" . "t")
    ("z" . "=")
    ("1" . "&")
    ("2" . "é")
    ("3" . "\"")
    ("4" . "'")
    ("5" . "(")
    ("6" . "§")
    ("7" . "è")
    ("8" . "!")
    ("9" . "ç")
    ("0" . "à")
    ("\\" . "µ")
    ("`" . "²"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding AZERTY-BE. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-colemak-kmap
  '(("'" . "q")
    ("," . "w")
    ("." . "f")
    ("p" . "p")
    ("y" . "g")
    ("f" . "j")
    ("g" . "l")
    ("c" . "u")
    ("r" . "y")
    ("l" . ";")
    ("a" . "a")
    ("o" . "r")
    ("e" . "s")
    ("u" . "t")
    ("i" . "d")
    ("d" . "h")
    ("h" . "n")
    ("t" . "e")
    ("n" . "i")
    ("s" . "o")
    (";" . "z")
    ("q" . "x")
    ("j" . "c")
    ("k" . "v")
    ("x" . "b")
    ("b" . "k")
    ("m" . "m")
    ("w" . ",")
    ("v" . ".")
    ("z" . "/"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding Colemak layout. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-colemak-mod-dh-kmap
  '(("'" . "q")
    ("," . "w")
    ("." . "f")
    ("p" . "p")
    ("y" . "b")
    ("f" . "j")
    ("g" . "l")
    ("c" . "u")
    ("r" . "y")
    ("l" . ";")
    ("a" . "a")
    ("o" . "r")
    ("e" . "s")
    ("u" . "t")
    ("i" . "g")
    ("d" . "k")
    ("h" . "n")
    ("t" . "e")
    ("n" . "i")
    ("s" . "o")
    (";" . "z")
    ("q" . "x")
    ("j" . "c")
    ("k" . "d")
    ("x" . "v")
    ("b" . "m")
    ("m" . "h")
    ("w" . ",")
    ("v" . ".")
    ("z" . "/"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding Colemak Mod-DH layout. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-dvorak-kmap
  '()
  "A alist, dvorak to dvorak.")

(defvar xah--dvorak-to-programer-dvorak-kmap
  '(
    ;; number row
    ("`" . "$")
    ("1" . "&")
    ("2" . "[")
    ("3" . "{")
    ("4" . "}")
    ("5" . "(")
    ("6" . "=")
    ("7" . "*")
    ("8" . ")")
    ("9" . "+")
    ("0" . "]")
    ("[" . "!")
    ("]" . "#")
    ;; number row, shifted
    ("!" . "%")
    ("@" . "7")
    ("#" . "5")
    ("$" . "3")
    ("%" . "1")
    ("^" . "9")
    ("&" . "0")
    ("*" . "2")
    ("(" . "4")
    (")" . "6")
    ("{" . "8")
    ("}" . "`")
    ;; left pinky outwards
    ("'" . ";")
    ("\"" . ":")
    ;; left pinky inwards
    (";" . "'")
    (":" . "\"")
    ;; right pinky outwards-sideways
    ("=" . "@")
    ("+" . "^")
    )
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding Programer Dvorak layout. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-qwerty-kmap
  '(("." . "e")
    ("," . "w")
    ("'" . "q")
    (";" . "z")
    ("/" . "[")
    ("[" . "-")
    ("]" . "=")
    ("=" . "]")
    ("-" . "'")
    ("a" . "a")
    ("b" . "n")
    ("c" . "i")
    ("d" . "h")
    ("e" . "d")
    ("f" . "y")
    ("g" . "u")
    ("h" . "j")
    ("i" . "g")
    ("j" . "c")
    ("k" . "v")
    ("l" . "p")
    ("m" . "m")
    ("n" . "l")
    ("o" . "s")
    ("p" . "r")
    ("q" . "x")
    ("r" . "o")
    ("s" . ";")
    ("t" . "k")
    ("u" . "f")
    ("v" . ".")
    ("w" . ",")
    ("x" . "b")
    ("y" . "t")
    ("z" . "/"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding QWERTY. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-qwerty-no-kmap
  '(("." . "e")
    ("," . "w")
    ("'" . "q")
    (";" . "z")
    ("/" . "å")
    ("[" . "+")
    ("]" . "´") ; NOTE: this is a dead key
    ("=" . "¨") ; NOTE: this is a dead key
    ("-" . "æ")
    ("a" . "a")
    ("b" . "n")
    ("c" . "i")
    ("d" . "h")
    ("e" . "d")
    ("f" . "y")
    ("g" . "u")
    ("h" . "j")
    ("i" . "g")
    ("j" . "c")
    ("k" . "v")
    ("l" . "p")
    ("m" . "m")
    ("n" . "l")
    ("o" . "s")
    ("p" . "r")
    ("q" . "x")
    ("r" . "o")
    ("s" . "ø")
    ("t" . "k")
    ("u" . "f")
    ("v" . ".")
    ("w" . ",")
    ("x" . "b")
    ("y" . "t")
    ("z" . "-"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding QWERTY-NO. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-qwerty-abnt-kmap
  '(("." . "e")
    ("," . "w")
    ("'" . "q")
    (";" . "z")
    ("/" . "'")
    ("[" . "-")
    ("]" . "=")
    ("=" . "[")
    ("-" . "~")
    ("a" . "a")
    ("b" . "n")
    ("c" . "i")
    ("d" . "h")
    ("e" . "d")
    ("f" . "y")
    ("g" . "u")
    ("h" . "j")
    ("i" . "g")
    ("j" . "c")
    ("k" . "v")
    ("l" . "p")
    ("m" . "m")
    ("n" . "l")
    ("o" . "s")
    ("p" . "r")
    ("q" . "x")
    ("r" . "o")
    ("s" . "ç")
    ("t" . "k")
    ("u" . "f")
    ("v" . ".")
    ("w" . ",")
    ("x" . "b")
    ("y" . "t")
    ("z" . ";"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding ABNT. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-qwertz-kmap
  '(("." . "e")
    ("," . "w")
    ("'" . "q")
    (";" . "y")
    ("/" . "ü")
    ("[" . "ß")
    ("]" . "´")
    ("=" . "+")
    ("-" . "ä")
    ("a" . "a")
    ("b" . "n")
    ("c" . "i")
    ("d" . "h")
    ("e" . "d")
    ("f" . "z")
    ("g" . "u")
    ("h" . "j")
    ("i" . "g")
    ("j" . "c")
    ("k" . "v")
    ("l" . "p")
    ("m" . "m")
    ("n" . "l")
    ("o" . "s")
    ("p" . "r")
    ("q" . "x")
    ("r" . "o")
    ("s" . "ö")
    ("t" . "k")
    ("u" . "f")
    ("v" . ".")
    ("w" . ",")
    ("x" . "b")
    ("y" . "t")
    ("z" . "-"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding QWERTZ. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-workman-kmap
  '(("'" . "q")
    ("," . "d")
    ("." . "r")
    ("p" . "w")
    ("y" . "b")
    ("f" . "j")
    ("g" . "f")
    ("c" . "u")
    ("r" . "p")
    ("l" . ";")
    ("a" . "a")
    ("o" . "s")
    ("e" . "h")
    ("u" . "t")
    ("i" . "g")
    ("d" . "y")
    ("h" . "n")
    ("t" . "e")
    ("n" . "o")
    ("s" . "i")
    (";" . "z")
    ("q" . "x")
    ("j" . "m")
    ("k" . "c")
    ("x" . "v")
    ("b" . "k")
    ("m" . "l")
    ("w" . ",")
    ("v" . ".")
    ("z" . "/"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding Workman layout. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(defvar xah--dvorak-to-norman-kmap
  '(
    ("'" . "q")
    ("," . "w")
    ("." . "d")
    ("p" . "f")
    ("y" . "k")

    ("f" . "j")
    ("g" . "u")
    ("c" . "r")
    ("r" . "l")
    ("l" . ";")

    ("a" . "a")
    ("o" . "s")
    ("e" . "e")
    ("u" . "t")
    ("i" . "g")

    ("d" . "y")
    ("h" . "n")
    ("t" . "i")
    ("n" . "o")
    ("s" . "h")

    (";" . "z")
    ("q" . "x")
    ("j" . "c")
    ("k" . "v")
    ("x" . "b")

    ("b" . "p")
    ("m" . "m")
    ("w" . ",")
    ("v" . ".")
    ("z" . "/"))
  "A alist, each element is of the form(\"e\" . \"d\"). First char is Dvorak, second is corresponding Norman layout. Not all chars are in the list, such as digits. When not in this alist, they are assumed to be the same.")

(define-obsolete-variable-alias 'xah-fly-key--current-layout 'xah-fly-key-current-layout "2020-04-09")
(defcustom xah-fly-key-current-layout nil
  "The current keyboard layout. Use `xah-fly-keys-set-layout' to set the layout.
If the value is nil, it's automatically set to \"dvorak\"."
  :type '(choice  (const :tag "AZERTY" azerty)
                  (const :tag "Belgian AZERTY" azerty-be)
                  (const :tag "Colemak" colemak)
                  (const :tag "Colemak Mod-DH" colemak-mod-dh)
                  (const :tag "Dvorak" dvorak)
                  (const :tag "Programmer Dvorak" programer-dvorak)
                  (const :tag "QWERTY" qwerty)
                  (const :tag "Portuguese QWERTY (ABNT)" qwerty-abnt)
                  (const :tag "QWERTY Norwegian" qwerty-no)
                  (const :tag "QWERTZ" qwertz)
                  (const :tag "Workman" workman)
                  (const :tag "Norman" norman))
  :group 'xah-fly-keys
  :set (lambda (@layout-var @new-layout)
         ;; Only reload xah-fly-keys if it was already loaded and the new layout is different:
         (if (and (featurep 'xah-fly-keys)
                  (not (eq @new-layout (symbol-value @layout-var))))
             (progn
               (set @layout-var @new-layout)
               (load "xah-fly-keys"))
           (set @layout-var @new-layout))))
(if xah-fly-key-current-layout nil (setq xah-fly-key-current-layout 'dvorak))

(defvar xah-fly--current-layout-kmap nil
  "The current keyboard layout key map. Value is a alist. e.g. the value of `xah--dvorak-to-qwerty-kmap'.
Value is automatically set from value of `xah-fly-key-current-layout'. Do not manually set this variable. Version 2019-02-12."
  )
(setq xah-fly--current-layout-kmap
      (symbol-value
       (intern
        (concat "xah--dvorak-to-"
                (if (symbolp xah-fly-key-current-layout)
                    (symbol-name xah-fly-key-current-layout)
                  xah-fly-key-current-layout)
                "-kmap"))))

(defun xah-fly--key-char (@charstr)
  "Return the corresponding char @charstr according to `xah-fly--current-layout-kmap'.
@charstr must be a string of single char. If more than 1 char, return it unchanged.
Version 2020-04-18"
  (interactive)
  (if (> (length @charstr) 1)
      @charstr
    (let (($result (assoc @charstr xah-fly--current-layout-kmap)))
      (if $result (cdr $result) @charstr ))))

(defmacro xah-fly--define-keys (@keymap-name @key-cmd-alist &optional @direct-q)
  "Map `define-key' over a alist @key-cmd-alist, with key layout remap.
The key is remapped by `xah-fly--key-char'.
If @direct-q is t, do not remap key.
Example usage:
;; (xah-fly--define-keys
;;  (define-prefix-command 'xah-fly-dot-keymap)
;;  '(
;;    (\"h\" . highlight-symbol-at-point)
;;    (\".\" . isearch-forward-symbol-at-point)
;;    (\"1\" . hi-lock-find-patterns)
;;    (\"w\" . isearch-forward-word)))
Version 2020-04-18"
  (let (($keymap-name (make-symbol "keymap-name")))
    `(let ((,$keymap-name , @keymap-name))
       ,@(mapcar
          (lambda ($pair)
            `(define-key
               ,$keymap-name
               (kbd (,(if @direct-q #'identity #'xah-fly--key-char) ,(car $pair)))
               ,(list 'quote (cdr $pair))))
          (cadr @key-cmd-alist)))))


;; keymaps

;; (defvar xah-fly-swapped-1-8-and-2-7-p nil "If non-nil, it means keys 1 and 8 are swapped, and 2 and 7 are swapped. See: http://xahlee.info/kbd/best_number_key_layout.html")

(defvar xah-fly-key-map (make-sparse-keymap)
  "Backward-compatibility map for `xah-fly-keys' minor mode.

Points to `xah-fly-insert-map' when `xah-fly-insert-state-q' is non-nil,
and points to `xah-fly-command-map' otherwise (which see).")
(make-obsolete-variable
 'xah-fly-key-map
 "Put bindings for command mode in `xah-fly-command-map', bindings for
insert mode in `xah-fly-insert-map' and bindings that are common to both
command and insert modes in `xah-fly-shared-map'."
 "2020-04-16")

(defvar xah-fly-shared-map (make-sparse-keymap)
  "Parent keymap of `xah-fly-command-map' and `xah-fly-insert-map'.

Define keys that are available in both command and insert modes here, like
`xah-fly-mode-toggle'")

;; (cons 'keymap xah-fly-shared-map) makes a new keymap with `xah-fly-shared-map' as its parent. See info node (elisp)Inheritance and Keymaps.
(defvar xah-fly-command-map (cons 'keymap xah-fly-shared-map)
  "Keymap that takes precedence over all other keymaps in command mode.

Inherits bindings from `xah-fly-shared-map'. In command mode, if no binding
is found in this map `xah-fly-shared-map' is checked, then if there is
still no binding, the other active keymaps are checked like normal. However,
if a key is explicitly bound to nil in this map, it will not be looked
up in `xah-fly-shared-map' and lookup will skip directly to the normally
active maps. In this way, bindings in `xah-fly-shared-map' can be disabled
by this map.

Effectively, this map takes precedence over all others when command mode
is enabled.")

(defvar xah-fly-insert-map (cons 'keymap xah-fly-shared-map)
  "Keymap for bindings that will be checked in insert mode. Active whenever
`xah-fly-keys' is non-nil.

Inherits bindings from `xah-fly-shared-map'. In insert mode, if no binding
is found in this map `xah-fly-shared-map' is checked, then if there is
still no binding, the other active keymaps are checked like normal. However,
if a key is explicitly bound to nil in this map, it will not be looked
up in `xah-fly-shared-map' and lookup will skip directly to the normally
active maps. In this way, bindings in `xah-fly-shared-map' can be disabled
by this map.

Keep in mind that this acts like a normal global minor mode map, so other
minor modes loaded later may override bindings in this map.")

(defvar xah-fly--deactivate-command-mode-func nil)


;; setting keys

(xah-fly--define-keys
 xah-fly-command-map
 '(
   ("~" . nil)
   (":" . nil)

   ("SPC" . xah-fly-leader-key-map)
   ("DEL" . xah-fly-leader-key-map)

   ("'" . xah-reformat-lines)
   ("," . xah-shrink-whitespaces)
   ("-" . xah-cycle-hyphen-lowline-space)
   ("." . backward-kill-word)
   (";" . xah-comment-dwim)
   ("/" . hippie-expand)
   ("\\" . nil)
   ("=" . nil)
   ("[" . xah-backward-punct )
   ("]" . xah-forward-punct)
   ("`" . other-frame)

   ;; ("$" . xah-forward-punct)

   ("1" . xah-extend-selection)
   ("2" . xah-select-line)
   ("3" . delete-other-windows)
   ("4" . split-window-below)
   ("5" . delete-char)
   ("6" . xah-select-block)
   ("7" . xah-select-line)
   ("8" . xah-extend-selection)
   ("9" . xah-select-text-in-quote)
   ("0" . xah-pop-local-mark-ring)

   ("a" . xah-fly-M-x)
   ("b" . swiper)
   ("c" . previous-line)
   ("d" . xah-beginning-of-line-or-block)
   ("e" . delete-backward-char)
   ("f" . undo)
   ("g" . backward-word)
   ("h" . backward-char)
   ("i" . xah-delete-current-text-block)
   ("j" . xah-copy-line-or-region)
   ("k" . xah-paste-or-paste-previous)
   ;; ("l" . xah-fly-insert-mode-activate-space-before)
   ("l" . xah-insert-space-before)
   ("m" . xah-backward-left-bracket)
   ("n" . forward-char)
   ("o" . open-line)
   ("p" . kill-word)
   ("q" . xah-cut-line-or-region)
   ("r" . forward-word)
   ("s" . xah-end-of-line-or-block)
   ("t" . next-line)
   ("u" . xah-fly-insert-mode-activate)
   ("v" . xah-forward-right-bracket)
   ("w" . xah-next-window-or-frame)
   ("x" . xah-toggle-letter-case)
   ("y" . set-mark-command)
   ("z" . xah-goto-matching-bracket)))

;; (when xah-fly-swapped-1-8-and-2-7-p
;;     (xah-fly--define-keys
;;      xah-command-key-map
;;      '(
;;        ("8" . pop-global-mark)
;;        ("7" . xah-pop-local-mark-ring)
;;        ("2" . xah-select-line)
;;        ("1" . xah-extend-selection))))


;; set control meta, etc keys

(xah-fly--define-keys
 xah-fly-shared-map
 '(("<home>" . xah-fly-command-mode-activate)
   ("<menu>" . xah-fly-command-mode-activate)
   ("<f8>" . xah-fly-command-mode-activate))
 :direct)

(when xah-fly-use-meta-key
  (xah-fly--define-keys
   xah-fly-shared-map
   '(("M-SPC" . xah-fly-command-mode-activate))
   :direct))

(when xah-fly-use-control-key
  (xah-fly--define-keys
   xah-fly-shared-map
   '(("<C-S-prior>" . xah-previous-emacs-buffer)
     ("<C-S-next>" . xah-next-emacs-buffer)

     ("<C-tab>" . xah-next-user-buffer)
     ("<C-S-tab>" . xah-previous-user-buffer)
     ("<C-S-iso-lefttab>" . xah-previous-user-buffer)

     ("C-SPC" . xah-fly-leader-key-map)

     ("<C-prior>" . xah-previous-user-buffer)
     ("<C-next>" . xah-next-user-buffer)

     ("C-9" . scroll-down-command)
     ("C-0" . scroll-up-command)

     ("C-1" . xah-next-user-buffer)
     ("C-2" . xah-previous-user-buffer)
     ("C-7" . xah-previous-user-buffer)
     ("C-8" . xah-next-user-buffer)

     ("C-5" . xah-previous-emacs-buffer)
     ("C-6" . xah-next-emacs-buffer)

     ("C-3" . previous-error)
     ("C-4" . next-error)

     ("C-a" . mark-whole-buffer)
     ("C-n" . xah-new-empty-buffer)
     ("C-S-n" . make-frame-command)
     ("C-o" . find-file)
     ("C-s" . save-buffer)
     ("C-S-s" . write-file)
     ("C-S-t" . xah-open-last-closed)
     ("C-v" . yank)
     ("C-w" . xah-close-current-buffer)
     ("C-z" . undo)

     ("C-+" . text-scale-increase)
     ("C--" . text-scale-decrease)

     ("C-d" . pop-global-mark))
   :direct)

  ;; (if xah-fly-swapped-1-8-and-2-7-p
  ;;     (xah-fly--define-keys
  ;;      xah-fly-shared-map
  ;;      '(("C-2" . xah-previous-user-buffer)
  ;;        ("C-1" . xah-next-user-buffer))
  ;;      :direct)
  ;;   (xah-fly--define-keys
  ;;    xah-fly-shared-map
  ;;    '(("C-7" . xah-previous-user-buffer)
  ;;      ("C-8" . xah-next-user-buffer))
  ;;    :direct))
  )

(when xah-fly-use-isearch-arrows
  (xah-fly--define-keys
   isearch-mode-map
   '(("<up>" . isearch-ring-retreat)
     ("<down>" . isearch-ring-advance)
     ("<left>" . isearch-repeat-backward)
     ("<right>" . isearch-repeat-forward)
     ( "C-v" . isearch-yank-kill))
   :direct)
  (xah-fly--define-keys
   minibuffer-local-isearch-map
   '(("<left>" . isearch-reverse-exit-minibuffer)
     ("<right>" . isearch-forward-exit-minibuffer))
   :direct))

;; HHH___________________________________________________________________
;; commands related to highlight
(xah-fly--define-keys
 (define-prefix-command 'xah-fly-dot-keymap)
 ;; 2019-02-22 experiment. this is now empty. so you can use this key space for all major mode custom keys or personal keys. These highlight command isn't used much in my experience
 '()
 ;; '(
 ;;   ("." . highlight-symbol-at-point)
 ;;   ("g" . unhighlight-regexp)
 ;;   ("c" . highlight-lines-matching-regexp)
 ;;   ("h" . highlight-regexp)
 ;;   ("t" . highlight-phrase)
 ;;   ("p" . isearch-forward-symbol-at-point)
 ;;   ;; ("c" . isearch-forward-symbol)
 ;;   ;; ("h" . isearch-forward-word)

 ;;   ;;
 ;;   )
 )

(xah-fly--define-keys
 (define-prefix-command 'xah-fly--tab-key-map)
 ;; This keymap I've not used. things are here experimentally.
 ;; The TAB key is not in a very good ergonomic position on average keyboards, so 【leader tab ‹somekey›】 probably should not be used much.
 ;; Currently (2018-03-13), these are commands related to completion or indent, and I basically never use any of these (except sometimes complete-symbol).
 ;; For average user, the way it is now is probably justified, because most emacs users don't use these commands.
 ;; To customize this keymap see http://xahlee.info/emacs/misc/xah-fly-keys_customization.html.
 '(
   ("TAB" . indent-for-tab-command)

   ("i" . complete-symbol)
   ("g" . indent-rigidly)
   ("r" . indent-region)
   ("s" . indent-sexp)

   ;; temp
   ("1" . abbrev-prefix-mark)
   ("2" . edit-abbrevs)
   ("3" . expand-abbrev)
   ("4" . expand-region-abbrevs)
   ("5" . unexpand-abbrev)
   ("6" . add-global-abbrev)
   ("7" . add-mode-abbrev)
   ("8" . inverse-add-global-abbrev)
   ("9" . inverse-add-mode-abbrev)
   ("0" . expand-jump-to-next-slot)
   ("=" . expand-jump-to-previous-slot)))

;; HHH___________________________________________________________________

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-c-keymap)
 '(
   ("," . xah-open-in-external-app)
   ("." . find-file)
   ("c" . bookmark-bmenu-list)
   ("e" . ibuffer)
   ("f" . xah-open-recently-closed)
   ("g" . xah-open-in-terminal)
   ("h" . recentf-open-files)
   ("i" . xah-copy-file-path)
   ("l" . bookmark-set)
   ("n" . xah-new-empty-buffer)
   ("o" . xah-show-in-desktop)
   ("p" . xah-open-last-closed)
   ("r" . xah-open-file-fast)
   ("s" . write-file)
   ("u" . xah-open-file-at-cursor)
   ("y" . xah-list-recently-closed)
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-e-keymap)
 '(
   ("RET" . insert-char)
   ("SPC" . xah-insert-unicode)

   ("W" . xah-insert-double-angle-bracket)
   ("b" . xah-insert-black-lenticular-bracket)
   ("c" . xah-insert-ascii-single-quote)
   ("d" . xah-insert-double-curly-quote)
   ("f" . xah-insert-emacs-quote)
   ("g" . xah-insert-ascii-double-quote)
   ("h" . xah-insert-brace)
   ("i" . xah-insert-curly-single-quote)
   ("l" . xah-insert-formfeed)
   ("m" . xah-insert-corner-bracket)
   ("n" . xah-insert-square-bracket)
   ("p" . xah-insert-single-angle-quote)
   ("r" . xah-insert-tortoise-shell-bracket )
   ("s" . xah-insert-string-assignment)
   ("t" . xah-insert-paren)
   ("u" . xah-insert-date)
   ("w" . xah-insert-angle-bracket)
   ("y" . xah-insert-double-angle-quote)))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-h-keymap)
 '(
   ;; ',.
   ;; ;
   ("a" . apropos-command)
   ("b" . describe-bindings)
   ("c" . describe-char)
   ("d" . apropos-documentation)
   ("e" . view-echo-area-messages)
   ("f" . describe-face)
   ("g" . info-lookup-symbol)
   ("h" . describe-function)
   ("i" . info)
   ("j" . man)
   ("k" . describe-key)
   ("l" . view-lossage)
   ("m" . xah-describe-major-mode)
   ("n" . describe-variable)
   ("o" . describe-language-environment)
   ;; p
   ;; q
   ("r" . apropos-variable)
   ("s" . describe-syntax)
   ;; t
   ("u" . elisp-index-search)
   ("v" . apropos-value)
   ;; wxy
   ("z" . describe-coding-system)))

(xah-fly--define-keys
 ;; commands here are “harmless”, they don't modify text etc. they turn on modes, change display, prompt, start shell, etc.
 (define-prefix-command 'xah-fly-n-keymap)
 '(
   ("SPC" . whitespace-mode)
   ("," . abbrev-mode)
   ("." . toggle-frame-fullscreen)
   ("'" . frame-configuration-to-register)
   (";" . window-configuration-to-register)
   ("1" . set-input-method)
   ("2" . global-hl-line-mode)
   ("4" . global-display-line-numbers-mode)
   ;; 5
   ("6" . calendar)
   ("7" . calc)
   ;; 8
   ("9" . shell-command)
   ("0" . shell-command-on-region)
   ("a" . text-scale-adjust)
   ("b" . toggle-debug-on-error)
   ("c" . toggle-case-fold-search)
   ("d" . narrow-to-page)
   ("e" . eshell)
   ;; f
   ("g" . xah-toggle-read-novel-mode)
   ("h" . widen)
   ("i" . make-frame-command)
   ("j" . flyspell-buffer)
   ("k" . menu-bar-open)
   ("l" . toggle-word-wrap)
   ;; m
   ("n" . narrow-to-region)
   ("o" . variable-pitch-mode)
   ("p" . read-only-mode)
   ;; q
   ;; 2021-08-10 xah-fly-keys, maybe add these
   ("r" . count-words)
   ("s" . count-matches)
   ("t" . narrow-to-defun)
   ("u" . shell)
   ("v" . visual-line-mode)
   ("w" . eww)
   ("x" . save-some-buffers)
   ("y" . toggle-truncate-lines)
   ("z" . abort-recursive-edit)))

(xah-fly--define-keys
 ;; kinda replacement related
 (define-prefix-command 'xah-fly-r-keymap)
 '(("SPC" . rectangle-mark-mode)
   ("," . apply-macro-to-region-lines)
   ("." . kmacro-start-macro)
   ("3" . number-to-register)
   ("4" . increment-register)
   ("c" . replace-rectangle)
   ("d" . delete-rectangle)
   ("e" . call-last-kbd-macro)
   ("g" . kill-rectangle)
   ("h" . xah-change-bracket-pairs)
   ("i" . xah-space-to-newline)
   ("j" . xah-slash-to-backslash)
   ("k" . xah-slash-to-double-backslash)
   ("l" . clear-rectangle)
   ;; m
   ("n" . rectangle-number-lines)
   ("o" . open-rectangle)
   ("p" . kmacro-end-macro)
   ("r" . yank-rectangle)
   ("u" . xah-quote-lines)
   ;; v w
   ("x" . xah-double-backslash-to-slash)
   ("y" . delete-whitespace-rectangle)
   ;; z
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-t-keymap)
 '(("SPC" . xah-clean-whitespace)
   ("TAB" . move-to-column)

   ("1" . xah-append-to-register-1)
   ("2" . xah-clear-register-1)

   ("3" . xah-copy-to-register-1)
   ("4" . xah-paste-from-register-1)

   ("8" . xah-clear-register-1)
   ("7" . xah-append-to-register-1)

   ("." . sort-lines)
   ("," . sort-numeric-fields)
   ("'" . reverse-region)
   ;; a
   ;; b
   ("c" . goto-char)
   ("d" . mark-defun)
   ("e" . list-matching-lines)
   ("f" . goto-line)
   ;; g
   ("h" . repeat-complex-command)
   ("i" . delete-non-matching-lines)
   ("j" . copy-to-register)
   ("k" . insert-register)
   ("l" . xah-escape-quotes)
   ("m" . xah-make-backup-and-save)
   ;; n
   ;; o
   ("p" . query-replace-regexp)
   ;; q
   ("r" . copy-rectangle-to-register)
   ;; s
   ("t" . repeat)
   ("u" . delete-matching-lines)
   ;; v
   ("w" . xah-next-window-or-frame)
   ;; x
   ("y" . delete-duplicate-lines)
   ;; z
   ))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-w-keymap)
 '(
   ("d" . xah-delete-current-file-make-backup)
   ("." . eval-buffer)
   ("e" . eval-defun)
   ("m" . eval-last-sexp)
   ("p" . eval-expression)
   ("u" . eval-region)
   ("q" . save-buffers-kill-terminal)
   ("w" . delete-frame)
   ("j" . xah-run-current-file)))

;; (xah-fly--define-keys
;;  (define-prefix-command 'xah-coding-system-keymap)
;;  '(
;;    ("n" . set-file-name-coding-system)
;;    ("s" . set-next-selection-coding-system)
;;    ("c" . universal-coding-system-argument)
;;    ("f" . set-buffer-file-coding-system)
;;    ("k" . set-keyboard-coding-system)
;;    ("l" . set-language-environment)
;;    ("p" . set-buffer-process-coding-system)
;;    ("r" . revert-buffer-with-coding-system)
;;    ("t" . set-terminal-coding-system)
;;    ("x" . set-selection-coding-system)))

(xah-fly--define-keys
 ;; kinda replacement related
 (define-prefix-command 'xah-fly-comma-keymap)
 '(
   ("t" . xref-find-definitions)
   ("n" . xref-pop-marker-stack)))

(xah-fly--define-keys
 (define-prefix-command 'xah-fly-leader-key-map)
 '(
   ("SPC" . xah-fly-insert-mode-activate)
   ("DEL" . xah-fly-insert-mode-activate)
   ("RET" . execute-extended-command)
   ("TAB" . xah-fly--tab-key-map)
   ("." . xah-fly-dot-keymap)
   ("'" . xah-fill-or-unfill)
   ("," . xah-fly-comma-keymap)
   ;; - / ; = [
   ("\\" . toggle-input-method)
   ;; `

   ;; 1
   ;; 2
   ("3" . delete-window)
   ("4" . split-window-right)
   ("5" . balance-windows)
   ("6" . xah-upcase-sentence)
   ;; 7
   ;; 8
   ("9" . ispell-word)
   ;; 0

   ("a" . mark-whole-buffer)
   ("b" . end-of-buffer)
   ("c" . xah-fly-c-keymap)
   ("d" . beginning-of-buffer)
   ("e" . xah-fly-e-keymap)
   ("f" . xah-search-current-word)
   ("g" . xah-close-current-buffer)
   ("h" . xah-fly-h-keymap)
   ("i" . kill-line)
   ("j" . xah-copy-all-or-region)
   ;; k
   ("l" . recenter-top-bottom)
   ("m" . dired-jump)
   ("n" . xah-fly-n-keymap)
   ("o" . exchange-point-and-mark)
   ("p" . query-replace)
   ("q" . xah-cut-all-or-region)
   ("r" . xah-fly-r-keymap)
   ("s" . save-buffer)
   ("t" . xah-fly-t-keymap)
   ("u" . switch-to-buffer)
   ;; v
   ("w" . xah-fly-w-keymap)
   ("x" . xah-toggle-previous-letter-case)
   ("y" . xah-show-kill-ring)
   ;; z
   ;;
   ))

;; HHH___________________________________________________________________
;; Movement key integrations with built-in Emacs packages

(xah-fly--define-keys
 indent-rigidly-map
 '(("h" . indent-rigidly-left)
   ("n" . indent-rigidly-right)))

;; HHH___________________________________________________________________
;;;; misc

;; the following have keys in gnu emacs, but i decided not to give them a key, because either they are rarely used (say, 95% of emacs users usel them less than once a month ), or there is a more efficient command/workflow with key in xah-fly-keys

;; C-x r w → window-configuration-to-register
;; C-x r f → frameset-to-register

;; C-x C-p   →   mark-page
;; C-x C-l   →   downcase-region
;; C-x C-u   →   upcase-region

;; C-x C-t   →   transpose-lines
;; C-x C-o   →   delete-blank-lines

;; C-x C-r   →   find-file-read-only
;; C-x C-v   →   find-alternate-file

;; C-x =   →   what-cursor-position, use describe-char instead
;; C-x <   →   scroll-left
;; C-x >   →   scroll-right
;; C-x [   →   backward-page
;; C-x ]   →   forward-page
;; C-x ^   →   enlarge-window

;; C-x {   →   shrink-window-horizontally
;; C-x }   →   enlarge-window-horizontally
;; C-x DEL   →   backward-kill-sentence

;; C-x C-z   →   suspend-frame

;; C-x k   →   kill-buffer , use xah-close-current-buffer
;; C-x l   →   count-lines-page
;; C-x m   →   compose-mail

;; HHH___________________________________________________________________
;; undecided yet

;; C-x e   →   kmacro-end-and-call-macro
;; C-x q   →   kbd-macro-query
;; C-x C-k   →   kmacro-keymap

;; C-x C-d   →   list-directory
;; C-x C-n   →   set-goal-column
;; C-x ESC   →   Prefix Command
;; C-x $   →   set-selective-display
;; C-x *   →   calc-dispatch
;; C-x -   →   shrink-window-if-larger-than-buffer
;; C-x .   →   set-fill-prefix

;; C-x 4   →   ctl-x-4-prefix
;; C-x 5   →   ctl-x-5-prefix
;; C-x 6   →   2C-command
;; C-x ;   →   comment-set-column

;; C-x `   →   next-error
;; C-x f   →   set-fill-column
;; C-x i   →   insert-file
;; C-x n   →   Prefix Command
;; C-x r   →   Prefix Command

;; C-x C-k C-a   →   kmacro-add-counter
;; C-x C-k C-c   →   kmacro-set-counter
;; C-x C-k C-d   →   kmacro-delete-ring-head
;; C-x C-k C-e   →   kmacro-edit-macro-repeat
;; C-x C-k C-f   →   kmacro-set-format
;; C-x C-k TAB   →   kmacro-insert-counter
;; C-x C-k C-k   →   kmacro-end-or-call-macro-repeat
;; C-x C-k C-l   →   kmacro-call-ring-2nd-repeat
;; C-x C-k RET   →   kmacro-edit-macro
;; C-x C-k C-n   →   kmacro-cycle-ring-next
;; C-x C-k C-p   →   kmacro-cycle-ring-previous
;; C-x C-k C-s   →   kmacro-start-macro
;; C-x C-k C-t   →   kmacro-swap-ring
;; C-x C-k C-v   →   kmacro-view-macro-repeat
;; C-x C-k SPC   →   kmacro-step-edit-macro
;; C-x C-k b   →   kmacro-bind-to-key
;; C-x C-k e   →   edit-kbd-macro
;; C-x C-k l   →   kmacro-edit-lossage
;; C-x C-k n   →   kmacro-name-last-macro
;; C-x C-k q   →   kbd-macro-query
;; C-x C-k r   →   apply-macro-to-region-lines
;; C-x C-k s   →   kmacro-start-macro

;; HHH___________________________________________________________________

;; C-x 4 C-f   →   find-file-other-window
;; C-x 4 C-o   →   display-buffer
;; C-x 4 .   →   find-tag-other-window
;; C-x 4 0   →   kill-buffer-and-window
;; C-x 4 a   →   add-change-log-entry-other-window
;; C-x 4 b   →   switch-to-buffer-other-window
;; C-x 4 c   →   clone-indirect-buffer-other-window
;; C-x 4 d   →   dired-other-window
;; C-x 4 f   →   find-file-other-window
;; C-x 4 m   →   compose-mail-other-window
;; C-x 4 r   →   find-file-read-only-other-window

;; C-x 6 2   →   2C-two-columns
;; C-x 6 b   →   2C-associate-buffer
;; C-x 6 s   →   2C-split

;; ctl-x-5-map

;; r C-f   →   find-file-other-frame
;; r C-o   →   display-buffer-other-frame
;; r .   →   find-tag-other-frame
;; r 0   →   delete-frame
;; r 1   →   delete-other-frames
;; r 2   →   make-frame-command
;; r b   →   switch-to-buffer-other-frame
;; r d   →   dired-other-frame
;; r f   →   find-file-other-frame
;; r m   →   compose-mail-other-frame
;; r o   →   other-frame
;; r r   →   find-file-read-only-other-frame

;; (xah-fly--define-keys
;;  (define-prefix-command 'xah-leader-vc-keymap)
;;  '(
;;    ("+" . vc-update)
;;    ("=" . vc-diff)
;;    ("D" . vc-root-diff)
;;    ("L" . vc-print-root-log)
;;    ("a" . vc-update-change-log)
;;    ("b" . vc-switch-backend)
;;    ("c" . vc-rollback)
;;    ("d" . vc-dir)
;;    ("g" . vc-annotate)
;;    ("h" . vc-insert-headers)
;;    ("l" . vc-print-log)
;;    ("m" . vc-merge)
;;    ("r" . vc-retrieve-tag)
;;    ("s" . vc-create-tag)
;;    ("u" . vc-revert)
;;    ("v" . vc-next-action)
;;    ("~" . vc-revision-other-window)))

;; HHH___________________________________________________________________

(defvar xah-fly-insert-state-p t "non-nil means insertion mode is on.")

(defun xah-fly--update-key-map ()
  (setq xah-fly-key-map (if xah-fly-insert-state-p
                            xah-fly-insert-map
                          xah-fly-command-map)))

(defun xah-fly-keys-set-layout (@layout)
  "Set a keyboard layout.
Argument must be one of:

 'azerty
 'azerty-be
 'colemak
 'colemak-mod-dh
 'dvorak
 'programer-dvorak
 'qwerty
 'qwerty-abnt
 'qwerty-no (qwerty Norwegian)
 'qwertz
 'workman
 'norman

For backwards compatibility, a string that is the name of one of the above symbols is also acceptable (case-sensitive).
Version 2020-04-09"
  (interactive (list
                (widget-prompt-value (get 'xah-fly-key-current-layout 'custom-type)
                                     "New keyboard layout: ")))
  (funcall (get 'xah-fly-key-current-layout 'custom-set)
           'xah-fly-key-current-layout
           @layout))

(defun xah-fly-command-mode-init ()
  "Set command mode keys.
Version: 2020-04-28"
  (interactive)
  (setq xah-fly-insert-state-p nil)
  (xah-fly--update-key-map)
  (setq xah-fly--deactivate-command-mode-func
        (set-transient-map xah-fly-command-map (lambda () t)))
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (setq mode-line-front-space "C")
  (force-mode-line-update))

(defun xah-fly-space-key ()
  "Switch to command mode if the char before cursor is a space.
experimental
Version: 2018-05-07"
  (interactive)
  (if (eq (char-before ) 32)
      (xah-fly-command-mode-activate)
    (insert " ")))

(defun xah-fly-insert-mode-init (&optional no-indication)
  "Enter insertion mode."
  (interactive)
  (setq xah-fly-insert-state-p t)
  (xah-fly--update-key-map)
  (funcall xah-fly--deactivate-command-mode-func)
  (unless no-indication
    (modify-all-frames-parameters '((cursor-type . bar)))
    (setq mode-line-front-space "I"))
  (force-mode-line-update))

(defun xah-fly-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if xah-fly-insert-state-p
      (xah-fly-command-mode-activate)
    (xah-fly-insert-mode-activate)))

(defun xah-fly-save-buffer-if-file ()
  "Save current buffer if it is a file."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))

(defun xah-fly-command-mode-activate ()
  "Activate command mode and run `xah-fly-command-mode-activate-hook'
Version: 2017-07-07"
  (interactive)
  (xah-fly-command-mode-init)
  (run-hooks 'xah-fly-command-mode-activate-hook))

(defun xah-fly-command-mode-activate-no-hook ()
  "Activate command mode. Does not run `xah-fly-command-mode-activate-hook'
Version: 2017-07-07"
  (interactive)
  (xah-fly-command-mode-init))

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode.
Version: 2017-07-07"
  (interactive)
  (xah-fly-insert-mode-init)
  (run-hooks 'xah-fly-insert-mode-activate-hook))

(defun xah-fly-insert-mode-activate-newline ()
  "Activate insertion mode, insert newline below."
  (interactive)
  (xah-fly-insert-mode-activate)
  (open-line 1))

(defun xah-fly-insert-mode-activate-space-before ()
  "Insert a space, then activate insertion mode."
  (interactive)
  (insert " ")
  (xah-fly-insert-mode-activate))

(defun xah-fly-insert-mode-activate-space-after ()
  "Insert a space, then activate insertion mode."
  (interactive)
  (insert " ")
  (xah-fly-insert-mode-activate)
  (left-char))

;; HHH___________________________________________________________________

(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout.

URL `http://xahlee.info/emacs/misc/ergoemacs_vi_mode.html'"
  :group 'xah-fly-keys
  :global t
  :lighter " ∑flykeys"
  :keymap xah-fly-insert-map
  (if xah-fly-keys
      ;; Construction:
      (progn
        (add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
        (add-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)
        (add-hook 'isearch-mode-end-hook 'xah-fly-command-mode-activate)
        (when (and (keymapp xah-fly-key-map)
                   (not (memq xah-fly-key-map (list xah-fly-command-map
                                                    xah-fly-insert-map))))
          (set-keymap-parent xah-fly-key-map xah-fly-shared-map)
          (setq xah-fly-shared-map xah-fly-key-map))
        (xah-fly-command-mode-activate))
    (progn
      ;; Teardown:
      (remove-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
      (remove-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)
      (remove-hook 'isearch-mode-end-hook 'xah-fly-command-mode-activate)
      (remove-hook 'eshell-mode-hook 'xah-fly-insert-mode-activate)
      (remove-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)
      (xah-fly-insert-mode-init :no-indication)
      (setq mode-line-front-space '(:eval (if (display-graphic-p) " " "-")))

      ;;
      )))

(provide 'xah-fly-keys)

;;; xah-fly-keys.el ends here
