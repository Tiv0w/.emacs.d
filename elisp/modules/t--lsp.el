;;; elisp/modules/t--lsp.el -*- lexical-binding: t; -*-
;;; Commentary:
;; LSP (and DAP) setup.
;; Every mode that wants to utilize LSP should hook `lsp-deferred'.

;;; Code:

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

;; TODO: create keybindings

(use-package lsp-mode
  :defer nil
  :hook (lsp-mode . lsp-lens-mode)
  :commands (lsp lsp-deferred lsp-restart-workspace)
  :init
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
	 (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
			 (fboundp 'json-parse-buffer))
                  'json-parse-buffer
		'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
	orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :config
  (setq lsp-modeline-code-actions-segments '(count icon name)
	lsp-signature-doc-lines 3
	;; Might need some per language server tweaking
	lsp-completion-show-detail nil
	lsp-completion-show-kind nil

	;; LSP performance tuning
	read-process-output-max (* 3 1024 1024) ;; 3mb
	gc-cons-threshold 104857600 ; 100mb
	lsp-prefer-flymake nil
	;; Makes LSP shutdown the server when all project buffers are closed.
	lsp-keep-workspace-alive nil)

  ;; (add-to-list 'lsp-language-id-configuration '(web-mode . "html"))

  ;; HACK: some patch for LSP-mode, mainly for Metals
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents _server-id)
    "Extract a representative line from CONTENTS, to show in the echo area."
    (s-join
     "\n"
     (take lsp-eldoc-render-lines-count
           (seq-filter
            (lambda (line)
              (not (seq-some
                    (lambda (regexp)
                      (string-match-p regexp line))
                    lsp-eldoc-exclude-line-regexps)))
            (s-lines (s-trim (lsp--render-element contents)))))))

  (defcustom lsp-eldoc-render-lines-count 1
    "Display at most x lines of info returned by document/onHover.
If this is set to 1, `eldoc' will show only the symbol information.
Useful for LSPs that format differently their output."
    :type 'number
    :group 'lsp-mode)

  (defcustom lsp-eldoc-exclude-line-regexps '()
    "Customize the lines that should be excluded from the text returned
by document/onHover.
Useful for LSPs that format differently their output."
    :type '(repeat regexp)
    :group 'lsp-mode)


  ;; https://github.com/emacs-lsp/lsp-mode/issues/713#issuecomment-985653873
  (defun ++git-ignore-p (path)
    (let* (; trailing / breaks git check-ignore if path is a symlink:
           (path (directory-file-name path))
           (default-directory (file-name-directory path))
           (relpath (file-name-nondirectory path))
           (cmd (format "git check-ignore '%s'" relpath))
           (status (call-process-shell-command cmd)))
      (eq status 0)))

  (defun ++lsp--path-is-watchable-directory-a
      (fn path dir ignored-directories)
    (and (not (++git-ignore-p (f-join dir path)))
	 (funcall fn path dir ignored-directories)))

  (advice-add 'lsp--path-is-watchable-directory
              :around #'++lsp--path-is-watchable-directory-a)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-diagnostic-max-lines 2
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-peek-enable t
        lsp-ui-doc-max-height 20 ; default 13
        lsp-ui-doc-max-width 72 ; default 150
        lsp-ui-doc-delay 0.75   ; default 0.2
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(provide 't--lsp)
