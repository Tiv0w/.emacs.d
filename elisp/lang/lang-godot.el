;;; elisp/lang/lang-godot.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Adds support for GDscript, the scripting language of Godot (+ LSP + DAP).


;; Main gdscript package
(use-package gdscript-mode
  ;; :hook
  ;; (gdscript-mode . lsp-deferred)
  ;; (gdscript-mode . tree-sitter-hl-mode)
  :config
  (t--set-formatter 'gdformat '("gdformat" "-") :modes '(gdscript-mode))
  (add-to-list 'projectile-project-root-files "project.godot")
  :mode-hydra
  (gdscript-mode
   (:title "Godot" :color blue :quit-key "q")
   ("Run"
    (("re" gdscript-godot-open-project-in-editor "open in Godot editor")
     ("rp" gdscript-godot-run-project "run project")
     ("rd" gdscript-godot-run-project-debug "run debug")
     ("rs" gdscript-godot-run-current-scene "run current scene"))
    "Debug"
    (("da" gdscript-debug-add-breakpoint "add breakpoint")
     ("db" gdscript-debug-display-breakpoint-buffer "display breakpoint buffer")
     ("dr" gdscript-debug-remove-breakpoint "remove breakpoint")
     ("dc" gdscript-debug-continue "continue execution")
     ("dn" gdscript-debug-next "next")
     ("ds" gdscript-debug-step "step"))
    "Help"
    (("hb" gdscript-docs-browse-api "browse online API")
     ("hk" gdscript-docs-browse-symbol-at-point "browse API at point"))
    "Format"
    (("hb" gdscript-format-buffer "format buffer")
     ("hk" gdscript-format-region "format region")))))


(provide 'lang-godot)
