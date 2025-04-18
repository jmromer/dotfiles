;;; $DOOMDIR/init.el -*- lexical-binding: t; -*-
;;
;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom!
 ;;((sources)
 ;; (flags))

 :completion
 ;; (company +childframe) ; TODO: Enable instead of others?
 (corfu +dabbrev +icons)
 ;; ivy
 (vertico +icons)

 :ui
 doom
 modeline
 doom-dashboard
 hl-todo
 indent-guides
 ;; modeline   ; TODO: keep? snazzy, Atom-inspired modeline, plus API
 ophints
 (popup +all +defaults)
 treemacs
 (vc-gutter +pretty)
 ;; vi-tilde-fringe   ; TODO: drop? fringe tildes to mark beyond EOB
 workspaces

 :editor
 (evil +everywhere)
 file-templates
 fold
 format ;; TODO: +onsave?
 multiple-cursors
 rotate-text
 snippets

 :emacs
 (dired +dirvish +icons)
 electric
 ;; ibuffer ;; TODO: drop??
 undo
 vc

 :term
 ;; eshell
 vterm

 :checkers
 (syntax +childframe)
 spell ;; TODO: +flyspell?

 :tools
 ;;(debugger +lsp)
 direnv
 editorconfig
 (eval +overlay)
 (lookup +docsets +dictionary)
 lsp
 (magit +childframe +forge)
 pdf
 tree-sitter
 ;;upload

 :os
 tty

 :lang
 beancount
 cc
 ;;common-lisp
 emacs-lisp
 elixir
 (gdscript +lsp)
 (go +lsp)
 (graphql +lsp)
 (haskell +lsp)
 janet
 (java +meghanada) ;; TODO: +lsp?
 (javascript +lsp)
 json
 ;;julia
 (latex +lsp +latexmk)
 ;;(lua +fennel)
 markdown
 ;; nix
 (org +dragndrop +roam2 +appear +noter +hugo +journal)
 ;;php
 ;;plantuml
 (python +lsp +pyright +pyenv +poetry)
 rest
 (ruby +rails)
 (rust +lsp)
 (scheme +guile)
 (sh +powershell +lsp)
 (web +lsp)
 yaml
 ;;(zig +lsp)

 :email
 ;; (mu4e +mbsync +fastmail +org +icons)

 :app
 ;;everywhere
 ;;irc

 :config
 literate
 (default +bindings +smartparens +gnupg))
