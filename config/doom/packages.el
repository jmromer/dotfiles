;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
;;
;; API Demos:
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/emacs-lisp/demos.org

(disable-packages! minitest)
(disable-packages! company-gtags)
(disable-packages! evil-snipe)

(package! command-log-mode)
(package! company-box)
(package! company-lsp)
(package! dash-at-point)
(package! evil-cleverparens)
(package! evil-iedit-state)
(package! evil-lion)
(package! evil-matchit)
(package! evil-quickscope)
(package! evil-rails)
(package! evil-ruby-text-objects)
(package! evil-string-inflection)
(package! evil-unimpaired :recipe (:host github :repo "zmaas/evil-unimpaired"))
(package! impatient-mode :recipe (:host github :repo "skeeto/impatient-mode"))
(package! ggtags)
(package! gxref)
(package! org-fancy-priorities)
(package! org-journal)
(package! org-superstar)
(package! ox-hugo)
(package! posframe)
(package! ruby-factory)
(package! ruby-test-mode)
(package! seeing-is-believing)
(package! xwwp :recipe (:host github :repo "canatella/xwwp"))
(package! yankee :recipe (:host github :repo "jmromer/yankee.el" :files ("yankee.el")))

;;                           LSP language servers
;; |------------------+--------------------------------------+--------------------------------|
;; | Module           | Major modes                          | Default language server        |
;; |------------------+--------------------------------------+--------------------------------|
;; | cc               | c-mode, c++-mode, objc-mode          | ccls                           |
;; | clojure          | clojure-mode                         | clojure-lsp                    |
;; | csharp           | csharp-mode                          | omnisharp                      |
;; | elixir           | elixir-mode                          | elixir-ls                      |
;; | fsharp           | fsharp-mode                          | Mono, .NET core                |
;; | go               | go-mode                              | go-langserver                  |
;; | haskell          | haskell-mode                         | haskell-language-server        |
;; | java             | java-mode                            | lsp-java                       |
;; | javascript       | js2-mode, rjsx-mode, typescript-mode | typescript-language-server     |
;; | julia            | julia-mode                           | LanguageServer.jl              |
;; | ocaml            | tuareg-mode                          | ocaml-language-server          |
;; | php              | php-mode                             | php-language-server            |
;; | python           | python-mode                          | lsp-python-ms                  |
;; | ruby             | ruby-mode                            | solargraph                     |
;; | rust             | rust-mode                            | rls                            |
;; | scala            | scala-mode                           | metals                         |
;; | sh               | sh-mode                              | bash-language-server           |
;; | swift            | swift-mode                           | sourcekit                      |
;; | web              | web-mode, css-mode, scss-mode        | vscode-css-languageserver-bin, |
;; |                  |      sass-mode, less-css-mode        | vscode-html-languageserver-bin |
;; | purescript       | purescript-mode                      | purescript-language-server     |
;; | zig              | zig-mode                             | zls                            |

;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/LAYERS.org#django
;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/LAYERS.org#react
