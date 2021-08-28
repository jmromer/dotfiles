;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
;;
;; API Demos:
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/emacs-lisp/demos.org

;; editing
(package! evil-matchit)

;; ruby
(disable-packages! minitest)

(package! evil-rails)
(package! ggtags)
(package! ruby-factory)
(package! ruby-test-mode)
(package! seeing-is-believing)

(package! yankee
  :recipe (:host github
           :repo "jmromer/yankee.el"
           :files ("yankee.el")))

(package! dash-at-point)
