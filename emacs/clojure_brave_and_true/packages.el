;;; packages.el --- clojure_brave_and_true Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq clojure_brave_and_true-packages
    '(
      ;; provided by clojure layer:
      ; align-cljlet
      ; cider
      ; cider-eval-sexp-fu
      ; clj-refactor
      ; clojure-mode
      ; clojure-mode-extra-font-locking
      ; company
      ; ido-ubiquitous
      ; magint
      ; rainbow-delimiters
      ; smex
      ; subword
      ; tagedit
      ido-ubiquitous
      tagedit
      magit
      ))

;; List of packages to exclude.
(setq clojure_brave_and_true-excluded-packages '())

;; For each package, define a function clojure_brave_and_true/init-<package-name>
;;
(defun clojure_brave_and_true/ido-ubiquitous () )
(defun clojure_brave_and_true/tagedit () )
(defun clojure_brave_and_true/magit () )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
