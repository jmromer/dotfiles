(require 'package)

(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)

; packages
(setq package-list '(cider clj-refactor clojure-mode clojure-mode-extra-font-locking company dash epl evil evil-leader f git-commit git-gutter goto-chg ido-completing-read+ ido-ubiquitous javap-mode js2-mode magit magit-popup package-build paredit pkg-info projectile queue rainbow-delimiters s shut-up smex tagedit undo-tree w3m with-editor))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; evil mode
(require 'evil)
(setq evil-shift-width 2)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "e" 'find-file
  "g" 'ag-project
  "w" 'save-buffer
  "f" 'projectile-find-file
  "x" 'kill-buffer)

(evil-mode 1)

; rcodetools
(setq rcodetools-dir
      (expand-file-name
        "../../.."
        (replace-regexp-in-string
          "\n$" ""
          (shell-command-to-string
            "gem which rcodetools/xmpfilter"))))

(add-to-list 'load-path rcodetools-dir)

(setq xmpfilter-command-name
      "ruby -S xmpfilter --no-warnings --dev --fork --detect-rbtest")

(require 'ruby-mode)
(require 'rcodetools)

(define-key ruby-mode-map (kbd "C-c C-c") 'xmp)

(ruby-mode)


; Customization
; =============

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
