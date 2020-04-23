;;; setup.el --- Emacs configuration customizations.

;;; Commentary:

;;; Code:
(require 'funcs)
(require 'editorconfig)


;;; Package configs

(config/flycheck)
(config/highlight-lines-at-length 80)
(config/javascript-modes)
(config/latex-mode)
(config/lisps)
(config/org-latex-preview)
(config/org-mode)
(config/python)
(config/ruby)
(config/shell-buffers)
(config/add-to-word-char-list)
(config/web-mode)


;;; Misc settings

(setq-default
 browse-url-browser-function 'xwidget-webkit-browse-url
 compilation-scroll-output 'first-error
 copy-as-format-asciidoc-include-file-name t
 copy-as-format-default "github"
 doom-modeline-buffer-encoding nil
 doom-modeline-buffer-file-name-style 'relative-to-project  ;; 'truncate-with-project
 doom-modeline-buffer-modification-icon t
 doom-modeline-buffer-state-icon t
 doom-modeline-display-default-persp-name t
 doom-modeline-persp-icon nil
 doom-modeline-enable-word-count t
 doom-modeline-evil-state-icon t
 doom-modeline-github t
 doom-modeline-height 20
 doom-modeline-icon (display-graphic-p)
 doom-modeline-indent-info nil
 doom-modeline-lsp t
 doom-modeline-major-mode-color-icon nil
 doom-modeline-major-mode-icon t
 doom-modeline-minor-modes nil
 doom-modeline-persp-name t
 doom-modeline-project-detection 'project
 evil-collection-outline-bind-tab-p t
 evil-collection-setup-debugger-keys t
 evil-collection-setup-minibuffer t
 evil-collection-term-sync-state-and-mode-p t
 evil-goggles-duration 0.7
 evil-goggles-pulse nil
 google-translate-backend-method 'curl
 google-translate-output-destination 'current-buffer ;; 'popup 'kill-ring 'current-buffer
 google-translate-pop-up-buffer-set-focus t
 google-translate-translation-directions-alist '(("en" . "es") ("en" . "fr"))
 ;; Used by helm to de-dupe history entries (or not)
 history-delete-duplicates t
 ;; history-length 1000
 ido-enable-flex-matching t
 ;; disable ido faces to see flx highlights
 ido-use-faces nil
 ;; Don’t compact font caches during GC
 inhibit-compacting-font-caches t
 ispell-program-name "ispell"
 json-fmt-on-save nil
 json-fmt-tool 'prettier
 lsp-signature-auto-activate nil  ;; lsp: don't auto-activate signature hints
 mac-command-modifier 'super
 projectile-completion-system 'helm
 projectile-enable-caching t
 projectile-find-dir-includes-top-level t
 projectile-project-search-path '("~/Projects" "~/Work" "~/Resources" "~/Exercism" "~/Tutorials")
 tramp-default-method "ssh"
 ;; when visiting a link under VC, follow to the real file
 vc-follow-symlinks t)


;;; Projectile

(with-eval-after-load 'projectile
  (projectile-register-project-type
   'elixir-mix
   '("mix.exs")
   :compile "mix compile"
   :test "mix test"
   :test-suffix "_test")
  (projectile-register-project-type
   'python-pipenv
   '("Pipfile")
   :compile "pipenv run compile"
   :test "pipenv run test"
   :test-suffix "_test")
  (projectile-register-project-type
   'python-pytest
   '(".pytest_cache")
   :compile ""
   :test "pytest"
   :test-prefix "test_"
   :test-suffix "_test")
  (projectile-register-project-type
   'rails-rspec
   '("Gemfile" "app" "lib" "db" "config" "spec")
   :compile "bin/rails server"
   :src-dir "lib/"
   :test "bin/rspec --no-profile --format progress"
   :test-dir "spec/"
   :test-suffix "_spec"
   :related-files-fn #'rails--find-related-file)

  (if (boundp 'projectile-globally-ignored-directories)
      (add-to-list 'projectile-globally-ignored-directories "node_modules" t)))



;;; Org Journal

(spacemacs|define-transient-state org-journal
  :title "Org Journal Transient State"
  :hint nil
  :foreign-keys run
  :doc
  "\n[_p_/_N_] previous [_n_] next [_c_] current [_s_] search all [_f_] search future [_S_] search range [_q_] quit"
  :bindings
  ("p" org-journal-open-previous-entry)
  ("N" org-journal-open-previous-entry)
  ("n" org-journal-open-next-entry)
  ("c" org-journal-today)
  ("s" org-journal-search-forever)
  ("f" org-journal-search-future)
  ("S" org-journal-search)
  ("q" nil :exit t))



;;; Hide-Show (code folding)

(with-eval-after-load 'hideshow
  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "do" "{" "[")) ;; Block start
                  ,(rx (or "}" "]" "end"))                      ;; Block end
                  ,(rx (or "#" "=begin"))                       ;; Comment start
                  ruby-forward-sexp nil)))



;;; Appearance

;; ensure new frames are created transparent
(add-hook 'after-make-frame-functions #'spacemacs/enable-transparency)

;; make the current one transparent
(spacemacs/enable-transparency)

;; Natural titlebar in GUI
(when window-system
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Ensure dark theme in terminal
(when (not window-system)
  (spacemacs/load-theme 'spacemacs-dark))

;; doom-modeline
(set-face-attribute 'doom-modeline-persp-name nil :inherit '(font-lock-doc-face italic mode-line-emphasis))
(set-face-attribute 'doom-modeline-persp-buffer-not-in-persp nil :inherit '(font-lock-doc-face mode-line-emphasis))
(doom-modeline-set-modeline 'main t)
(doom-modeline-mode)



;;; Emoji

;; Render unicode emoji as emoji
(defun set-emoji-font (frame)
  "Render unicode emoji as emoji.
Adjust the font settings of the given FRAME to do this."
  (if (eq system-type 'darwin)
      (set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") frame 'prepend)
    (set-fontset-font t 'unicode (font-spec :family "Symbola") frame 'prepend)))

(if window-system
    (set-emoji-font nil)
  (add-hook 'after-make-frame-functions #'set-emoji-font))



;;; Globally enabled minor modes

(beacon-mode)
(company-prescient-mode)
(direnv-mode)
(display-battery-mode 1)
(editorconfig-mode)
(evil-goggles-mode)
(evil-goggles-use-diff-refine-faces)
(global-evil-matchit-mode)
(global-evil-quickscope-mode)
(global-visual-line-mode)
(ido-mode)  ;; ido (for amx, ido-dired)
(flx-ido-mode 1)
(prescient-persist-mode)
(smartparens-global-strict-mode)



;; Hooks

;; after-init hooks
(add-hook 'after-init-hook #'global-emojify-mode)
(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; mode-specific hooks
(add-hook 'elixir-mode-hook #'lsp)
(add-hook 'ledger-mode-hook #'evil-ledger-mode)

(with-eval-after-load 'elm-mode
  (remove-hook 'elm-mode-hook 'elm-indent-mode))

(with-eval-after-load 'evil
  (when (require 'evil-collection nil t)
    (evil-collection-init)))



;;; Garbage collection optimizations

(defun disable-garbage-collection ()
  "Disable GC by setting threshold to max num."
  (setq-default gc-cons-threshold most-positive-fixnum))

(defun enable-garbage-collection ()
  "Re-enable GC collection. Set threshold to init.el value."
  (defvar dotspacemacs-gc-cons)
  (setq-default gc-cons-threshold (car dotspacemacs-gc-cons)))

(add-hook 'minibuffer-setup-hook #'disable-garbage-collection)
(add-hook 'minibuffer-exit-hook #'enable-garbage-collection)



;;; "Tail" settings (should be run last)

(config/prettify-symbols)
(config/company)

(provide 'setup)
;;; setup.el ends here
