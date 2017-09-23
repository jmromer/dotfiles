;;; spacemacs.d/init.el -- Spacemacs configuration

;;; Commentary:
;; This file is loaded by Spacemacs at startup.

;;; Code:
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior nil
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 2
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)
     better-defaults
     (c-c++ :variables
            c-c++-enable-clang-support t)
     (clojure :variables clojure-enable-fancify-symbols t)
     command-log
     dash
     deft
     elixir
     elm
     emacs-lisp
     evil-cleverparens
     evil-commentary
     git
     github
     (go :variables
         gofmt-command "goimports"
         go-tab-width 4
         go-use-gometalinter t)
     graphviz
     haskell
     ivy
     (javascript :variables
                 tern-command '("node" "/Users/jkrmr/.node/bin/tern"))
     jekyll
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-folding t)
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     (org :variables
          org-enable-github-support t)
     osx
     (python :variables
             python-test-runner '(pytest nose)
             python-sort-imports-on-save t
             python-enable-yapf-format-on-save t)
     react
     restclient
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-test-runner 'rspec)
     ruby-on-rails
     search-engine
     (shell :variables
            shell-default-shell 'multi-term
            shell-default-term-shell "/usr/local/bin/zsh"
            shell-default-full-span nil
            shell-default-height 35
            shell-default-position 'bottom)
     spacemacs-layouts
     sql
     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-by-default t
                      syntax-checking-enable-tooltips t)
     swift
     vimscript
     vinegar
     version-control
     yaml
     )
   dotspacemacs-additional-packages
   '(
     company-flx
     evil-quickscope
     flx
     ob-swift
     ov
     pretty-mode
     seeing-is-believing
     toc-org
     vimish-fold
     )
   dotspacemacs-excluded-packages
   '(
     ace-jump-mode
     evil-mc
     fancy-battery
     neotree
     ob-elixir
     )
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-enable-lazy-installation nil
   ))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings t
                                       hybrid-mode-default-state 'normal)
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 3)
                                (projects . 5)
                                (bookmarks . 5)
                                (agenda . 5)
                                (todos . 5))
   dotspacemacs-scratch-mode 'markdown-mode
   dotspacemacs-themes '(solarized-dark spacemacs-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code Retina"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.3
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers '(:relative t
                               :enabled-for-modes prog-mode
                               :size-limit-kb 1024)
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'all))

(defun dotspacemacs/user-init ()
  "Load variables before package initialization."
  (setq-default git-magit-status-fullscreen t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(evil-want-Y-yank-to-eol t)
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-eslint)))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-confirm-babel-evaluate nil)
 '(org-format-latex-options
   (quote
    (:foreground "White" :background default :scale 1.8 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(package-selected-packages
   (quote
    (ob-swift auctex-latexmk yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vmd-mode vimrc-mode vimish-fold vi-tilde-fringe uuidgen use-package unfill toc-org tagedit swift-mode sql-indent spaceline powerline smex smeargle slim-mode shell-pop seeing-is-believing scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reveal-in-osx-finder restart-emacs request rbenv rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode pretty-mode popwin pip-requirements persp-mode pcre2el pbcopy paradox ox-gfm ov osx-trash osx-dictionary orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download org-bullets open-junk-file org-plus-contrib ob-restclient ob-http mwim multi-term move-text mmm-mode minitest markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc ivy-hydra intero info+ indent-guide hyde hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make haskell-snippets haml-mode graphviz-dot-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md fuzzy flyspell-correct-ivy flyspell-correct flycheck-pos-tip flycheck-mix flycheck-haskell flycheck-gometalinter flycheck-elm flycheck-credo flycheck flx-ido fill-column-indicator feature-mode eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist projectile-rails rake evil-quickscope evil-numbers evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens smartparens evil-args evil-anzu anzu evil goto-chg undo-tree eshell-z eshell-prompt-extras esh-help engine-mode emmet-mode elm-mode elisp-slime-nav dumb-jump disaster diminish diff-hl deft dash-at-point dactyl-mode cython-mode counsel-projectile projectile counsel-dash helm-dash helm helm-core counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-statistics company-restclient restclient know-your-http-well company-quickhelp pos-tip company-go go-mode company-ghci company-ghc ghc haskell-mode company-flx flx company-cabal company-c-headers company-auctex company-anaconda command-log-mode column-enforce-mode coffee-mode cmm-mode cmake-mode clojure-snippets clj-refactor hydra inflections edn multiple-cursors paredit peg clean-aindent-mode clang-format cider-eval-sexp-fu eval-sexp-fu highlight cider seq spinner queue clojure-mode chruby bundler inf-ruby bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex async anaconda-mode pythonic f alchemist s company elixir-mode pkg-info epl aggressive-indent adaptive-wrap ace-window ace-link avy ac-ispell auto-complete popup solarized-theme dash)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(projectile-git-command "/usr/local/bin/git ls-files -zco --exclude-standard")
 '(safe-local-variable-values
   (quote
    ((projectile-ignored-directories quote
                                     ("node_modules"))
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking)
     (eval setenv "RBENV_VERSION" "system")
     (eval setq exec-path
           (append
            (quote
             ((concat
               (projectile-project-root)
               "bin")))
            exec-path))
     (eval setenv "PATH"
           (concat
            (projectile-project-root)
            "bin:"
            (getenv "PATH")))
     (eval add-to-list
           (quote exec-path)
           (concat
            (projectile-project-root)
            "bin")))))
 '(spacemacs-theme-comment-bg nil t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#af0000"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#03A48D"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#718E4B"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#268bd2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#9e216e"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#4c7173"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#348994"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#a25707")))))

(defun dotspacemacs/user-config ()
  "Load configuration after layer initialization."
  ;; Add local packages directory to load path
  (add-to-list 'load-path (format "%s/.spacemacs.d/local" (getenv "HOME")))

  (config/firacode)
  (config/prettify-symbols)
  (config/evil-cleverparens)
  (config/highlight-sexp)
  (config/highlight-lines-at-length 80)
  (config/flycheck)
  (config/exercism)
  (config/org-mode)
  (config/org-latex-preview)
  (config/git-and-magit)
  (config/yankee)
  (config/underscore-to-word-char-list)
  (config/company)
  (config/terminal-buffers)
  (config/dired)
  (config/latex-mode)

  ;; Don't create lockfiles
  (setq create-lockfiles nil)

  (condition-case err
      (if (jkrmr/is-in-terminal-p)
          (spacemacs/load-theme 'spacemacs-dark))
    (error "Error in spacemacs/load-theme: %s" (error-message-string err)))

  ;; Copy exec-path from shell PATH
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; Projectile settings
  (setq-default projectile-completion-system 'ivy
                projectile-enable-caching t
                projectile-globally-ignored-directories '("node_modules"))

  ;; ivy
  (setq-default ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-fuzzy)
                                        (mx . ivy--regex-fuzzy)
                                        (swiper . ivy--regex-plus)
                                        (counsel-git-grep . ivy--regex-fuzzy)
                                        (t . ivy--regex-fuzzy))
                ivy-initial-inputs-alist nil
                ivy-wrap t)

  ;; Explicitly prepend python binaries location to exec-path.
  ;; Fixes flake8, yapfify failure to load.
  (setq-default exec-path
                (cons (format "%s/.anaconda3/bin" (getenv "HOME")) exec-path))
  ;; Go mode
  (add-hook 'go-mode-hook '(lambda ()
                             (whitespace-toggle-options 'tabs)))
  ;; Haskell
  (add-hook 'haskell-mode-hook 'intero-mode)

  ;; prevent visual selection from overriding system clipboard
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; disable mouse support in terminal
  (xterm-mouse-mode -1)

  ;; markdown mode leader keybindings
  (spacemacs/declare-prefix-for-mode 'markdown-mode "e" "markdown/evaluate")
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "ee" #'alchemist-iex-send-current-code-block)

  ;; Objective-C
  (add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))


  ;; ruby-tools minor mode
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode "x#" 'ruby-tools-interpolate)

  ;; Configure miscellaneous settings (temporary).
  ;; In evil ex buffer, backward char like emacs
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)

  ;; By default, don't soft-wrap lines longer than line length
  (set-default 'truncate-lines nil)

  ;; enable evil-quickscope-mode
  (global-evil-quickscope-mode 1)

  ;; add a space to the right of line numbers
  (setq-default left-fringe-width 10)

  (add-hook 'highlight-sexp-mode-hook
            '(lambda () (diminish 'highlight-sexp-mode "⋂")))
  (add-hook 'minitest-mode-hook
            '(lambda () (diminish 'minitest-mode "⨷")))
  (add-hook 'elm-indent-mode-hook
            '(lambda () (diminish 'elm-indent-mode "⨕")))
  (add-hook 'alchemist-mode-hook
            '(lambda () (diminish 'alchemist-mode "⊛")))
  (add-hook 'alchemist-phoenix-mode-hook
            '(lambda () (diminish 'alchemist-phoenix-mode "⊙")))
  (add-hook 'tern-mode-hook
            '(lambda () (diminish 'tern-mode "₸")))
  (add-hook 'rubocop-mode-hook
            '(lambda () (diminish 'rubocop-mode "℞")))
  (add-hook 'seeing-is-believing-hook
            '(lambda () (diminish 'seeing-is-believing "S")))

  ;; don't warn about large files
  (setq-default large-file-warning-threshold nil)

  ;; make focus window commands primary
  (spacemacs/set-leader-keys "ws" #'split-window-below-and-focus)
  (spacemacs/set-leader-keys "wS" #'split-window-below)
  (spacemacs/set-leader-keys "wv" #'split-window-right-and-focus)
  (spacemacs/set-leader-keys "wV" #'split-window-right)
  (spacemacs/set-leader-keys "wT" #'split-term-window-right-and-focus)

  ;; ruby mode: seeing-is-believing
  (require 'seeing-is-believing)
  (add-hook 'ruby-mode-hook 'seeing-is-believing)

  (defun xmpfilter-eval-current-line ()
    (interactive)
    (seeing-is-believing-mark-current-line-for-xmpfilter)
    (seeing-is-believing-run-as-xmpfilter))

  (define-key ruby-mode-map (kbd "C-c C-c") 'xmpfilter-eval-current-line)
  (define-key ruby-mode-map (kbd "C-c C-v") 'seeing-is-believing-clear)
  (define-key ruby-mode-map (kbd "C-c C-f") 'seeing-is-believing-run)

  ;; vimish-fold settings
  (add-to-list 'evil-fold-list '((vimish-fold-mode)
                                 :open-all   vimish-fold-unfold-all
                                 :close-all  nil
                                 :toggle     vimish-fold-toggle
                                 :open       vimish-fold-unfold
                                 :open-rec   nil
                                 :close      vimish-fold))
  ;; vimish-fold keybindings
  (define-key evil-normal-state-map (kbd "zf") nil)
  (define-key evil-visual-state-map (kbd "zf") #'vimish-fold)
  (define-key evil-normal-state-map (kbd "zfd") #'vimish-fold-delete)
  (define-key evil-normal-state-map (kbd "zfj") #'vimish-fold-next-fold)
  (define-key evil-normal-state-map (kbd "zfk") #'vimish-fold-previous-fold)
  (define-key evil-normal-state-map (kbd "zfm") #'vimish-fold-refold-all)
  (define-key evil-normal-state-map (kbd "zfr") #'vimish-fold-unfold-all)
  (define-key evil-normal-state-map (kbd "zft") #'vimish-fold-toggle-all)
  (define-key evil-normal-state-map (kbd "zfa") #'vimish-fold-toggle)

  ;; js, react configuration
  (setq-default js-indent-level 2
                js2-basic-offset 2
                css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  ;; Tell js2 mode to shut the hell up about semicolons
  (setq js2-strict-missing-semi-warning nil)

  (defun js-standard-fix ()
    (interactive)
    (shell-command-on-region
     (point-min) (point-max)
     (format "standard --fix %s" (buffer-file-name))))

  ;; web-beautify
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'js
    '(add-hook 'js-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'json-mode
    '(add-hook 'json-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'sgml-mode
    '(add-hook 'html-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

  (eval-after-load 'css-mode
    '(add-hook 'css-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

  ;; rbenv: use global rbenv-managed ruby
  (rbenv-use-global)

  (with-eval-after-load 'spaceline
    (spaceline-define-segment version-control
      "Only display the current branch name in mode line."
      (when vc-mode
        (powerline-raw
         (s-trim (replace-regexp-in-string "Git[:-]" "" vc-mode)))))
    (setq-default powerline-default-separator nil)
    (spaceline-compile))

  (spacemacs/declare-prefix "fd" "files/display")
  (spacemacs/set-leader-keys "fdp" 'jkrmr/display-file-path)

  ;; wrap lines in compilation buffer
  (defun my-compilation-mode-hook ()
    (setq truncate-lines nil) ;; automatically becomes buffer local
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'compilation-mode-hook #'my-compilation-mode-hook)

  ;; execute local configuration file last
  (jkrmr/config-load-local))

(defun config/latex-mode ()
  "Configure LaTeX mode."
  (defun XeLaTeX-compile ()
    (interactive)
    (async-shell-command (format "xelatex %s" (buffer-file-name))))

  (spacemacs/declare-prefix-for-mode 'latex-mode "SPC" "compile")
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode "SPC SPC" #'XeLaTeX-compile)

  ;; Compile resume: Defined in local config
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode "SPC r" #'XeLaTeX-compile-resume)

  ;; Update preview when file changes
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; Detect xelatex files
  (add-to-list 'auto-mode-alist '("\\.xtx\\'" . LaTeX-mode)))

(defun config/dired ()
  "Configure dired."
  ;; In normal mode, `-` opens dired in the PWD
  (define-key evil-normal-state-map (kbd "-")
    (lambda () (interactive) (dired "./")))

  (defun dired-keybindings ()
    (define-key dired-mode-map "?" #'evil-search-backward)
    (define-key dired-mode-map (kbd "C-w") nil)
    (define-key dired-mode-map (kbd "C-o")
      (lambda () (interactive) (switch-to-previous-buffer)))
    (define-key dired-mode-map (kbd "C-i")
      (lambda () (interactive) (switch-to-previous-buffer))))

  ;; dired keybindings
  (add-hook 'dired-mode-hook #'dired-keybindings))

(defun config/terminal-buffers ()
  "Configure terminal buffers."

  ;; Use emacs state when starting term mode
  (evil-set-initial-state 'term-mode 'emacs)

  ;; toggle between emacs and evil-normal states with ESC x 3
  (global-set-key (kbd "ESC ESC ESC") #'evil-normal-state)

  (defun term-send-ctrl-y ()
    (interactive)
    (term-send-raw-string "\C-y"))

  (defun term-mode-config ()
    (define-key term-raw-map (kbd "C-y") #'term-send-ctrl-y)
    (define-key term-raw-map (kbd "C-p") #'term-send-up)
    (define-key term-raw-map (kbd "C-n") #'term-send-down)
    (goto-address-mode))

  (add-hook 'term-mode-hook #'term-mode-config)

  ;; Use utf8
  (defun my-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook #'my-term-use-utf8)

  ;; ansi-term: always use default shell
  (defadvice ansi-term (before force-bash)
    (interactive (list shell-default-term-shell)))
  (ad-activate #'ansi-term))

(defun config/underscore-to-word-char-list ()
  "Add underscore to word char list in prog and other modes."
  (defun add-underscore-to-word-chars ()
    "Adds underscore to the word chars syntax entry list."
    (modify-syntax-entry ?_ "w"))

  (add-hook 'prog-mode-hook #'add-underscore-to-word-chars)
  (add-hook 'markdown-mode-hook #'add-underscore-to-word-chars)
  (add-hook 'org-mode-hook #'add-underscore-to-word-chars))

(defun config/company ()
  "Enable and configure company mode."
  (global-company-mode)
  (with-eval-after-load 'company
    (company-flx-mode +1)))

(defun config/yankee ()
  "Load and configure yankee.el.
Provides facilities for yanking formatted code snippets."
  (load "yankee.el/yankee.el")
  (require 'yankee)
  (define-key evil-visual-state-map (kbd "gy") nil)
  (define-key evil-visual-state-map (kbd "gym") #'yankee/yank-as-gfm-code-block)
  (define-key evil-visual-state-map (kbd "gyf") #'yankee/yank-as-gfm-code-block-folded)
  (define-key evil-visual-state-map (kbd "gyo") #'yankee/yank-as-org-code-block))

(defun config/git-and-magit ()
  "Configure Magit and git-related settings."
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)
    (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)
    (magit-define-popup-switch 'magit-log-popup ?m "Omit merge commits" "--no-merges"))

  ;; leader gb to display branching controls
  (spacemacs/set-leader-keys "gb" 'magit-branch-popup)

  ;; leader gB to display Git blame
  (spacemacs/set-leader-keys "gB" 'spacemacs/git-blame-micro-state)

  ;; Git Gutter: Display fringe on left
  (setq-default git-gutter-fr+-side 'left-fringe)
  (setq-default git-gutter-fr:side 'left-fringe))

(defun config/org-mode ()
  "Configure and enable org mode."
  ;; Org Babel: Elixir
  (load "ob-elixir/ob-elixir.el")
  (require 'ob-elixir)

  ;; Org Babel languages
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure . t)
                                 (elixir . t)
                                 (emacs-lisp . t)
                                 (haskell . t)
                                 (js . t)
                                 (org . t)
                                 (python . t)
                                 (ruby . t)
                                 (swift . t)
                                 (shell . t)))

  (with-eval-after-load 'org
    (setq-default org-md-headline-style 'setext)
    (setq-default org-src-tab-acts-natively t)

    (setq-default org-babel-python-command "python3")
    ;; Org Babel: Python
    (setq org-babel-default-header-args:python
          (cons '(:results . "value pp")
                (assq-delete-all :results org-babel-default-header-args:python)))

    ;; Org Babel: Elixir
    (setq org-babel-default-header-args:elixir
          (cons '(:results . "value")
                (assq-delete-all :results org-babel-default-header-args:elixir)))
    (setq org-babel-default-header-args:elixir
          (cons '(:preamble . "Code.compiler_options(ignore_module_conflict: true)")
                (assq-delete-all :preamble org-babel-default-header-args:elixir)))

    ;; Source Blocks
    ;; Python: <p
    (add-to-list 'org-structure-template-alist
                 '("p"
                   "#+BEGIN_SRC python :exports both\n?\n#+END_SRC"
                   "<src lang=\"python\">\n?\n</src>"))
    ;; Elixir: <x
    (add-to-list 'org-structure-template-alist
                 '("x"
                   "#+BEGIN_SRC elixir\n?\n#+END_SRC"
                   "<src lang=\"elixir\">\n?\n</src>"))
    ;; Emacs Lisp: <el
    (add-to-list 'org-structure-template-alist
                 '("el"
                   "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"
                   "<src lang=\"emacs-lisp\">\n?\n</src>"))))

(defun config/exercism ()
  "Configure and enable exercism mode."
  (setq-default exercism-dir "~/Projects/exercism")
  (setq-default exercism-auto-enable nil)
  (load "exercism-emacs/exercism.el")
  (require 'exercism))

(defun config/flycheck ()
  "Configure and enable Flycheck."
  (setq-default
   flycheck-global-modes '(LaTeX-mode
                           c++-mode
                           c-mode
                           coffee-mode
                           elixir-mode
                           emacs-lisp-mode
                           enh-ruby-mode
                           go-mode
                           haml-mode
                           haskell-mode
                           js2-mode
                           json-mode
                           less-mode
                           markdown-mode
                           pug-mode
                           python-mode
                           react-mode
                           ruby-mode
                           sass-mode
                           scss-mode
                           slim-mode
                           web-mode
                           yaml-mode))
  (global-flycheck-mode))

(defun config/prettify-symbols ()
  "Enable and configure prettify-symbols mode and pretty mode."
  ;; pretty-mode
  (require 'pretty-mode)
  (global-pretty-mode t)

  (pretty-deactivate-groups '(:equality :ordering :ordering-double :arrows
                                        :ordering-triple :arrows-twoheaded
                                        :punctuation :logic :sets))

  (pretty-activate-groups '(:sub-and-superscripts :greek :arithmetic-nary))

  ;; prettify symbols
  (global-prettify-symbols-mode)

  ;; prettify symbols: Python
  (defun config/prettify-symbols-python ()
    "Provide prettify-symbol mode mappings for python-mode."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("def" .    #x0192)
            ("in" .     #x2208)
            ("is" .     #x2261)
            ("is not" . #x2262)
            ("not in" . #x2209)
            ("all" .    #x2200)
            ("any" .    #x2203))))
  (add-hook 'python-mode-hook #'config/prettify-symbols-python)

  ;; prettify symbols: Emacs Lisp
  (defun config/prettify-symbols-emacs-lisp ()
    "Provide prettify-symbol mode mappings for emacs-lisp-mode."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("defun" .  #x0192))))
  (add-hook 'emacs-lisp-mode-hook #'config/prettify-symbols-emacs-lisp)

  ;; prettify symbols: JavaScript
  (defun config/prettify-symbols-javascript ()
    "Provide prettify-symbol mode mappings for javascript modes."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("function" .  #x0192))))
  (add-hook 'js2-mode-hook #'config/prettify-symbols-javascript))

(defun config/org-latex-preview ()
  "Configure LaTeX preview settings for Org mode."
  (with-eval-after-load 'org
    (setq org-format-latex-options
          (plist-put org-format-latex-options :justify 'center))

    (defun org-justify-fragment-overlay (beg end image imagetype)
      "Adjust the justification of a LaTeX fragment.
The justification is set by :justify in `org-format-latex-options'.
Only equations at the beginning of a line are justified."
      (setq-default org-format-latex-header "\\documentclass[reqno]{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}
% do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-16cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

    (require 'ov)
    (cond
     ;; Centered justification
     ((and (eq 'center (plist-get org-format-latex-options :justify))
           (= beg (line-beginning-position)))
      (let* ((img (create-image image 'imagemagick t))
             (width (car (image-size img)))
             (offset (floor (- (/ 40 2) (/ width 2)))))
        (overlay-put (ov-at) 'before-string (make-string offset ? ))))
     ;; Right justification
     ((and (eq 'right (plist-get org-format-latex-options :justify))
           (= beg (line-beginning-position)))
      (let* ((img (create-image image 'imagemagick t))
             (width (car (image-display-size (overlay-get (ov-at) 'display))))
             (offset (floor (- 40 width (- (line-end-position) end)))))
        (overlay-put (ov-at) 'before-string (make-string offset ? ))))))
  (advice-add 'org--format-latex-make-overlay :after #'org-justify-fragment-overlay)))

(defun config/highlight-lines-at-length (chars)
  "Configure and enable whitespace mode to color text after CHARS chars."
  (setq-default whitespace-line-column chars
                whitespace-style '(face lines-tail empty tabs))
  ;; Enable excess length highlighting in prog-mode
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'org-mode-hook #'whitespace-mode)

  ;; Manually set trailing whitespace cleanup
  ;; (workaround, since the preceding breaks whitespace-cleanup,
  ;; which `dotspacemacs-whitespace-cleanup 'all' uses.)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(defun config/highlight-sexp ()
  "Configure highlight-sexp.
https://github.com/daimrod/highlight-sexp."
  (load "highlight-sexp/highlight-sexp.el")
  (require 'highlight-sexp)
  (setq-default hl-sexp-background-color "#20525E")
  (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
  (add-hook 'clojure-mode-hook 'highlight-sexp-mode))

(defun config/evil-cleverparens ()
  "Configure evil-cleverparens layer."
  (require 'evil-cleverparens-text-objects)
  (smartparens-strict-mode)
  (spacemacs/toggle-evil-cleverparens-on)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode))

(defun config/firacode ()
  "Configure firacode font face with ligatures.
See: https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs"
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(defun yas/strip (str)
  "Remove commas, spaces, and colons from STR."
  (replace-regexp-in-string "[:\s,]" "" str))

(defun yas/to-ivar-assignment (str)
  "Make 'STR' to '@`STR` = `STR`'."
  (format "@%s = %s" (yas/strip str) (yas/strip str)))

(defun yas/indent-level ()
  "Determine the number of spaces the current line is indented."
  (interactive)
  (string-match "[^[:space:]]" (thing-at-point 'line t)))

(defun yas/indent-string ()
  "Return a string of spaces matching the current indentation level."
  (interactive)
  (make-string (yas/indent-level) ?\s))

(defun yas/indented-newline ()
  "Newline followed by correct indentation."
  (interactive)
  (format "\n%s" (yas/indent-string)))

(defun yas/args-list ()
  "Extract an args list from teh current line."
  (interactive)
  (string-match "\(.+\)" (thing-at-point 'line t)))

(defun yas/to-ruby-ivars-list (str)
  "Splits STR into a set of ivar assignments."
  (interactive)
  (mapconcat 'yas/to-ivar-assignment
             (split-string str ",")
             (yas/indented-newline)))

(defun jkrmr/display-file-path ()
  "Print the path of the current buffer's file."
  (interactive)
  (message (abbreviate-file-name (buffer-file-name))))

(defun jkrmr/config-load-local ()
  "Load local configuration overrides."
  (load "~/.init.local.el"))

(defun jkrmr/is-in-terminal-p ()
  "Return true if in terminal Emacs, else false."
  (not (display-graphic-p)))

(defun buffer-exists-p (bufname)
  "Check if a buffer with the given name BUFNAME exists."
  (not (eq nil (get-buffer bufname))))

(defun switch-to-previous-buffer ()
  "Switch to the previously open buffer.
Repeated invocations toggle between the two most recently open buffers.
Excludes the ibuffer."
  (interactive)
  (if (buffer-exists-p "*Ibuffer*")  (kill-buffer "*Ibuffer*"))
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun alchemist-iex-send-current-code-block ()
  "Compile the current markdown block to iex."
  (interactive)
  (save-excursion
    (re-search-backward "```")
    (next-line)
    (push-mark)
    (re-search-forward "```")
    (previous-line)
    (move-end-of-line nil)
    (alchemist-iex-send-region (region-beginning) (region-end))
    (pop-mark)))

(defun split-term-window-right-and-focus ()
  "Open an `ansi-term' split to the right and focus it."
  (interactive)
  (defvar shell-default-term-shell)
  (split-window-right-and-focus)
  (ansi-term shell-default-term-shell))

(defun rerun-term-command-right ()
   "Re-issue previously issued command in terminal split to the right."
   (interactive)
   (evil-window-right 1)
   (evil-insert-state)
   (execute-kbd-macro (kbd "M-p"))
   (execute-kbd-macro (kbd "RET"))
   (evil-window-left 1))

(defun rerun-term-command-below ()
  "Re-issue previously issued command in a terminal split below."
  (interactive)
  (evil-window-down 1)
  (evil-insert-state)
  (execute-kbd-macro (kbd "M-p"))
  (execute-kbd-macro (kbd "RET"))
  (evil-window-up 1))

;;; init.el ends here
