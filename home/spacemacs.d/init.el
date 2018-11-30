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
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 2
                      auto-completion-idle-delay 0.2
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)
     better-defaults
     (c-c++ :variables
            c-c++-enable-clang-support t)
     command-log
     dash
     deft
     django
     (elm :variables
          elm-sort-imports-on-save t
          elm-format-on-save t
          elm-format-command "elm-format")
     emacs-lisp
     evil-commentary
     git
     github
     (go :variables
         gofmt-command "goimports"
         go-tab-width 4
         go-use-gometalinter t)
     graphviz
     html
     (ivy :variables
          ivy-wrap t)
     (javascript :variables
                 tern-command '("node" "/Users/jmromer/.node/bin/tern")
                 javascript-backend 'lsp
                 javascript-fmt-tool 'prettier
                 node-add-modules-path t)
     jekyll
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-folding t)
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     (org :variables
          org-enable-github-support t)
     (osx :variables
          osx-command-as 'hyper
          osx-option-as 'meta
          osx-control-as 'control
          osx-function-as nil
          osx-right-command-as 'left
          osx-right-option-as 'none
          osx-right-control-as 'left)
     (python :variables
             python-save-before-test t
             python-test-runner '(pytest nose)
             python-sort-imports-on-save nil
             python-enable-yapf-format-on-save nil)
     react
     restclient
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-test-runner 'rspec)
     ruby-on-rails
     search-engine
     (shell :variables
            multi-term-program "/usr/local/bin/zsh"
            shell-default-full-span nil
            shell-default-position 'bottom
            shell-default-shell 'multi-term
            shell-default-term-shell "/usr/local/bin/zsh"
            shell-enable-smart-eshell t)
     spacemacs-layouts
     speed-reading
     sql
     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-by-default t
                      syntax-checking-enable-tooltips t)
     version-control
     vimscript
     vinegar
     yaml
     )
   dotspacemacs-additional-packages
   '(
     bison-mode
     company-flx
     csv-mode
     dockerfile-mode
     emmet-mode
     evil-lion
     evil-quickscope
     evil-text-object-python
     flx
     graphql-mode
     indium
     ob-swift
     ov
     pretty-mode
     rjsx-mode
     rufo
     seeing-is-believing
     toc-org
     traad
     vimish-fold
     )
   dotspacemacs-excluded-packages
   '(
     ace-jump-mode
     auctex-latexmk
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
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 3)
                                (projects . 5)
                                (bookmarks . 5)
                                (agenda . 5)
                                (todos . 5))
   dotspacemacs-scratch-mode 'fundamental-mode
   dotspacemacs-themes '(spacemacs-dark solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code Retina"
                               :size 14
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
  ;; Display Magit full-screen
  (setq-default git-magit-status-fullscreen t)
  ;; Don't create lockfiles
  (setq-default create-lockfiles nil)
  ;; Raise the gc threshold
  (setq-default gc-cons-threshold 100000000)
  ;; add a space to the right of line numbers
  (setq-default left-fringe-width 10)
  ;; don't warn about large files
  (setq-default large-file-warning-threshold nil)
  ;; prevent visual selection from overriding system clipboard
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; disable mouse support in terminal
  (xterm-mouse-mode -1)
  ;; By default, don't soft-wrap lines longer than line length
  (set-default 'truncate-lines nil)
  ;; Add local packages directory to load path
  (add-to-list 'load-path (format "%s/.spacemacs.d/local" (getenv "HOME"))))

(defun dotspacemacs/user-config ()
  "Load configuration after layer initialization."
  (config/frames)
  (config/global-modes)

  (config/compilation-buffers)
  (config/elm)
  (config/elixir)
  (config/evil-cleverparens)
  (config/evil-in-ex-buffer)
  (config/evil-lion)
  (config/exercism)
  (config/firacode)
  (config/flycheck)
  (config/version-control)
  (config/highlight-lines-at-length 80)
  (config/highlight-sexp)
  (config/ivy)
  (config/javascript-modes)
  (config/latex-mode)
  (config/markdown-mode)
  (config/org-latex-preview)
  (config/org-mode)
  (config/projectile)
  (config/python)
  (config/ruby-autoformatter)
  (config/ruby-in-buffer-eval)
  (config/set-terminal-emacs-theme)
  (config/terminal-buffers)
  (config/underscore-to-word-char-list)
  (config/code-folding)
  (config/web-beautify)
  (config/web-mode)
  (config/window-splitting)
  (config/yankee)

  (config/diminish)
  (config/prettify-symbols)

  (setq-default ispell-program-name "/usr/local/bin/ispell")

  ;; leader-fp to open file at point
  (spacemacs/set-leader-keys "fp" #'find-file-at-point)

  (if (boundp 'company-backends)
      (add-to-list 'company-backends 'company-restclient)
    (error "Failed adding REST client to company backends"))
  ;; Workaround for long line crashes
  (setq-default search-invisible t)
  ;; Display and copy buffer-file's path
  (spacemacs/declare-prefix "fd" "files/display")
  (spacemacs/set-leader-keys "fdp" 'display-and-copy-file-path)

  ;; execute local configuration file last
  (config/load-local-config))

(defun config/frames ()
  "Configure GUI Emacs frames."
  ;; Open full-height, on left half of screen
  (let ((midpoint (/ (x-display-pixel-width) 2)))
    (progn
      (add-to-list 'default-frame-alist '(ns-appearance . dark))
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (set-frame-height (selected-frame) (x-display-pixel-height) :pixelwise t)
      (set-frame-width (selected-frame) midpoint :pixelwise t)
      (set-frame-position (selected-frame) midpoint 0))))

(defun config/global-modes ()
  "Enable globally set modes."
  (global-company-mode)
  (with-eval-after-load 'company
    (company-flx-mode +1))

  (global-evil-quickscope-mode 1)
  (rbenv-use-global)
  (smartparens-global-strict-mode)
  (visual-line-mode)

  ;; Display time in the modeline by default
  (setq-default display-time-default-load-average nil
                display-time-format "%m/%d %l:%M %p")
  (spacemacs/toggle-display-time-on))

(defun config/window-splitting ()
  "Make focus window commands primary."
  (spacemacs/set-leader-keys "ws" #'split-window-below-and-focus)
  (spacemacs/set-leader-keys "wS" #'split-window-below)
  (spacemacs/set-leader-keys "wv" #'split-window-right-and-focus)
  (spacemacs/set-leader-keys "wV" #'split-window-right)
  (spacemacs/set-leader-keys "wT" #'split-term-window-right-and-focus))

(defun config/evil-in-ex-buffer ()
  "Emacs bindings in Evil ex minibuffer."
  (if (boundp 'evil-ex-completion-map)
      (progn
        (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
        (define-key evil-ex-completion-map (kbd "C-k") 'kill-line)
        (define-key evil-ex-completion-map (kbd "C-a") 'beginning-of-line))
    (error "Failed setting up ex mode keybindings")))

(defun config/compilation-buffers ()
  "Configure compilation buffer settings."
  (defun compilation-mode-settings ()
    ;; wrap lines in compilation buffer
    (setq truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'compilation-mode-hook #'compilation-mode-settings))

(defun config/python ()
  "Configure python and related modes."
  (let* ((conda-path (format "%s/.anaconda" (getenv "HOME")))
         (python-path (format "%s/bin" conda-path)))
    (setenv "WORKON_HOME" (format "%s/envs" conda-path))
    (setq-default conda-anaconda-home conda-path
                  exec-path (cons python-path exec-path)))

  (setq-default python-guess-indent nil
                python-indent-offset 4
                python-shell-completion-native-enable t
                python-shell-interpreter "ipython"
                python-shell-interpreter-args "-i --simple-prompt")

  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'evil-text-object-python-add-bindings)

  (if (boundp 'company-backends)
      (add-to-list 'company-backends 'company-anaconda)
    (error "Failed setting up anaconda autocomplete"))

  ;; Register Pipenv project type with projectile
  (projectile-register-project-type 'python-pipenv '("Pipfile")
                                    :compile "pipenv run compile"
                                    :test "pipenv run test"
                                    :test-suffix "_test")

  (defun python-before-save-hooks ()
    (if (and (eq major-mode 'python-mode)
             (not (string-match-p "kizen" (buffer-file-name))))
      (progn
        (spacemacs/python-remove-unused-imports)
        (py-isort-buffer)
        (yapfify-buffer))))

  (add-hook 'before-save-hook #'python-before-save-hooks))

(defun config/set-terminal-emacs-theme ()
  "Set theme for terminal session."
  (if (not (display-graphic-p))
      (spacemacs/load-theme 'spacemacs-dark)))

(defun config/projectile ()
  "Configure Projectile."
  (setq-default projectile-completion-system 'ivy
                projectile-enable-caching t
                projectile-find-dir-includes-top-level t)

  (if (bound-and-true-p projectile-globally-ignored-directories)
      (setq-default projectile-globally-ignored-directories
                    (append projectile-globally-ignored-directories
                            '("node_modules")))
    (error "Failed appending to projectile-globally-ignored-directories")))

(defun config/ivy ()
  "Configure Ivy."
  (setq-default ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-fuzzy)
                                        (mx . ivy--regex-fuzzy)
                                        (swiper . ivy--regex-plus)
                                        (counsel-git-grep . ivy--regex-fuzzy)
                                        (t . ivy--regex-fuzzy))
                ivy-initial-inputs-alist nil))

(defun config/web-mode ()
  "Configure web-mode (for CSS, HTML)."
  (setq-default css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)

  (with-eval-after-load 'web-mode
    (if (boundp 'web-mode-indentation-params)
        (progn
          (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
          (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
          (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
      (error "Failed setting up web-mode indentation params"))))

(defun config/javascript-modes ()
  "Configure JavaScript modes: js, js2, react."
  (setq-default js-indent-level 2
                js2-strict-missing-semi-warning nil)

  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  (with-eval-after-load 'web-mode
    (if (boundp 'web-mode-indentation-params)
        (progn
          (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
          (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
          (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
      (error "Failed setting up js-modes indentation params")))

  (defun rjsx-hybrid-keybindings ()
    "Bind C-d to `rjsx-delete-creates-full-tag'."
    (if (bound-and-true-p evil-hybrid-state-map)
        (define-key evil-hybrid-state-map (kbd "C-d") #'rjsx-delete-creates-full-tag)
      (error "Failed defining RJSX hybrid state keybindings")))
  (add-hook 'rjsx-mode-hook #'rjsx-hybrid-keybindings)

  (defun js-standard-fix ()
    (interactive)
    (shell-command-on-region
     (point-min) (point-max)
     (format "standard --fix %s" (buffer-file-name)))))

(defun config/web-beautify ()
  "Configure web-beautify hooks."

  (defun web-beautify/beautify-js-buffer-on-save ()
    "Add a before-save hook to beautify JavaScript on save."
    (add-hook 'before-save-hook 'web-beautify-js-buffer t t))

  (defun web-beatify/beautify-html-buffer-on-save ()
    "Add a before-save hook to beautify HTML on save."
    (add-hook 'before-save-hook 'web-beautify-html-buffer t t))

  (defun web-beatify/beautify-css-buffer-on-save ()
    "Add a before-save hook to beautify CSS on save."
    (add-hook 'before-save-hook 'web-beautify-css-buffer t t))

  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook #'web-beautify/beautify-js-buffer-on-save))
  (eval-after-load 'js
    '(add-hook 'js-mode-hook #'web-beautify/beautify-js-buffer-on-save))
  (eval-after-load 'json-mode
    '(add-hook 'json-mode-hook #'web-beautify/beautify-js-buffer-on-save))
  (eval-after-load 'html-mode
    '(add-hook 'html-mode-hook #'web-beautify/beautify-html-buffer-on-save))
  (eval-after-load 'css-mode
    '(add-hook 'css-mode-hook #'web-beautify/beautify-css-buffer-on-save)))

(defun config/code-folding ()
  "Configure code folding settings."
  (if (and (boundp 'evil-visual-state-map)
           (boundp 'evil-normal-state-map)
           (boundp 'evil-fold-list))
      (progn
        ;; Toggle folds with TAB
        (define-key evil-normal-state-map (kbd "TAB") #'evil-toggle-fold)
        ;; Set up vimish-fold
        (add-to-list 'evil-fold-list '((vimish-fold-mode)
                                       :open-all   vimish-fold-unfold-all
                                       :close-all  nil
                                       :toggle     vimish-fold-toggle
                                       :open       vimish-fold-unfold
                                       :open-rec   nil
                                       :close      vimish-fold))
        (define-key evil-visual-state-map (kbd "zf") #'vimish-fold)
        (define-key evil-normal-state-map (kbd "zf") nil)
        (define-key evil-normal-state-map (kbd "zfd") #'vimish-fold-delete)
        (define-key evil-normal-state-map (kbd "zfj") #'vimish-fold-next-fold)
        (define-key evil-normal-state-map (kbd "zfk") #'vimish-fold-previous-fold)
        (define-key evil-normal-state-map (kbd "zfm") #'vimish-fold-refold-all)
        (define-key evil-normal-state-map (kbd "zfr") #'vimish-fold-unfold-all)
        (define-key evil-normal-state-map (kbd "zft") #'vimish-fold-toggle-all)
        (define-key evil-normal-state-map (kbd "zfa") #'vimish-fold-toggle))
    (error "Failed setting up vimish-fold")))

(defun config/diminish ()
  "Configure diminish glyphs for various minor modes."
  (with-eval-after-load 'diminish
    (diminish 'alchemist-mode "⊛")
    (diminish 'alchemist-phoenix-mode "⊙")
    (diminish 'elm-indent-mode "⨕")
    (diminish 'highlight-sexp-mode "⋂")
    (diminish 'minitest-mode "⨷")
    (diminish 'rubocop-mode "℞")
    (diminish 'ruby-refactor-mode "RR")
    (diminish 'rufo-minor-mode "℞℞")
    (diminish 'seeing-is-believing "S")
    (diminish 'tern-mode "₸")))

(defun config/elm ()
  "Configure Elm."
  (with-eval-after-load 'elm-mode
    (remove-hook 'elm-mode-hook 'elm-indent-mode))

  (if (boundp 'company-backends)
      (add-to-list 'company-backends 'company-elm)
    (error "Failed setting up Elm auto-completion")))

(defun config/elixir ()
  "Configure Elixir mode."
  (setq-default flycheck-elixir-credo-strict t)

  (defun elixir-format-buffer ()
    (interactive)
    (shell-command-on-region
     (point-min) (point-max)
     (format "mix format %s" (buffer-file-name))))

  ;; mix-format interface
  (defun elixir-after-save-hooks ()
    (if (eq major-mode 'elixir-mode)
        (elixir-format-buffer)))
  (add-hook 'after-save-hook #'elixir-after-save-hooks))

(defun config/ruby-autoformatter ()
  "Configure autoformatter for Ruby mode."
  ;; Enable rufo-format
  (add-hook 'ruby-mode-hook 'rufo-minor-mode)
  ;; Enable autoformat on save in Ruby modes
  (add-hook 'ruby-mode-hook (lambda () (add-hook 'before-save-hook #'rufo-format t)))
  ;; Define keybinding to manually trigger autoformat
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode "=" #'rufo-format))

(defun config/ruby-in-buffer-eval ()
  "Configure and enable seeing-is-believing and xmpfilter for Ruby."
  (require 'seeing-is-believing)
  (add-hook 'ruby-mode-hook 'seeing-is-believing)

  (defun xmpfilter-eval-current-line ()
    (interactive)
    (seeing-is-believing-mark-current-line-for-xmpfilter)
    (seeing-is-believing-run-as-xmpfilter))

  (defun define-xmpfilter-keybindings ()
    "Define keybindings for xmpfilter."
    (if (boundp 'ruby-mode-map)
        (progn
          (define-key ruby-mode-map (kbd "C-c C-c") 'xmpfilter-eval-current-line)
          (define-key ruby-mode-map (kbd "C-c C-v") 'seeing-is-believing-clear)
          (define-key ruby-mode-map (kbd "C-c C-f") 'seeing-is-believing-run))
      (error "Failed setting up xmpfilter keybindings")))

  (add-hook 'ruby-mode-hook 'define-xmpfilter-keybindings))

(defun config/markdown-mode ()
  "Configure Markdown mode."
  (defun alchemist-iex-send-current-code-block ()
    "Send the current Markdown code block to iex."
    (interactive)
    (save-excursion
      (re-search-backward "```")
      (forward-line)
      (push-mark)
      (re-search-forward "```")
      (forward-line -1)
      (move-end-of-line nil)
      (alchemist-iex-send-region (region-beginning) (region-end))
      (pop-mark)))

  ;; markdown mode leader keybindings
  (spacemacs/declare-prefix-for-mode 'markdown-mode "e" "markdown/evaluate")
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "ee" #'alchemist-iex-send-current-code-block)

  (setq-default markdown-asymmetric-header t))

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

(defun config/terminal-buffers ()
  "Configure terminal buffers."
  (evil-set-initial-state 'term-mode 'insert)

  (defun term-send-ctrl-y ()
    (interactive)
    (term-send-raw-string "\C-y"))

  (defun term-mode-config ()
    (if (boundp 'term-raw-map)
        (progn
          (define-key term-raw-map (kbd "C-y") #'term-send-ctrl-y)
          (define-key term-raw-map (kbd "C-p") #'term-send-up)
          (define-key term-raw-map (kbd "C-n") #'term-send-down)
          (define-key term-raw-map (kbd "C-v") #'term-paste)
          (goto-address-mode))
      (error "Failed setting up term mode keybindings")))

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
  (add-hook 'text-mode-hook #'add-underscore-to-word-chars)
  (add-hook 'python-mode-hook #'add-underscore-to-word-chars)
  (add-hook 'markdown-mode-hook #'add-underscore-to-word-chars)
  (add-hook 'org-mode-hook #'add-underscore-to-word-chars))

(defun config/yankee ()
  "Load and configure yankee.el.
Provides facilities for yanking formatted code snippets."
  (load "yankee.el/yankee.el")
  (require 'yankee)
  (if (boundp 'evil-visual-state-map)
      (progn
        (define-key evil-visual-state-map (kbd "gy") nil)
        (define-key evil-visual-state-map (kbd "gym") #'yankee/yank-as-gfm-code-block)
        (define-key evil-visual-state-map (kbd "gyf") #'yankee/yank-as-gfm-code-block-folded)
        (define-key evil-visual-state-map (kbd "gyo") #'yankee/yank-as-org-code-block)
        (define-key evil-visual-state-map (kbd "gyj") #'yankee/yank-as-jira-code-block))
    (error "Failed setting up yankee.el keybindings")))

(defun config/version-control ()
  "Configure version-control-related settings."
  (with-eval-after-load 'magit
    (if (and (boundp 'magit-completing-read-function)
             (boundp 'magit-mode-map))
        (progn
          (setq magit-completing-read-function 'ivy-completing-read)
          (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)
          (magit-define-popup-switch 'magit-log-popup ?m "Omit merge commits" "--no-merges"))
      (error "Failed setting up magit")))

  (setq-default
   magit-repository-directories '(
                                  ("~/Projects/" . 2)
                                  ("~/Documents". 2)
                                  ))

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
                                 (dot . t)
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

    ;; Org Babel: Elixir
    (if (boundp 'org-babel-default-header-args:elixir)
        (progn
          (setq org-babel-default-header-args:elixir
                (cons '(:results . "value")
                      (assq-delete-all :results org-babel-default-header-args:elixir)))
          (setq org-babel-default-header-args:elixir
                (cons '(:preamble . "Code.compiler_options(ignore_module_conflict: true)")
                      (assq-delete-all :preamble org-babel-default-header-args:elixir))))
      (error "Failed setting up org-babel for Elixir"))

    ;; Source Blocks
    ;; Python: <p
    (if (boundp 'org-structure-template-alist)
        (progn
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
                         "<src lang=\"emacs-lisp\">\n?\n</src>")))
      (error "Failed setting up org-babel source block"))))

(defun config/evil-lion ()
  "Configure evil-lion alignment text objects."
  ;; align (e.g.: gaip=, gaip/)
  (if (and (boundp 'evil-normal-state-map)
           (boundp 'evil-visual-state-map))
      (progn
        (define-key evil-normal-state-map (kbd "ga") #'evil-lion-left)
        (define-key evil-normal-state-map (kbd "gA") #'evil-lion-right)
        (define-key evil-visual-state-map (kbd "ga") #'evil-lion-left)
        (define-key evil-visual-state-map (kbd "gA") #'evil-lion-right))
    (error "Failed setting up evil-lion alignment keybindings")))

(defun config/exercism ()
  "Configure and enable exercism mode."
  (setq-default exercism-dir "~/Projects/exercism")
  (setq-default exercism-auto-enable nil)
  (setq-default exercism-config-file "~/.config/exercism/user.json")
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

  (pretty-activate-groups '(:greek :arithmetic-nary))

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

  ;; prettify symbols: Elixir
  (defun config/prettify-symbols-elixir()
    "Provide prettify-symbol mode mappings for elixir-mode."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("def" .  #x0192)
            ("defp" .  #x0070)
            ("defmodule" . #x006D)
            ("fn" . #x03BB))))
  (add-hook 'elixir-mode-hook #'config/prettify-symbols-elixir)

  ;; prettify symbols: JavaScript
  (defun config/prettify-symbols-javascript ()
    "Provide prettify-symbol mode mappings for javascript modes."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("function" .  #x0192))))
  (add-hook 'js2-mode-hook #'config/prettify-symbols-javascript))

(defun config/org-latex-preview ()
  "Configure LaTeX preview settings for Org mode."
  (with-eval-after-load 'org
    (if (boundp 'org-format-latex-options)
        (setq org-format-latex-options
              (plist-put org-format-latex-options :justify 'center)))

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
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)
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

(defun display-and-copy-file-path ()
  "Print the path of the current buffer's file.
Depends on yankee.el."
  (interactive)
  (let ((file-path (yankee--abbreviated-project-or-home-path-to-file)))
    (kill-new file-path)
    (message file-path)))

(defun config/load-local-config ()
  "Load local configuration overrides."
  (load "~/.init.local.el"))

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
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-text-object-python yasnippet-snippets yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vmd-mode vimrc-mode vimish-fold vi-tilde-fringe uuidgen use-package unfill traad toc-org tagedit symon string-inflection sql-indent spray spaceline-all-the-icons solarized-theme smex smeargle slim-mode shell-pop seeing-is-believing scss-mode sass-mode rvm rufo ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe rjsx-mode reveal-in-osx-finder restart-emacs rbenv rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode projectile-rails pretty-mode prettier-js popwin pony-mode pippel pipenv pip-requirements persp-mode password-generator paradox ox-gfm overseer ov osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file ob-swift ob-restclient ob-http nameless mwim multi-term move-text mmm-mode minitest markdown-toc magithub magit-svn magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode link-hint launchctl json-navigator json-mode js-doc ivy-yasnippet ivy-xref ivy-purpose ivy-hydra indium indent-guide importmagic impatient-mode hyde hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-make graphviz-dot-mode graphql-mode google-translate golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy font-lock+ flyspell-correct-ivy flycheck-pos-tip flycheck-gometalinter flx-ido fill-column-indicator feature-mode eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-quickscope evil-org evil-numbers evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help engine-mode emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-modeline dockerfile-mode diminish diff-hl deft dash-at-point dactyl-mode cython-mode csv-mode counsel-projectile counsel-dash counsel-css company-web company-tern company-statistics company-restclient company-quickhelp company-go company-flx company-auctex company-anaconda command-log-mode column-enforce-mode clean-aindent-mode chruby centered-cursor-mode bundler browse-at-remote bison-mode auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent add-node-modules-path ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
