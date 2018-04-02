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
     (elm :variables
          elm-sort-imports-on-save t
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
     groovy
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
     traad
     dockerfile-mode
     evil-quickscope
     ob-swift
     ov
     pretty-mode
     s
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
     auctex-latexmk
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
  (config/latex-mode)
  (config/markdown-mode)
  (config/ruby-in-buffer-eval)
  (config/diminish)
  (config/vimish-fold)
  (config/web-beautify)
  (config/javascript-modes)
  (config/web-mode)
  (config/ivy-and-projectile)
  (config/spaceline)
  (config/set-terminal-emacs-theme)
  (config/exec-path)
  (config/compilation-buffers)
  (config/python)

  (smartparens-global-strict-mode)

  ;; Don't create lockfiles
  (setq create-lockfiles nil)

  ;; Raise the gc threshold
  (setq gc-cons-threshold 100000000)

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

  ;; make focus window commands primary
  (spacemacs/set-leader-keys "ws" #'split-window-below-and-focus)
  (spacemacs/set-leader-keys "wS" #'split-window-below)
  (spacemacs/set-leader-keys "wv" #'split-window-right-and-focus)
  (spacemacs/set-leader-keys "wV" #'split-window-right)
  (spacemacs/set-leader-keys "wT" #'split-term-window-right-and-focus)
  (spacemacs/declare-prefix "fd" "files/display")
  (spacemacs/set-leader-keys "fdp" 'display-and-copy-file-path)
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode "x#" 'ruby-tools-interpolate)

  ;; Emacs bindings in Evil ex minibuffer
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-k") 'kill-line)
  (define-key evil-ex-completion-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-normal-state-map (kbd "TAB") #'evil-toggle-fold)

  ;; enable evil-quickscope-mode
  (global-evil-quickscope-mode 1)

  ;; Objective-C
  (add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))
  ;; Ruby
  (rbenv-use-global)
  ;; Golang
  (add-hook 'go-mode-hook '(lambda () (whitespace-toggle-options 'tabs)))
  ;; Haskell
  (add-hook 'haskell-mode-hook 'intero-mode)

  ;; execute local configuration file last
  (config/load-local-config))

(defun config/compilation-buffers ()
  "Configure compilation buffer settings."
  (defun compilation-mode-settings ()
    ;; wrap lines in compilation buffer
    (setq truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'compilation-mode-hook #'compilation-mode-settings))

(defun config/exec-path ()
  "Set up the `exec-path'."
  ;; Copy exec-path from shell PATH if in GUI emacs
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(defun config/python ()
  "Configure python and related modes."
  ;; Python: Explicitly prepend python binaries location to exec-path.
  ;; Fixes flake8, yapfify failure to load.
  (let* ((conda-path (format "%s/.anaconda3/" (getenv "HOME")))
         (python-path (format "%s/bin" conda-path)))
    (setenv "WORKON_HOME" (format "%s/envs" conda-path))
    (setq-default exec-path (cons python-path exec-path)))

  (setq-default python-shell-completion-native-enable t)
  (setq-default python-indent-offset 4)
  (setq-default python-shell-interpreter-args "-i --no-banner --simple-prompt")
  (add-to-list 'company-backends 'company-anaconda)

  (add-hook 'after-save-hook 'spacemacs//python-sort-imports)
  (add-hook 'after-save-hook 'spacemacs/python-remove-unused-imports))

(defun config/set-terminal-emacs-theme ()
  "Set theme for terminal session."
  (if (not (display-graphic-p))
      (spacemacs/load-theme 'spacemacs-dark)))

(defun config/spaceline ()
  "Configure the spaceline."
  (with-eval-after-load 'spaceline
    (spaceline-define-segment version-control
      "Only display the current branch name in mode line."
      (when vc-mode
        (powerline-raw
         (s-trim (replace-regexp-in-string "Git[:-]" "" vc-mode)))))
    (setq-default powerline-default-separator nil)
    (spaceline-compile)))

(defun config/ivy-and-projectile ()
  "Configure Ivy and Projectile for fuzzy-searching and project utilities."
  ;; Projectile settings
  (setq-default projectile-completion-system 'ivy
                projectile-enable-caching t
                projectile-globally-ignored-directories '("node_modules"))

  ;; Ivy settings
  (setq-default ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-fuzzy)
                                        (mx . ivy--regex-fuzzy)
                                        (swiper . ivy--regex-plus)
                                        (counsel-git-grep . ivy--regex-fuzzy)
                                        (t . ivy--regex-fuzzy))
                ivy-initial-inputs-alist nil
                ivy-wrap t))

(defun config/web-mode ()
  "Configure web-mode (for CSS, HTML)."
  (setq-default css-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))))

(defun config/javascript-modes ()
  "Configure JavaScript modes: js, js2, react."
  (setq-default js-indent-level 2
                js2-basic-offset 2
                js2-strict-missing-semi-warning nil)

  (defun js-standard-fix ()
    (interactive)
    (shell-command-on-region
     (point-min) (point-max)
     (format "standard --fix %s" (buffer-file-name)))))

(defun config/web-beautify ()
  "Configure web-beautify hooks."

  (defun web-beatify/beautify-js-buffer-on-save ()
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

(defun config/vimish-fold ()
  "Configure vimish-fold and associated keybindings."

  (add-to-list 'evil-fold-list '((vimish-fold-mode)
                                 :open-all   vimish-fold-unfold-all
                                 :close-all  nil
                                 :toggle     vimish-fold-toggle
                                 :open       vimish-fold-unfold
                                 :open-rec   nil
                                 :close      vimish-fold))

  (define-key evil-normal-state-map (kbd "zf") nil)
  (define-key evil-visual-state-map (kbd "zf") #'vimish-fold)
  (define-key evil-normal-state-map (kbd "zfd") #'vimish-fold-delete)
  (define-key evil-normal-state-map (kbd "zfj") #'vimish-fold-next-fold)
  (define-key evil-normal-state-map (kbd "zfk") #'vimish-fold-previous-fold)
  (define-key evil-normal-state-map (kbd "zfm") #'vimish-fold-refold-all)
  (define-key evil-normal-state-map (kbd "zfr") #'vimish-fold-unfold-all)
  (define-key evil-normal-state-map (kbd "zft") #'vimish-fold-toggle-all)
  (define-key evil-normal-state-map (kbd "zfa") #'vimish-fold-toggle))

(defun config/diminish ()
  "Configure diminish glyphs for various minor modes."
  (with-eval-after-load 'diminish
    (diminish 'alchemist-mode "⊛")
    (diminish 'alchemist-phoenix-mode "⊙")
    (diminish 'elm-indent-mode "⨕")
    (diminish 'highlight-sexp-mode "⋂")
    (diminish 'minitest-mode "⨷")
    (diminish 'rubocop-mode "℞")
    (diminish 'seeing-is-believing "S"))
    (diminish 'tern-mode "₸"))

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
    (define-key ruby-mode-map (kbd "C-c C-c") 'xmpfilter-eval-current-line)
    (define-key ruby-mode-map (kbd "C-c C-v") 'seeing-is-believing-clear)
    (define-key ruby-mode-map (kbd "C-c C-f") 'seeing-is-believing-run))

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
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "ee" #'alchemist-iex-send-current-code-block))

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
  (add-hook 'python-mode-hook #'add-underscore-to-word-chars)
  (add-hook 'markdown-mode-hook #'add-underscore-to-word-chars)
  (add-hook 'org-mode-hook #'add-underscore-to-word-chars))

(defun config/company ()
  "Enable and configure company mode."
  (global-company-mode))

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
    (yasnippet-snippets yapfify yaml-mode xterm-color ws-butler winum wgrep web-mode web-beautify volatile-highlights vmd-mode vimrc-mode vimish-fold vi-tilde-fringe uuidgen unfill traad virtualenvwrapper request-deferred toc-org tagedit symon swift-mode string-inflection sql-indent spaceline-all-the-icons all-the-icons memoize spaceline powerline solarized-theme smex smeargle slim-mode shell-pop seeing-is-believing scss-mode sayid sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe reveal-in-osx-finder restart-emacs request realgud test-simple loc-changes load-relative rbenv rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode projectile-rails rake pretty-mode popwin pippel pipenv pip-requirements persp-mode pcre2el pbcopy password-generator paradox ox-gfm overseer ov osx-trash osx-dictionary orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-bullets org-brain open-junk-file ob-swift ob-restclient ob-http nameless mwim multi-term move-text mmm-mode minitest markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode skewer-mode live-py-mode linum-relative link-hint less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc ivy-xref ivy-rtags ivy-purpose window-purpose imenu-list ivy-hydra intero indent-guide importmagic epc ctable concurrent deferred impatient-mode simple-httpd hyde hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-make haskell-snippets haml-mode graphviz-dot-mode google-translate google-c-style golden-ratio godoctor go-tag go-rename go-guru go-eldoc gnuplot gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md fuzzy flyspell-correct-ivy flyspell-correct flycheck-rtags flycheck-pos-tip flycheck-mix flycheck-haskell flycheck-gometalinter flycheck-elm flycheck-credo flx-ido flx fill-column-indicator feature-mode eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-quickscope evil-org evil-numbers evil-matchit evil-magit magit magit-popup git-commit ghub let-alist with-editor evil-lisp-state evil-lion evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens smartparens evil-args evil-anzu anzu eshell-z eshell-prompt-extras esh-help engine-mode emmet-mode elm-mode elisp-slime-nav editorconfig dumb-jump dockerfile-mode disaster diff-hl deft dash-at-point dante lcr flycheck dactyl-mode cython-mode counsel-projectile projectile counsel-dash helm-dash counsel-css counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-statistics company-rtags rtags company-restclient restclient know-your-http-well company-quickhelp pos-tip company-go go-mode company-ghci company-ghc ghc haskell-mode company-cabal company-c-headers company-auctex company-anaconda command-log-mode column-enforce-mode coffee-mode cmm-mode clojure-snippets clojure-cheatsheet helm helm-core clj-refactor inflections edn multiple-cursors paredit peg clean-aindent-mode clang-format cider-eval-sexp-fu eval-sexp-fu highlight cider seq spinner queue clojure-mode chruby centered-cursor-mode bundler inf-ruby browse-at-remote auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex anaconda-mode pythonic f alchemist s company dash elixir-mode pkg-info epl aggressive-indent adaptive-wrap ace-window ace-link avy ac-ispell auto-complete popup which-key use-package org-plus-contrib hydra font-lock+ exec-path-from-shell evil goto-chg undo-tree diminish bind-map bind-key async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
