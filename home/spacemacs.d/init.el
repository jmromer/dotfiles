;;; spacemacs.d/init.el -- Spacemacs configuration
;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;; Commentary:
;; This file is loaded by Spacemacs at startup.

;;; Code:
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-sort-by-usage t
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 2
                      auto-completion-idle-delay 0.1
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets"
                      auto-completion-enable-help-tooltip t)
     better-defaults
     bm
     (c-c++ :variables
            c-c++-enable-clang-support t)
     command-log
     copy-as-format
     dash
     deft
     django
     docker
     elixir
     (elm :variables
          elm-sort-imports-on-save t
          elm-format-on-save t
          elm-format-command "elm-format")
     emacs-lisp
     evil-commentary
     finance
     git
     github
     (go :variables
         gofmt-command "goimports"
         go-tab-width 4
         go-use-gometalinter t)
     graphviz
     (gtags :variables
            gtags-enable-by-default t)
     (html :variables
           web-fmt-tool 'prettier)
     imenu-list
     ipython-notebook
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     (ivy :variables
          ivy-enable-advanced-buffer-information t
          ivy-wrap t)
     (javascript :variables
                 javascript-backend 'tern
                 javascript-fmt-tool 'prettier
                 node-add-modules-path t)
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-folding t)
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     (mu4e :variables
           mu4e-enable-async-operations t
           mu4e-enable-mode-line t
           mu4e-enable-notifications t
           mu4e-installation-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
           mu4e-spacemacs-kill-layout-on-exit t
           mu4e-spacemacs-layout-binding "m"
           mu4e-spacemacs-layout-name "mail")
     nginx
     (org :variables
          org-projectile-file "TODOS.org"
          org-enable-hugo-support t
          org-enable-org-journal-support t
          org-journal-dir "~/Dropbox/org/journal"
          org-enable-reveal-js-support t
          org-enable-bootstrap-support t
          org-enable-github-support t)
     (osx :variables
          osx-command-as 'hyper
          osx-option-as 'meta
          osx-control-as 'control
          osx-function-as nil
          osx-right-command-as 'left
          osx-right-option-as 'none
          osx-right-control-as 'left)
     parinfer
     pdf
     phoenix
     (python :variables
             python-save-before-test t
             python-test-runner 'pytest
             python-sort-imports-on-save nil
             python-enable-yapf-format-on-save nil)
     prettier
     semantic
     react
     restclient
     (ruby :variables
           ruby-enable-enh-ruby-mode nil
           ruby-version-manager nil
           ruby-test-runner 'rspec)
     ruby-on-rails
     rust
     scala
     search-engine
     (shell :variables
            multi-term-program "/usr/local/bin/zsh"
            shell-default-full-span nil
            shell-default-position 'bottom
            shell-default-shell 'multi-term
            shell-default-term-shell "/usr/local/bin/zsh"
            shell-enable-smart-eshell t)
     shell-scripts
     (slack :variables
            slack-spacemacs-layout-name "slack"
            slack-spacemacs-layout-binding "C")
     spacemacs-layouts
     spacemacs-purpose
     speed-reading
     (sql :variables
          sql-capitalize-keywords t
          sql-auto-indent t)
     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-by-default t
                      syntax-checking-enable-tooltips t)
     (treemacs :variables
               treemacs-use-filewatch-mode t
               treemacs-use-follow-mode t)
     version-control
     vimscript
     vinegar
     w3m
     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     amx
     bison-mode
     company-flx
     company-jedi
     conda
     csv-mode
     direnv
     editorconfig
     emmet-mode
     evil-collection
     evil-ledger
     evil-lion
     evil-quickscope
     evil-text-object-python
     flx
     graphql-mode
     gxref
     indium
     lispy
     magit-todos
     ob-swift
     (ob-elixir :location (recipe :fetcher github :repo "jmromer/ob-elixir" :branch "develop"))
     ov
     (ox-gfm :location (recipe :fetcher github :repo "jmromer/ox-gfm" :branch "develop"))
     pretty-mode
     pyimport
     rjsx-mode
     (rufo :location (recipe :fetcher github :repo "aleandros/emacs-rufo" :branch "master"))
     seeing-is-believing
     toc-org
     traad
     vimish-fold
     (yankee :location (recipe :fetcher github :repo "jmromer/yankee.el" :branch "develop")))

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages
   '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     ace-jump-mode
     emoji-cheat-sheet-plus
     evil-mc
     fancy-battery
     google-translate
     rbenv
     rvm)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

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

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-check-for-update nil
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings t
                                       hybrid-mode-default-state 'normal)

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   dotspacemacs-startup-lists '((recents . 3)
                                (projects . 5)
                                (bookmarks . 5)
                                (agenda . 5)
                                (todos . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme 'doom
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code Retina"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-command-key ":"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "org"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.3

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers '(:relative t
                               :enabled-for-modes prog-mode
                               :disabled-for-modes pdf-view-mode org-mode doc-view-mode dired-mode
                               :size-limit-kb 1024)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%b"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format "%b"

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Load variables before package initialization."
  ;; Separate server socket location for CLI emacs
  (when (not window-system)
    (setq-default server-socket-dir (getenv "EMACS_SOCKET_DIR")))

  ;; Display Magit full-screen
  (setq-default git-magit-status-fullscreen t)

  ;; set these before loading evil
  (setq-default evil-want-integration t
                evil-want-keybinding nil)

  ;; Don't create lockfiles
  (setq-default create-lockfiles nil)

  ;; add a space to the right of line numbers
  (setq-default left-fringe-width 10)

  ;; don't warn about large files
  ;; (setq-default large-file-warning-threshold nil)
  ;; prevent visual selection from overriding system clipboard
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; disable mouse support in terminal
  (xterm-mouse-mode -1)

  ;; By default, don't soft-wrap lines longer than line length
  (set-default 'truncate-lines nil)

  ;; Add local packages directory to load path
  (add-to-list 'load-path (format "%s/.spacemacs.d/local" (getenv "HOME"))))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Load configuration after layer initialization."
  ;; Pre-config
  (config/frames)
  (config/global-modes)

  (config/amx)
  (config/code-folding)
  (config/company)
  (config/compilation-buffers)
  (config/copy-as-format)
  (config/deft)
  (config/elixir)
  (config/elm)
  (config/email)
  (config/evil-collection)
  (config/evil-goggles)
  (config/evil-in-ex-buffer)
  (config/evil-lion)
  (config/flycheck)
  (config/gtags)
  (config/highlight-lines-at-length 80)
  (config/ivy)
  (config/javascript-modes)
  (config/latex-mode)
  (config/ligatures)
  (config/lisps)
  (config/markdown-mode)
  (config/modeline)
  (config/org-latex-preview)
  (config/org-mode)
  (config/projectile)
  (config/python)
  (config/ruby)
  (config/ruby-in-buffer-eval)
  (config/semantic)
  (config/set-terminal-emacs-theme)
  (config/terminal-buffers)
  (config/underscore-to-word-char-list)
  (config/version-control)
  (config/web-beautify)
  (config/web-mode)
  (config/window-splitting)
  (config/yankee)
  (config/yasnippet)

  ;; Post-config
  (config/diminish)
  (config/prettify-symbols)

  (editorconfig-mode 1)

  ;; tramp
  (setq-default tramp-default-method "ssh")

  ;; dash prefix
  (spacemacs/declare-prefix "d" "docs")

  ;; ledger
  (add-hook 'ledger-mode-hook #'evil-ledger-mode)

  ;; treemacs
  (treemacs-resize-icons 15)

  (setq-default ispell-program-name "/usr/local/bin/ispell")

  ;; leader-fp to open file at point
  (spacemacs/set-leader-keys
    "f p" #'find-file-at-point)

  ;; Display and copy buffer-file's path
  (spacemacs/declare-prefix "f d" "files/display")
  (spacemacs/set-leader-keys
    "f d" nil
    "f d p" #'display-and-copy-file-path)

  (spacemacs/declare-prefix "L" "layouts")
  (spacemacs/set-leader-keys
    "L" nil
    "L r" #'layouts-reset
    "L o" #'layouts-org
    "L d" #'layouts-dotfiles)

  (spacemacs/declare-prefix "E" "edit")
  (spacemacs/set-leader-keys
    "E" nil
    "E i" #'iedit-mode)

  ;; execute local configuration file last
  (config/load-local-config))

;; Overrides

(with-eval-after-load 'doom-modeline-segments
  (defun doom-modeline-update-persp-name (&rest _)
    "Update perspective name in mode-line.
Overrides doom-modeline's version to respect
`dotspacemacs-display-default-layout'."
    (setq doom-modeline--persp-name
          ;; Support `persp-mode', while not support `perspective'
          (when (and doom-modeline-persp-name
                     (bound-and-true-p persp-mode)
                     (fboundp 'safe-persp-name)
                     (fboundp 'get-current-persp))
            (let* ((persp (get-current-persp))
                   (name (safe-persp-name persp)))
              (unless (and (string-equal persp-nil-name name)
                           ;; NB: Added the below
                           (not dotspacemacs-display-default-layout))
                (propertize
                 (format " #%s " name)
                 'face (if (and persp
                                (not (persp-contain-buffer-p (current-buffer) persp)))
                           'doom-modeline-persp-buffer-not-in-persp
                         'doom-modeline-persp-name)
                 'help-echo "mouse-1: Switch perspective, mouse-2: Show help for minor mode"
                 'mouse-face 'mode-line-highlight
                 'local-map (let ((map (make-sparse-keymap)))
                              (define-key map [mode-line mouse-1]
                                #'persp-switch)
                              (define-key map [mode-line mouse-2]
                                (lambda ()
                                  (interactive)
                                  (describe-function 'persp-mode)))
                              map))))))))

;; Custom functions

;; amx

(defun amx/emacs-commands ()
  "Execute amx with a better prompt."
  (interactive)
  (let ((amx-prompt-string "Emacs commands: "))
    (amx)))

(defun amx/amx-major-mode-commands ()
  "Re-execute smex with major mode commands only."
  (interactive)
  (let ((amx-prompt-string (format "%s commands: " major-mode)))
    (amx-major-mode-commands)))

;; company

(defun company-tng-on ()
  "Enable company-tng-defaults, override TAB, RET, S-TAB key bindings."
  (company-tng-configure-default)
  (if (boundp 'company-active-map)
      (let ((keymap company-active-map))
        (define-key keymap [return] #'company-complete)
        (define-key keymap (kbd "RET") #'company-complete)
        (define-key keymap [tab] #'company-complete)
        (define-key keymap (kbd "TAB") #'company-select-next)
        (define-key keymap [backtab] nil)
        (define-key keymap (kbd "S-TAB") nil))
    (error "Not overriding company-tng keybindings")))

;; layout

(defun layouts-reset ()
  "Reset to the default set of layouts."
  (interactive)
  (progn
    ;; Kill other buffers, don't prompt for confirmation.
    (mapc 'kill-buffer
          (mapc 'switch-to-buffer
                (delq (current-buffer) (buffer-list))))
    (layouts-org)
    (layouts-dotfiles)
    (spacemacs/layout-goto-default)))

(defun layouts-org ()
  "Set up org layout."
  (interactive)
  (progn
    (spacemacs/layout-goto-default)
    (delete-other-windows)

    ;; Kill the journal buffer, since it might need to visit a new file
    (when (get-buffer "*journal*")
      (kill-buffer "*journal*"))

    (find-file (expand-file-name org-default-notes-file))
    (rename-buffer "*sprint*")
    (find-file (expand-file-name org-default-backlog-file))
    (rename-buffer "*backlog*")
    (org-journal-today)
    (rename-buffer "*journal*")
    (delete-other-windows)

    (switch-to-buffer "*sprint*")
    (split-window-right-and-focus)
    (switch-to-buffer "*backlog*")
    (split-window-below-and-focus)
    (switch-to-buffer "*journal*")

    (select-window (get-buffer-window "*sprint*"))
    (split-window-below-and-focus)
    (org-agenda-list)
    (rename-buffer "*agenda*")

    (select-window (get-buffer-window "*sprint*"))))

(defun layouts-dotfiles ()
  "Set up dotfiles layout."
  (interactive)
  (progn
    (persp-switch "dotfiles")
    (delete-other-windows)
    (spacemacs/find-dotfile)
    (rename-buffer "*init.el*")))

;; mu4e

(defun mu4e-save-message-at-point (&optional dir)
  "Save message at point to somewhere else as <date>_<subject>.eml.
Optionally save at DIR (default value: ~/Desktop)."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (target (format "%s_%s.eml"
                         (format-time-string "%F" (mu4e-message-field msg :date))
                         (or (mu4e-message-field msg :subject) "No subject"))))
    (copy-file
     (mu4e-message-field msg :path)
     (format "%s/%s"
             (or dir (read-directory-name "Copy message to: " "~/Desktop"))
             target)
     1)))

;; Org mode

(defun org-journal-find-location ()
  "Open today's journal entry."
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(defun org-capture-deft-new-file ()
  "Open a new deft notes file, prompting for the file name."
  (setq org-capture-deft--title (read-string "Title: ")
        org-capture-deft--timestamp (format-time-string "%Y%m%d%H%M%S"))

  (if (and (boundp 'org-capture-deft--title)
           (not (string-blank-p org-capture-deft--title)))
    (deft-new-file-named
      (downcase (replace-regexp-in-string " " "-" org-capture-deft--title)))
    (deft-new-file)))

(defun org-journal-today ()
  "Open today's journal."
  (interactive)
  (org-journal-find-location)
  (goto-char (point-max)))

(defun org-notes-open-sprint ()
  "Open the sprint file."
  (interactive)
  (if (boundp 'org-default-notes-file)
      (find-file org-default-notes-file)
    (error "No `org-default-notes-file' set")))

(defun org-notes-open-backlog ()
  "Open the backlog file."
  (interactive)
  (if (boundp 'org-default-backlog-file)
      (find-file org-default-backlog-file)
    (error "No `org-default-backlog-file' set")))

(defun org-hugo-new-post-capture-template ()
  "Return `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
  (save-match-data
    (let* ((title (read-from-minibuffer "Post Title: "))
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat "   :EXPORT_FILE_NAME: " fname)
                   "   :END:"
                   "   %?\n")
                 "\n"))))

;; yasnippet

(defun yas/camelcase-file-name ()
  "Camel-case the current buffer's file name."
  (interactive)
  (let ((filename
         (file-name-nondirectory (file-name-sans-extension
                                  (or (buffer-file-name)
                                      (buffer-name (current-buffer)))))))
    (mapconcat #'capitalize (split-string filename "[_\-]") "")))

(defun yas/strip (str)
  "Extract a parameter name from STR."
  (replace-regexp-in-string ":.*$" ""
   (replace-regexp-in-string "^\s+" ""
    (replace-regexp-in-string "," ""
     str))))

(defun yas/to-field-assignment (str)
  "Make 'STR' to 'self.`STR` = `STR`'."
  (format "self.%s = %s" (yas/strip str) (yas/strip str)))

(defun yas/prepend-colon (str)
  "Make `STR' to :`STR'."
  (format ":%s" (yas/strip str)))

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
  "Extract an args list from the current line."
  (interactive)
  (string-match "\(.+\)" (thing-at-point 'line t)))

(defun yas/to-ruby-accessors (str)
  "Splits STR into an `attr_accesor' statement."
  (interactive)
  (mapconcat 'yas/prepend-colon (split-string str ",") ", "))

(defun yas/to-ruby-setters (str)
  "Splits STR into a sequence of field assignments."
  (interactive)
  (mapconcat 'yas/to-field-assignment
             (split-string str ",")
             (yas/indented-newline)))

;; Utilities

(defun display-and-copy-file-path ()
  "Print the path of the current buffer's file.
Depends on yankee.el."
  (interactive)
  (let ((file-path (yankee--abbreviated-project-or-home-path-to-file)))
    (kill-new file-path)
    (message file-path)))

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

(defun config/load-local-config ()
  "Load local configuration overrides."
  (load "~/.init.local.el"))

;; Config Functions

(defun config/amx ()
  "Configure amx keybindings."
  (if (boundp 'evil-visual-state-map)
      (progn
        (define-key evil-visual-state-map (kbd ",:") #'amx/amx-major-mode-commands)))
  (if (boundp 'evil-normal-state-map)
      (progn
        (define-key evil-normal-state-map (kbd ",:") #'amx/amx-major-mode-commands))))

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

(defun config/company ()
  "Configure company auto-completion mode."
  (with-eval-after-load 'company
    (progn
      (company-flx-mode +1)
      (add-hook 'text-mode-hook #'company-mode-on)
      (add-hook 'text-mode-hook #'company-tng-on)

      ;; (if (boundp 'company-backends)
      ;;     (progn
      ;;       (add-to-list 'company-backends '(company-anaconda :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-capf :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-dabbrev :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-dabbrev-code :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-etags :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-files :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-gtags :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-jedi :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-keywords :with company-yasnippet))
      ;;       (add-to-list 'company-backends '(company-tern :with company-yasnippet)))
      ;;   (error "Not adding company backends"))

      (if (boundp 'company-frontends)
          (progn
            (add-to-list 'company-frontends 'company-tng-frontend))
        (error "Not adding company front-ends")))))

(defun config/compilation-buffers ()
  "Configure compilation buffer settings."
  (defun compilation-mode-settings ()
    ;; wrap lines in compilation buffer
    (setq truncate-lines nil)
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'compilation-mode-hook #'compilation-mode-settings))

(defun config/copy-as-format ()
  "Configure copy-as-format."
  (setq-default
   copy-as-format-asciidoc-include-file-name t
   copy-as-format-default "org-mode"))

(defun config/deft ()
  "Configure deft notes browser."
  (setq-default
   deft-auto-save-interval 10
   deft-directory "~/Dropbox/deft"
   deft-extensions '("txt" "text" "tex" "md" "markdown" "org")
   deft-default-extension "org"
   deft-recursive t)

  (spacemacs/set-leader-keys
    "a n" nil
    "a n RET" #'spacemacs/deft
    "a n f" #'deft-find-file
    "a n n" #'deft-new-file-named
    "a n s" #'org-notes-open-sprint
    "a n b" #'org-notes-open-backlog)
  (spacemacs/declare-prefix "a n" "notes")

  (spacemacs/set-leader-keys-for-major-mode 'deft-mode
    "g" #'deft-refresh))

(defun config/diminish ()
  "Configure diminish glyphs for various minor modes."
  (with-eval-after-load 'diminish
    (diminish 'alchemist-mode "⊛")
    (diminish 'alchemist-phoenix-mode "⊙")
    (diminish 'elm-indent-mode "⨕")
    (diminish 'minitest-mode "⨷")
    (diminish 'rubocop-mode "℞")
    (diminish 'ruby-refactor-mode "RR")
    (diminish 'seeing-is-believing "S")
    (diminish 'tern-mode "₸")))

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

(defun config/elm ()
  "Configure Elm."
  (with-eval-after-load 'elm-mode
    (remove-hook 'elm-mode-hook 'elm-indent-mode)))

(defun config/email ()
  "Configure email packages."
  (mu4e-maildirs-extension)

  (setq-default
   mail-user-agent 'mu4e-user-agent
   mu4e-change-filenames-when-moving t ;; t required for mbsync
   mu4e-compose-signature-auto-include t
   mu4e-compose-complete-only-personal t
   mu4e-context-policy 'pick-first
   mu4e-get-mail-command "mbsync --all"
   mu4e-maildir (expand-file-name "~/Dropbox/mail")
   mu4e-maildir-shortcuts '(("/fastmail/INBOX" . ?i)
                            ("/fastmail/Archive" . ?a)
                            ("/fastmail/Trash" . ?t))
   mu4e-drafts-folder "/fastmail/Drafts"
   mu4e-refile-folder "/fastmail/Archive"
   mu4e-trash-folder "/fastmail/Trash"
   mu4e-sent-folder "/fastmail/Sent"
   mu4e-completing-read-function 'completing-read
   message-kill-buffer-on-exit t
   mu4e-confirm-quit nil
   mu4e-update-interval 600
   mu4e-view-show-addresses t
   mu4e-compose-format-flowed t
   mu4e-use-fancy-chars t
   mu4e-view-show-images t
   mu4e-view-prefer-html t
   mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"
   mu4e-html2text-command "w3m -dump -cols 80 -T text/html"
   mu4e-msg2pdf ""
   mu4e-headers-include-related t
   mu4e-attachment-dir (expand-file-name "~/Downloads")
   smtpmail-queue-dir (expand-file-name "~/Dropbox/mail/queue/cur")
   smtpmail-queue-mail nil
   smtpmail-stream-type 'ssl
   send-mail-function #'async-smtpmail-send-it
   message-send-mail-function #'async-smtpmail-send-it
   mu4e-bookmarks `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                    ("date:today..now" "Today's messages" ?t)
                    ("maildir:\"/Fastmail/Sent\" date:1d..now" "Sent recently" ?r)
                    ("flag:flagged" "Flagged messages" ?f)
                    ("date:7d..now" "Last 7 days" ?w)))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Open with external browser
  (setq-default
   ;; browse-url-browser-function #'w3m-browse-url
   browse-url-default-browser nil
   browse-url-generic-program "open")

  (spacemacs/set-leader-keys "f P" #'browse-url-generic)

  ;; w3m
  (setq-default
   w3m-home-page "https://www.google.com"
   w3m-default-display-inline-images t
   w3m-default-toggle-inline-images t
   w3m-command-arguments '("-cookie" "-F")
   w3m-use-cookies t
   w3m-view-this-url-new-session-in-background t)

  (with-eval-after-load 'mu4e-view
    (if (boundp 'mu4e-view-actions)
        (setq-default
         mu4e-view-actions
         '(("capture message" . mu4e-action-capture-message)
           ("show this thread" . mu4e-action-show-thread)
           ("view in browser" . mu4e-action-view-in-browser)))
      (error "Not adding to `mu4e-view-actions'")))

  (defun mu4e-compose-config ()
    "Config mu4e compose view."
    (set-fill-column 72)
    (auto-fill-mode 0)
    (setq-default mu4e-compose-format-flowed t
                  visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
    (visual-line-mode))
  (add-hook 'mu4e-compose-mode-hook #'mu4e-compose-config)

  (with-eval-after-load 'mu4e-alert
    (mu4e-alert-set-default-style 'notifier)))

(defun config/evil-collection ()
  "Enable evil keybindings everywhere."
  (setq-default evil-collection-company-use-tng t
                evil-collection-outline-bind-tab-p t
                evil-collection-term-sync-state-and-mode-p t
                evil-collection-setup-minibuffer t
                evil-collection-setup-debugger-keys t)
  ;; load init evil-collection after loading evil
  (when (require 'evil-collection nil t)
    (evil-collection-init)))

(defun config/evil-in-ex-buffer ()
  "Emacs bindings in Evil ex minibuffer."
  (if (boundp 'evil-ex-completion-map)
      (progn
        (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
        (define-key evil-ex-completion-map (kbd "C-k") 'kill-line)
        (define-key evil-ex-completion-map (kbd "C-a") 'beginning-of-line))
    (error "Failed setting up ex mode keybindings")))

(defun config/evil-goggles ()
  "Configure evil-goggles."
  (setq-default evil-goggles-pulse nil
                evil-goggles-duration 0.7)

  (evil-goggles-mode)
  (evil-goggles-use-diff-refine-faces))

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

(defun config/frames ()
  "Configure GUI Emacs frames."
  (when window-system
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

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

(defun config/global-modes ()
  "Enable globally set modes."
  (flx-ido-mode)
  (global-evil-quickscope-mode 1)
  (smartparens-global-strict-mode)
  (visual-line-mode))

(defun config/gtags ()
  "Configure GNU Global tag backend."
  ;; Enable direnv-mode, so bundler-gtags-produced .envrc is activated
  (direnv-mode)
  ;; Add GNU Global as an xref-backend
  (add-to-list 'xref-backend-functions 'gxref-xref-backend))

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

(defun config/ivy ()
  "Configure Ivy."
  (setq-default ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-fuzzy)
                                        (mx . ivy--regex-fuzzy)
                                        (swiper . ivy--regex-plus)
                                        (counsel-git-grep . ivy--regex-fuzzy)
                                        (t . ivy--regex-fuzzy))
                ivy-initial-inputs-alist nil))

(defun config/javascript-modes ()
  "Configure JavaScript modes: js, js2, react."
  (setq-default js-indent-level 2
                js2-basic-offset 2
                js2-strict-missing-semi-warning nil
                prettier-js-command "prettier-standard")

  (defun json-mode-hooks ()
    (progn
      (setq tab-width 2)
      (spacemacs/set-leader-keys-for-major-mode
        'json-mode "=" #'json-pretty-print-buffer)))
  (add-hook 'json-mode-hook #'json-mode-hooks)

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
  (add-hook 'rjsx-mode-hook #'rjsx-hybrid-keybindings))

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

(defun config/ligatures ()
  "Configure firacode font face with ligatures."
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
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))

  (defun toggle-ligatures-off ()
    "Disable ligatures."
    (interactive)
    (auto-composition-mode -1))

  (defun toggle-ligatures-on ()
    "Disable ligatures."
    (interactive)
    (auto-composition-mode))

  ;; Disable Fira Code ligatures in the following modes:
  (with-eval-after-load 'mu4e-headers
    (if (boundp 'mu4e-headers-mode-hook)
      (add-hook 'mu4e-headers-mode-hook #'toggle-ligatures-off)
     (error "Not disabling ligatures in mu4e headers"))

   (with-eval-after-load 'org
     (if (boundp 'org-mode-hook)
         (add-hook 'org-mode-hook #'toggle-ligatures-off)
       (error "Not disabling ligatures in org mode")))))

(defun config/lisps ()
  "Configure Lisp modes."
  ;; evil-cleverparens
  (require 'evil-cleverparens-text-objects)
  (smartparens-strict-mode)
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)

  (setq-default
   parinfer-extensions '(defaults pretty-parens evil lispy paredit smart-tab smart-yank))

  ;; Lisp modes
  (defun add-lisp-modes-hook (func)
    "Add FUNC as a hook to each of the major Lisp major modes."
    (progn
      (add-hook 'clojure-mode-hook func)
      (add-hook 'scheme-mode-hook func)
      (add-hook 'emacs-lisp-mode-hook func)))

  ;; Lisp minor modes
  (defun lisp-packages ()
    "Enable Lisp minor modes, in the correct sequence."
    (progn
      (lispy-mode +1)
      (parinfer-mode)
      (spacemacs/toggle-parinfer-indent-on)
      (evil-cleverparens-mode)))

  (remove-hook 'emacs-lisp-mode-hook #'parinfer-mode)
  (add-lisp-modes-hook #'lisp-packages))

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

(defun config/modeline ()
  "Configure the modeline."
  ;; evil mode indicators
  (setq-default
   evil-normal-state-tag   (propertize "[Normal]" 'face '((:background "green" :foreground "black")))
   evil-emacs-state-tag    (propertize "[Emacs]" 'face '((:background "orange" :foreground "black")))
   evil-insert-state-tag   (propertize "[Insert]" 'face '((:background "red") :foreground "white"))
   evil-hybrid-state-tag   (propertize "[Hybrid]" 'face '((:background "red") :foreground "white"))
   evil-motion-state-tag   (propertize "[Motion]" 'face '((:background "blue") :foreground "white"))
   evil-visual-state-tag   (propertize "[Visual]" 'face '((:background "grey80" :foreground "black")))
   evil-operator-state-tag (propertize "[Operator]" 'face '((:background "purple"))))

  ;; doom-modeline
  (setq-default doom-modeline-buffer-file-name-style 'truncate-with-project
                doom-modeline-github t
                doom-modeline-major-mode-color-icon t
                doom-modeline-major-mode-icon t
                doom-modeline-python-executable nil
                doom-modeline-version nil
                doom-modeline-env-command nil)

  (doom-modeline-def-modeline 'main
    '(workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches
                       " " buffer-info remote-host buffer-position
                       " " selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method buffer-encoding major-mode process vcs checker))

  (defun enable-doom-modeline-in-messages ()
    "Enable doom-modeline in messages buffer."
    (let ((msg-window (get-buffer-window "*Messages*")))
      (if msg-window
          (with-current-buffer (window-buffer msg-window)
            (doom-modeline-set-main-modeline)))))
  (add-hook 'post-command-hook #'enable-doom-modeline-in-messages)

  (doom-modeline-set-modeline 'main t)
  (doom-modeline-mode))

(defun config/org-mode ()
  "Configure and enable org mode."
  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (setq-default org-agenda-files '("~/Dropbox/org")
                  org-agenda-window-setup 'current-window)

    (if (boundp 'org-agenda-files)
        (mapc
         #'(lambda (file)
             (when (file-exists-p file)
               (push file org-agenda-files)))
         (org-projectile-todo-files))
      (error "Failed: 'org-agenda-files not bound")))

  (setq-default
   org-refile-use-outline-path 'file
   org-refile-allow-creating-parent-nodes 'confirm
   org-outline-path-complete-in-steps nil
   org-refile-targets '(("~/Dropbox/org/sprint-today.org" :maxlevel . 1)
                        ("~/Dropbox/org/sprint-backlog.org" :maxlevel . 1)
                        ("~/Dropbox/org/sprint-icebox.org" :maxlevel . 1)))

  (with-eval-after-load 'org
    (setq-default
     org-confirm-babel-evaluate nil
     org-startup-indented t
     org-edit-src-content-indentation 0
     org-md-headline-style 'setext
     spaceline-org-clock-p t
     org-src-tab-acts-natively t
     org-babel-python-command "python3"
     org-export-with-sub-superscripts '{}
     org-export-coding-system 'utf-8
     org-directory "~/Dropbox/org"
     org-default-notes-file "~/Dropbox/org/sprint-today.org"
     org-default-backlog-file "~/Dropbox/org/sprint-backlog.org"
     org-archive-location "~/Dropbox/org/archive.org::* %s")

    (setq-default
     org-todo-keywords
     '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)")))

    ;; Keybindings
    (evil-define-key 'hybrid evil-org-mode-map (kbd "C-H-<return>") 'org-insert-subheading)

    ;; Save clocks
    (setq-default org-clock-persist t)
    (org-clock-persistence-insinuate)

    ;; Journal
    (spacemacs/set-leader-keys-for-major-mode
      'org-journal-mode
      "s" 'org-journal-search
      "t" 'org-journal-today)

    (spacemacs/set-leader-keys
      "a j" nil
      "a j RET" #'org-journal-new-entry
      "a j t" #'org-journal-today
      "a j s" #'org-journal-search
      "a j f" #'org-journal-search-forever
      "a j d" #'org-journal-new-date-entry)
    (spacemacs/declare-prefix "a j" "org-journal")

    (add-hook 'org-journal-mode-hook #'org-mode)
    (add-hook 'org-capture-mode-hook #'org-align-all-tags)

    ;; Org capture templates
    (setq-default
     org-capture-templates
     '(
       ;;
       ;; Backlog items (to be prioritized, scoped, estimated)
       ;;
       ("b" "Backlog" entry
        (file "~/Dropbox/org/sprint-backlog.org")
        "* %?\n  %U")
       ("a" "Annotation" entry
        (file org-default-backlog-file)
        "* %?\n  %a\n  %U")
       ;;
       ("d" "Deadline" entry
        (file org-default-backlog-file)
        "* %?\n  DEADLINE: %^t\n  %U")
       ;;
       ;; Today's Sprint Tasks
       ;;
       ("t" "Task (Today's Sprint)" entry
        (file+headline org-default-notes-file "To Do Today")
        "* TODO %?\n  SCHEDULED: %t\n  %U"
        :empty-lines 1)
       ;;
       ("m" "Meeting" entry
        (file org-default-backlog-file)
        "* Meeting: %?\n  %^t\n  %U"
        :empty-lines 1)
       ;;
       ;; Notes / Journal / Blog Entry
       ;;
       ("n" "Note" plain
        (function org-capture-deft-new-file)
        "%(format \"#+TITLE: %s\n#+DATE: %s\n\" org-capture-deft--title %U)\n*  %?")
       ;;
       ("j" "Journal entry" entry
        (function org-journal-find-location)
        "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
       ;;
       ("p" "Blog post" entry
        (file+headline "blog-metier.org" "Post Ideas")
        (function org-hugo-new-post-capture-template)
        :empty-lines 1)
       ;;
       ;; Journal: Daily standup
       ;;
       ("s" "Standup" plain
        (file+olp+datetree "~/Dropbox/org/journal-standup.org")
        "     %?")
       ;;
       ;; Journal: Health
       ;;
       ("h" "Health / Diet journal" entry
        (file+olp+datetree "~/Dropbox/org/journal-health.org")
        "**** [%<%l:%M %p>] %^{Entry} %^g"
        :immediate-finish t)
       ;;
       ;; Reading / Code Journals
       ;;
       ("v" "Paste from clipboard" entry
        (file org-default-backlog-file)
        "* %^{Title} %^G\n   %?\n   %c")
       ;;
       ("c" "Commonplace" entry
        (file "~/Dropbox/org/journal-commonplaces.org")
        "* %^{Title} %^G\n   %?")))

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

    (setq-default org-structure-template-alist '(("a" . "export ascii")
                                                 ("c" . "center")
                                                 ("C" . "comment")
                                                 ("e" . "example")
                                                 ("E" . "export")
                                                 ("h" . "export html")
                                                 ("l" . "export latex")
                                                 ("n" . "export notes")
                                                 ("q" . "quote")
                                                 ("s" . "src")
                                                 ("sp" . "src python")
                                                 ("se" . "src elixir")
                                                 ("sr" . "src ruby")
                                                 ("sj" . "src js")
                                                 ("v" . "verse")))))

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
          (overlay-put (ov-at) 'before-string (make-string offset ?\s))))
       ;; Right justification
       ((and (eq 'right (plist-get org-format-latex-options :justify))
             (= beg (line-beginning-position)))
        (let* ((img (create-image image 'imagemagick t))
               (width (car (image-display-size (overlay-get (ov-at) 'display))))
               (offset (floor (- 40 width (- (line-end-position) end)))))
          (overlay-put (ov-at) 'before-string (make-string offset ?\s))))))
    (advice-add 'org--format-latex-make-overlay :after #'org-justify-fragment-overlay)))

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
  (defun prettify-symbols-python ()
    "Provide prettify-symbol mode mappings for python-mode."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("def" .    #x0192)
            ("in" .     #x2208)
            ("is" .     #x2261)
            ("is not" . #x2262)
            ("not in" . #x2209)
            ("all" .    #x2200)
            ("any" .    #x2203))))
  (add-hook 'python-mode-hook #'prettify-symbols-python)

  ;; prettify symbols: Emacs Lisp
  (defun prettify-symbols-emacs-lisp ()
    "Provide prettify-symbol mode mappings for emacs-lisp-mode."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("defun" .  #x0192))))
  (add-hook 'emacs-lisp-mode-hook #'prettify-symbols-emacs-lisp)

  ;; prettify symbols: Elixir
  (defun prettify-symbols-elixir()
    "Provide prettify-symbol mode mappings for elixir-mode."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("def" .  #x0192)
            ("defp" .  #x0070)
            ("defmodule" . #x006D)
            ("fn" . #x03BB))))
  (add-hook 'elixir-mode-hook #'prettify-symbols-elixir)

  ;; prettify symbols: JavaScript
  (defun prettify-symbols-javascript ()
    "Provide prettify-symbol mode mappings for javascript modes."
    (mapc (lambda (pair) (push pair prettify-symbols-alist))
          '(("function" .  #x0192))))
  (add-hook 'js2-mode-hook #'prettify-symbols-javascript))

(defun config/projectile ()
  "Configure Projectile."
  (setq-default projectile-completion-system 'ivy
                projectile-enable-caching t
                projectile-find-dir-includes-top-level t
                projectile-project-search-path '("~/Projects" "~/Work"))

  (if (bound-and-true-p projectile-globally-ignored-directories)
      (setq-default projectile-globally-ignored-directories
                    (append projectile-globally-ignored-directories
                            '("node_modules")))
    (error "Failed appending to projectile-globally-ignored-directories")))

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

  ;; conda-env
  (setq-default conda-anaconda-home (getenv "ANACONDA_HOME"))
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode)

  ;; traad
  ;; Expects a conda env of the same name be defined
  ;; Execute (traad-install-server) to set up
  (setq-default traad-environment-name "traad")

  ;; Register Pipenv project type with projectile
  (projectile-register-project-type 'python-pipenv '("Pipfile")
                                    :compile "pipenv run compile"
                                    :test "pipenv run test"
                                    :test-suffix "_test")

  (defun python-before-save-hooks ()
    (if (eq major-mode 'python-mode)
      (progn
        (pyimport-remove-unused)
        (importmagic-fix-imports)
        (py-isort-buffer)
        (yapfify-buffer))))

  (add-hook 'before-save-hook #'python-before-save-hooks))

(defun config/ruby ()
  "Configure packages for Ruby mode."
  ;; Define keybinding to manually trigger autoformat
  (setq-default rufo-enable-format-on-save t)
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode "=" #'rufo-format-buffer))

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

(defun config/semantic ()
  "Remove semantic mode hooks."
  (defun config/semantic-remove-hooks ()
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-notc-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-nolongprefix-completion-at-point-function))
  (add-hook 'semantic-mode-hook #'config/semantic-remove-hooks))

(defun config/set-terminal-emacs-theme ()
  "Set theme for terminal session."
  (if (not (display-graphic-p))
      (spacemacs/load-theme 'spacemacs-dark)))

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

(defun config/version-control ()
  "Configure version-control-related settings."
  (with-eval-after-load 'magit
    (magit-todos-mode)

    (if (and (boundp 'magit-completing-read-function)
             (boundp 'magit-mode-map))
        (progn
          (setq magit-completing-read-function 'ivy-completing-read)
          (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)

          (spacemacs/set-leader-keys-for-major-mode 'magit-status-mode "t" #'magit-todos-jump-to-todos)
          (magit-define-popup-switch 'magit-log-popup ?m "Omit merge commits" "--no-merges"))
      (error "Failed setting up magit")))

  (setq-default
   magit-repository-directories '(("~/Projects/" . 2)
                                  ("~/Documents/". 2)))


  ;; leader gb to display branching controls
  (spacemacs/set-leader-keys "gb" 'magit-branch-popup)

  ;; leader gB to display Git blame
  (spacemacs/set-leader-keys "gB" 'spacemacs/git-blame-micro-state)

  ;; Git Gutter: Display fringe on left
  (setq-default git-gutter-fr+-side 'left-fringe
                git-gutter-fr:side 'left-fringe))

(defun config/web-beautify ()
  "Configure web-beautify hooks."
  (eval-after-load 'sgml-mode
    '(add-hook 'html-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

  (eval-after-load 'css-mode
    '(add-hook 'css-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-css-buffer t t)))))

(defun config/web-mode ()
  "Configure web-mode (for CSS, HTML)."
  (setq-default css-indent-offset 2
                web-mode-markup-indent-offset 4
                web-mode-css-indent-offset 2
                web-mode-attr-indent-offset 2
                web-mode-code-indent-offset 2)

  (with-eval-after-load 'web-mode
    (if (boundp 'web-mode-indentation-params)
        (progn
          (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
          (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
          (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
      (error "Failed setting up web-mode indentation params"))))

(defun config/window-splitting ()
  "Make focus window commands primary."
  (spacemacs/set-leader-keys "ws" #'split-window-below-and-focus)
  (spacemacs/set-leader-keys "wS" #'split-window-below)
  (spacemacs/set-leader-keys "wv" #'split-window-right-and-focus)
  (spacemacs/set-leader-keys "wV" #'split-window-right)
  (spacemacs/set-leader-keys "wT" #'split-term-window-right-and-focus))

(defun config/yankee ()
  "Load and configure yankee.el.
Provides facilities for yanking formatted code snippets."
  (require 'yankee)
  (if (boundp 'evil-visual-state-map)
      (define-key evil-visual-state-map (kbd "g y") #'yankee/yank)
    (error "Failed setting up yankee.el keybinding")))

(defun config/yasnippet ()
  "Define yasnippet keybindings."
  (with-eval-after-load 'lispy
    (if (boundp 'lispy-mode-map)
        (define-key lispy-mode-map (kbd "C-j") nil)
      (error "Not overriding `lispy-mode-map' for yasnippet")))

  (define-key global-map (kbd "C-j") nil)
  (spacemacs/declare-prefix (kbd "C-j") "tools")
  (define-key global-map (kbd "C-j C-j") #'ivy-yasnippet)
  (define-key global-map (kbd "C-j C-;") #'yas-expand))

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
      (magithub forge doom-modeline magit yasnippet-snippets yapfify yankee yaml-mode xterm-color ws-butler writeroom-mode winum which-key wgrep web-mode web-beautify volatile-highlights vmd-mode vimrc-mode vimish-fold vi-tilde-fringe uuidgen use-package unfill treemacs-projectile treemacs-evil traad toc-org tagedit symon string-inflection stickyfunc-enhance srefactor sqlup-mode sql-indent spray spaceline-all-the-icons solarized-theme smex smeargle slim-mode shrink-path shell-pop seeing-is-believing scss-mode sass-mode rvm rufo ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe rjsx-mode reveal-in-osx-finder restart-emacs rbenv rainbow-delimiters pyvenv pytest pyimport pyenv-mode py-isort pug-mode projectile-rails pretty-mode prettier-js popwin pony-mode pippel pipenv pip-requirements persp-mode password-generator parinfer paradox ox-twbs ox-reveal ox-hugo ox-gfm overseer ov osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-mime org-journal org-download org-bullets org-brain open-junk-file ob-swift ob-restclient ob-ipython ob-http ob-elixir noflet nginx-mode nameless mwim mvn multi-term move-text mmm-mode minitest meghanada maven-test-mode markdown-toc magit-todos magit-svn magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode lispy link-hint launchctl json-navigator js-doc ivy-yasnippet ivy-xref ivy-rtags ivy-rich ivy-purpose ivy-hydra insert-shebang indium indent-guide importmagic impatient-mode ibuffer-projectile hungry-delete highlight-parentheses highlight-numbers highlight-indentation helm-make gxref groovy-mode groovy-imports graphviz-dot-mode graphql-mode gradle-mode google-translate google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates gitignore-mode github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist ghub+ gh-md ggtags fuzzy font-lock+ flyspell-correct-ivy flycheck-rtags flycheck-pos-tip flycheck-mix flycheck-gometalinter flycheck-elm flycheck-credo flycheck-bashate flx-ido fish-mode fill-column-indicator feature-mode eyebrowse expand-region exercism evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-text-object-python evil-surround evil-quickscope evil-org evil-numbers evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-commentary evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help ensime engine-mode emmet-mode elm-test-runner elm-mode elisp-slime-nav eldoc-eval ein editorconfig dumb-jump dotenv-mode dockerfile-mode docker disaster direnv diminish diff-hl deft dash-at-point dactyl-mode cython-mode csv-mode counsel-projectile counsel-gtags counsel-dash counsel-css conda company-web company-tern company-statistics company-shell company-rtags company-restclient company-quickhelp company-jedi company-go company-flx company-emacs-eclim company-c-headers company-auctex company-anaconda command-log-mode column-enforce-mode closql clean-aindent-mode clang-format chruby centered-cursor-mode bundler browse-at-remote bm bison-mode auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk amx alchemist aggressive-indent add-node-modules-path ace-link ac-ispell))))
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  (custom-set-faces))
