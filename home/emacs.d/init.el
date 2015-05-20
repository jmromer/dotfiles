(require 'cask "/usr/local/Cellar/cask/0.7.2/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

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

(add-to-list 'load-path "~/.emacs.d/custom")
