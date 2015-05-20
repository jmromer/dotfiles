(require 'cask "/usr/local/Cellar/cask/0.7.2/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

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

;(require 'package)
;
; (setq package-archives
;       '(("gnu"    . "http://elpa.gnu.org/packages/")
;         ("melpa"  . "http://melpa.milkbox.net/packages/")))
;
;(package-initialize)
;
; (when (not (package-installed-p 'evil))
;   (package-refresh-contents)
;   (package-install 'evil))
