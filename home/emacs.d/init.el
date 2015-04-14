(require 'package)

(setq package-archives
      '(("gnu"    . "http://elpa.gnu.org/packages/")
        ("melpa"  . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(when (not (package-installed-p 'evil))
  (package-refresh-contents)
  (package-install 'evil))

(evil-mode)
