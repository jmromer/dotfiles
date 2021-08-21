;;; init.el -*- lexical-binding: t; -*-

(setq user-emacs-directory
      (format "%s/%s"
              (or (getenv "XDG_DATA_HOME")
                  "~/.dotfiles/share")
              "emacs"))

(load (format "%s/%s" user-emacs-directory "init"))
