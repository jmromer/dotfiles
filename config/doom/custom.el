(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((apheleia-global-mode . -1)
     (apheleia-global-mode)
     (+format-with-lsp)
     (rspec-docker-command . "docker compose run --rm --use-aliases")
     (rspec-docker-command . "run --rm --use-aliases")
     (rspec-use-opts-file-when-available)
     (rspec-docker-cwd . "./")
     (rspec-docker-command . "docker compose run --rm --use-aliases tests")
     (rspec-use-docker-when-possible . t)
     (rspec-spec-command . "bin/rspec")
     (rspec-use-bundler-when-possible)
     (lsp-javascript-format-enable)
     (+format-on-save-enabled-modes quote
      (not rjsx-mode))
     (rubocop-check-command . "bundle exec rubocop --format emacs")
     (rubocop-format-command . "bundle exec rubocop --format emacs -x")
     (rubocop-autocorrect-command . "bundle exec rubocop --format emacs -a")
     (lsp-html-format-enable)))
 '(warning-suppress-log-types '((lsp-mode) (lsp-mode) (defvaralias)))
 '(warning-suppress-types '((lsp-mode) (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
