augroup vimrcEx
  autocmd!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  " Cucumber navigation commands
  autocmd User Rails Rnavcommand step features/step_definitions -glob=**/* -suffix=_steps.rb
  autocmd User Rails Rnavcommand config config -glob=**/* -suffix=.rb -default=routes

  " Enable spellchecking for Markdown
  autocmd FileType markdown setlocal spell

  " Automatically wrap at 80 characters for Markdown
  autocmd BufRead,BufNewFile *.md setlocal textwidth=80

  " Automatically wrap at 72 characters and spell check git commit messages
  autocmd FileType gitcommit setlocal textwidth=72
  autocmd FileType gitcommit setlocal spell

  " Allow stylesheets to autocomplete hyphenated words
  autocmd FileType css,scss,sass setlocal iskeyword+=-

  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile Appraisals set filetype=ruby
  autocmd BufRead,BufNewFile *.md set filetype=markdown
  autocmd BufRead,BufNewFile *.jison set filetype=javascript
  autocmd BufRead,BufNewFile *gitconfig set syntax=dosini

  " Set folding for semantic-whitespace and tagged languages
  autocmd BufNewFile,BufReadPost *.coffee setl foldmethod=indent
  autocmd BufNewFile,BufReadPost *.html setl foldmethod=indent
  autocmd BufNewFile,BufReadPost *.html.erb setl foldmethod=indent

  " for TeX
  autocmd BufRead,BufNewFile *.xtx set syntax=tex
  autocmd BufRead,BufNewFile *.cls set syntax=tex
  autocmd BufWritePost *.xtx !xelatex %
augroup END

