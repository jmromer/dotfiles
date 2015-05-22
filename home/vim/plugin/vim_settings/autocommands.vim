augroup vimrcEx
  autocmd!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  " automatically re-balance the visible splits as tmux panes are created,
  " destroyed, or resized or window resized
  autocmd VimResized * :wincmd =

  " Automatically wrap at 80 characters for Markdown
  autocmd BufRead,BufNewFile *.md setlocal textwidth=80

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
  autocmd BufRead,BufNewFile *.xtx,*.cls set syntax=tex filetype=tex
  autocmd BufWritePost *.xtx !xelatex %

  " Allow stylesheets to autocomplete hyphenated words
  autocmd FileType css,scss,sass setlocal iskeyword+=-
augroup END
