" -------------------- Syntax Highlighting ------------------------
syntax on    " turn syntax highlighting on
filetype on

" ------------- Syntastic (style checking) ------------------------
" configure syntastic syntax checking to check on open as well as save
let g:syntastic_check_on_open = 1
let g:syntastic_javascript_checkers = ['jslint']
let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_aggregate_errors = 1
let g:syntastic_enable_signs=1
let g:syntastic_auto_jump=1
let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'
let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute \"ng-"]

" ----------------- File type associations ------------------------
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
  autocmd BufRead,BufNewFile *.jison setfiletype=javascript
  autocmd BufRead,BufNewFile *gitconfig setfiletype=dosini
augroup END

" -------------------- Tab completion ------------------------
" Insert tab if at beginning of line, else use completion if
" not at beginning

set wildmode=list:longest,list:full

" function! InsertTabWrapper()
"   let col = col('.') - 1
"   if !col || getline('.')[col - 1] !~ '\k'
"     return "\<tab>"
"   else
"     return "\<c-p>"
"   endif
" endfunction
"
" inoremap <Tab> <c-r>=InsertTabWrapper()<cr>
" inoremap <S-Tab> <c-n>
"
