" Test Runners
" ------------
" vim-rspec + tslime: open in tmux pane
let g:rspec_command = 'call Send_to_Tmux("rspec {spec}\n")'
"using spring: spring rspec {spec}

" Rspec
" nearest spec
nnoremap <Leader>s :call RunNearestSpec()<CR>
" the last spec run
nnoremap <Leader>sl :call RunLastSpec()<CR>
" current file
nnoremap <Leader>sf :call RunCurrentSpecFile()<CR>
" all specs
nnoremap <Leader>sa :call RunAllSpecs()<CR>

" Cucumber
" current scenario
nnoremap <Leader>c :call RunCucumberFeature()<CR>
" current file
nnoremap <Leader>cf :call RunCucumberFile()<CR>
" all feature files
nnoremap <Leader>ca :w<cr>:!cucumber<cr>


function! RunCucumberFile()
  let filename = expand('%:p')
  let command = "cucumber " . filename . "\n"
  call Send_to_Tmux(command)
endfunction

function! RunCucumberFeature()
  let filename = expand('%:p')
  let command = "cucumber " . filename . ":" . line(".") . "\n"
  call Send_to_Tmux(command)
endfunction

