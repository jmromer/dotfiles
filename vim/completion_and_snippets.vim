" YouCompleteMe: use system python
let g:ycm_path_to_python_interpreter = '/usr/bin/python'

" YouCompleteMe: fallback path to global ycm conf
let g:ycm_global_ycm_extra_conf = '~'

" YouCompleteMe: Don't ask for confirmation to load global ycm conf
let g:ycm_confirm_extra_conf = 0

" YouCompleteMe: move through completion list
let g:ycm_key_list_select_completion = ['<C-n>']
let g:ycm_key_list_previous_completion = ['<C-p>']

" YouCompleteMe: semantic completion trigger
let g:ycm_key_invoke_completion = '<C-Space>'

" UltiSnips: Trigger configuration.
let g:UltiSnipsExpandTrigger       = '<Tab>'
let g:UltiSnipsListSnippets        = '<C-Tab>'
let g:UltiSnipsJumpForwardTrigger  = '<C-j>'
let g:UltiSnipsJumpBackwardTrigger = '<C-k>'
let g:UltiSnipsEditSplit           = 'context'
