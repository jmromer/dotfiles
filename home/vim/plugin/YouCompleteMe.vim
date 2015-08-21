" Use system Python
" (YCM and Vim must be built against the same version of python)
let g:ycm_path_to_python_interpreter = '/usr/bin/python'

" Fall back path to global ycm conf
let g:ycm_global_ycm_extra_conf = '~'

" Don't ask for confirmation to load global ycm conf
let g:ycm_confirm_extra_conf = 0

" Move through completion list with C-n, C-p
let g:ycm_key_list_select_completion = ['<C-n>']
let g:ycm_key_list_previous_completion = ['<C-p>']

" Trigger completion with C-Space
let g:ycm_key_invoke_completion = '<C-Space>'

" Fixes clang 'pattern not found' messages
let g:clang_user_options='|| exit 0'
