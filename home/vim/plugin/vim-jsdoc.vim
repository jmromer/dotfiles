" Allow prompt for interactive input.
let g:jsdoc_allow_input_prompt = 1

" Prompt for a function description
let g:jsdoc_input_description = 0

" Prompt for a value for @name, add it to the JSDoc
" block comment along with the @function tag.
let g:jsdoc_additional_descriptions = 0

" Add the @return tag.
let g:jsdoc_return = 1

" Prompt for and add a type for the aforementioned @return tag.
let g:jsdoc_return_type = 1

" Prompt for and add a description for the @return tag.
let g:jsdoc_return_description = 1

" Set value to 0 to turn off default mapping of <C-l> :JsDoc<cr>
let g:jsdoc_default_mapping = 0

" Set value to 1 to turn on access tags like @access <private|public>.
" Set value to 2 to turn on access tags like @<private|public>
let g:jsdoc_access_descriptions = 2

" Set value to 1 to turn on detecting underscore
" starting functions as private convention
let g:jsdoc_underscore_private = 1

" Set value to 1 to allow ECMAScript6 shorthand syntax.
let g:jsdoc_allow_shorthand = 1

" Characters used to separate @param name and description.
let g:jsdoc_param_description_separator = ' : '

" Override default type and description. See help more detail.
let g:jsdoc_custom_args_hook = {}
