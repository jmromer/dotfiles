" TODO
" - Consolidate variables
" - Consolidate git invocations (if git [gh/glab/bb], elsif hg [bb], else)
" - language-specific commenting
" - VCS-agnostic
" - Optionally omit links / configure format
" - Yank folded:
"    <details>
"    <summary><a href="https://github.com/jkrmr/dot_spacemacs/blob/498e45c8/init.el#L16-L23">init.el#L16-L23 (498e45c8)</a></summary>
"
"    ```el
"    ;; init.el L16-L23 (498e45c8)
"
"         (auto-completion :variables
"                          auto-completion-return-key-behavior nil
"                          auto-completion-tab-key-behavior 'complete
"                          auto-completion-complete-with-key-sequence nil
"                          auto-completion-complete-with-key-sequence-delay 0.1
"                          auto-completion-private-snippets-directory nil
"                          auto-completion-enable-snippets-in-popup t
"                          auto-completion-enable-help-tooltip t)
"    ```
"    </details>
"
function! s:CommentedLine(line)
  echo &filetype
  if &filetype =~# 'html\|eruby'
    return '<!-- ' . a:line . ' -->'
  elseif &filetype =~# 'lisp\|clojure'
    return ';; ' . a:line
  elseif &filetype =~# 'tex'
    return '% ' . a:line
  elseif &filetype =~# 'python\|ruby\|sh\|desktop\|fstab\|conf\|profile\|bash\|yaml'
    return '# ' . a:line
  elseif &filetype =~# 'lua'
    return '-- ' . a:line
  elseif &filetype =~# 'vim'
    return '" ' . a:line
  else
    return '// ' . a:line
  endif
endfunction

function! s:CurrentVCSCommit(target_dir, sha)
  if a:sha =~# 'fatal'
    return ''
  else
    return ' (' . a:sha[0:8] . ')'
  endif
endfunction

function! s:PathFromProjectRoot(full_path, project_root, target_dir, home_dir)
  if a:project_root =~# 'fatal'
    return substitute(a:full_path, a:home_dir, '~', '')
  else
    return substitute(a:full_path, a:project_root . '/', '', '')
  endif
endfunction

function! s:CurrentVCSRemote(target_dir, commit, filepath, filename, startline, endline)
  let l:git_result = system('cd ' . a:target_dir . " && git remote -v | awk '/fetch/{print $2}' | sed -Ee 's#(git@|git://)#http://#' -e 's@com:@com/@' -e 's/\.git$//'")
  let l:remote = substitute(l:git_result, '\n\+$', '', '')

  if l:remote =~# 'http'
    if l:remote =~# 'github|gitlab'
      return l:remote . '/blob/' . a:commit . '/' . a:filepath . '#L' . a:startline . '-L' . a:endline
    elseif l:remote =~# 'bitbucket'
      return l:remote . '/src/' . a:commit . '/' . a:filepath . '#' . a:filename . '-' . a:startline . '-' . a:endline
    else
      return l:remote . '/blob/' . a:commit . '/' . a:filepath . '#L' . a:startline . '-L' . a:endline
    endif
  end
endfunction

function! s:FormattedLineNummbers(line1, line2)
  if a:line1 ==# a:line2
    return 'L' . a:line1
  else
    return 'L' . a:line1 . '-L' . a:line2
  endif
endfunction

function! s:RemoteMarkdownLink(remote_url)
  if a:remote_url ==# '0'
    return ''
  else
    return '<sup>[[source](' . a:remote_url . ')]</sup>'
  endif
endfunction

function! s:YankAsMarkdown(line1, line2)
  let l:save_cursor = getpos('.')

  let l:home_dir = substitute(system('echo $HOME'), '\n\+$', '', '')
  let l:full_path = expand('%:p')
  let l:filename = expand('%:t')

  let l:target_directory = substitute(l:full_path, l:filename, '', '')
  let l:project_root = substitute(system('cd ' . l:target_directory . ' && git rev-parse --show-toplevel'), '\n\+$', '', '')
  let l:sha = substitute(system('cd ' . l:target_directory . ' && git rev-parse --short HEAD'), '\n\+$', '', '')

  let l:commit = s:CurrentVCSCommit(l:target_directory, l:sha)
  let l:short_path = s:PathFromProjectRoot(l:full_path, l:project_root, l:target_directory, l:home_dir)
  let l:line_numbers = s:FormattedLineNummbers(a:line1, a:line2)
  let l:remote = s:CurrentVCSRemote(l:target_directory, l:sha, l:short_path, l:filename, a:line1, a:line2)
  let l:remote_link = s:RemoteMarkdownLink(l:remote)

  let l:source_comment = l:short_path . ' ' . l:line_numbers . l:commit
  let l:comment_leader = s:CommentedLine(l:source_comment)

  let l:lines = getline(a:line1, a:line2)
  let l:markdown = [ '```' . &filetype, l:comment_leader . "\n" ]
        \ + l:lines
        \ + ['```', l:remote_link]

  let l:snippet = join(l:markdown, "\n")
  let @* = l:snippet
  call setpos('.', l:save_cursor)
endfunction

command! -range=% YankAsMarkdown call <SID>YankAsMarkdown(<line1>, <line2>)
" command! -range=% YankAsFoldedMarkdown call <SID>YankAsFoldedMarkdown(<line1>, <line2>)

vnoremap <silent> gym :YankAsMarkdown<CR>
" vnoremap <silent> gyf :YankAsFoldedMarkdown<CR>

