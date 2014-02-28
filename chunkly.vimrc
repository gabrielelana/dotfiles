" regenerate mongo object id under the cursor
nnoremap <Leader>r ciw<C-R>=
  \substitute(
    \system('mongo --quiet --eval "print(new ObjectId().valueOf())"'),
    \'^\s*\(.\{-}\)[\s\n]*$', '\1', ''
  \)<CR><ESC>

augroup Chunkly
  au!
  au BufRead,BufWrite *.log :call clearmatches()
  au BufRead,BufWrite *.log
    \ execute 'normal mP' |
    \ %g/\v\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\+\d{2}:\d{2}/call CheckForConflicts(line('.')) |
    \ execute 'normal `P'
augroup END

function! CheckForConflicts(line_number)
  let l:start_time_pattern = '\v.*(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\+\d{2}:\d{2}).*'
  let l:previous_line_number = a:line_number - 1
  if l:previous_line_number > 0
    let l:pomodoro_id = substitute(getline(a:line_number), '^\(.\{24}\)\s.*', '\1', '')
    let l:previous_start_time = substitute(getline(l:previous_line_number), l:start_time_pattern, '\1', '')
    let l:start_time = substitute(getline(a:line_number), l:start_time_pattern, '\1', '')
    let l:previous_start_timestamp = system('date --date="' . l:previous_start_time . '" +"%s"')
    let l:start_timestamp = system('date --date="' . l:start_time . '" +"%s"')

    if l:start_timestamp - l:previous_start_timestamp < 1500
      call matchadd('Error', '\M\%' . a:line_number . 'l' . l:start_time . '')
    endif
    if search(l:pomodoro_id, 'bn', l:previous_line_number)
      call matchadd('Error', '\M\%' . a:line_number . 'l' . l:pomodoro_id . '')
    endif
  endif
endfunction
