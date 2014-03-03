Bundle 'vim-ruby/vim-ruby'
Bundle 'thoughtbot/vim-rspec'

augroup RubyLanguage
  au!
  au Filetype haml setlocal ts=2 sts=2 sw=2
  au FileType ruby setlocal ts=2 sts=2 sw=2

  " For some unknown reason my <C-]> doesn't work...
  " Now I will do the same mapping as vim-ruby plugin
  " but in the g namespace
  au FileType ruby :nnoremap <silent> <buffer> g]
    \ :<C-U>execute v:count1."tag <C-R>=RubyCursorIdentifier()<CR>"<CR>
  au FileType ruby :nnoremap <silent> <buffer> <C-W>g]
    \ :<C-U>execute v:count1."stag <C-R>=RubyCursorIdentifier()<CR>"<CR>
  au FileType ruby :nnoremap <silent> <buffer> ga]
    \ :<C-U>execute "tjump <C-R>=RubyCursorIdentifier()<CR>"<CR>
  au FileType ruby :nnoremap <silent> <buffer> <C-W>ga]
    \ :<C-U>execute "stjump <C-R>=RubyCursorIdentifier()<CR>"<CR>
  au FileType ruby :nnoremap <silent> <buffer> gp]
    \ :<C-U>execute "ptag <C-R>=RubyCursorIdentifier()<CR>"<CR>
  au FileType ruby :nnoremap <silent> <buffer> <C-W>gp]
    \ :<C-U>execute "ptjump <C-R>=RubyCursorIdentifier()<CR>"<CR>

  au FileType ruby :nnoremap <silent> <buffer> g[ <C-T>
augroup END
