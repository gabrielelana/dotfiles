augroup MdAdvisedMappings
  autocmd!
  autocmd FileType markdown nnoremap <buffer> <Leader>e :MdEditCodeBlock<CR>
  autocmd FileType markdown vnoremap <buffer> <Leader>e :'<,'>MdEditCodeBlock<CR>
augroup END

Bundle 'gabrielelana/vim-markdown'
