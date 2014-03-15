augroup MdAdvisedMappings
  autocmd!
  autocmd FileType markdown nnoremap <buffer> <Leader>e :MdEditCodeBlock<CR>
  autocmd FileType markdown vnoremap <buffer> <Leader>e :'<,'>MdEditCodeBlock<CR>
augroup END

let g:markdown_include_jekyll_support = 1

Bundle 'gabrielelana/vim-markdown'
