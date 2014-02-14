set nocompatible
" allow backspacing over everything in insert mode
set backspace=indent,eol,start
" keep 500 lines of command line history
set history=10000
" show the cursor position all the time
set ruler
" display incomplete commands
set showcmd
" do incremental searching
set incsearch
" no sound bell, flash screen
set visualbell
" indent automatically
set autoindent
" highlight search
set hlsearch
" show line numbers
set number
" word wrap
set wrap
" wildmode
set wildmode=list:longest,full
" faster esc in visual/insert mode
set ttimeout
set ttimeoutlen=10
" text settings
set expandtab
set tabstop=2
set shiftwidth=2
set textwidth=0
set smarttab
" always show the statusline
set laststatus=2
" default encoding
set encoding=utf-8
" insert only one space when joining lines that contain sentence-terminating
set nojoinspaces

" map leader
let mapleader="\\"

" disable backup and swap files
set nobackup noswapfile nowritebackup

" persistent undo
if exists("+undofile")
  " undofile - This allows you to use undos after exiting and restarting
  " This, like swap and backups, uses .vim-undo first, then ~/.vim/.undo
  " :help undo-persistence
  if isdirectory($HOME . '/.vim/.undo') == 0
    :silent !mkdir -p ~/.vim/.undo > /dev/null 2>&1
  endif
  set undodir=./.vim-undo//,~/.vim/.undo//
  set undofile
endif

" autocommands
if has("autocmd")
  autocmd Filetype html setlocal ts=4 sts=4 sw=4
  autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
  autocmd Filetype haml setlocal ts=2 sts=2 sw=2
  autocmd Filetype php setlocal ts=4 sts=4 sw=4
  autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
  autocmd Filetype json setlocal ts=2 sts=2 sw=2
  autocmd Filetype scheme setlocal ts=2 sts=2 sw=2
  autocmd Filetype sh setlocal ts=2 sts=2 sw=2
  autocmd BufEnter,BufWinEnter * call DisableCursorLineWhenInQuickfix()
endif

augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END

augroup FixBeforeWrite
  au!
  au BufWritePre * call RemoveTrailingWhitespaces()
augroup END

" abbreviations
"
iab shrug ¯\_(ツ)_/¯
iab dunno ¯\_(ツ)_/¯

" mappings
"
" esc removes highlight
nnoremap <silent> <esc> :nohlsearch<cr>

" easely move and resize windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" make Shift-Tab works
map <esc>[Z <s-tab>
ounmap <esc>[Z


" fix terminal problems
if !has("gui_running")
  set t_RV=
  " explicitly tell vim that the terminal supports 256 colors
  set t_Co=256
  " prevent vim from clobbering the scrollback buffer
  " see http://www.shallowsky.com/linux/noaltscreen.html
  set t_ti= t_te=

  set t_ZH=[3m
  set t_ZR=[23m
endif


source ~/.vim/vundle.vim

source ~/.vim/ack.vim
source ~/.vim/ctrlp.vim
source ~/.vim/syntastic.vim
source ~/.vim/airline.vim
source ~/.vim/cucumber.vim
source ~/.vim/local.vim
source ~/.vim/surround.vim
source ~/.vim/unimpaired.vim
source ~/.vim/repeat.vim
source ~/.vim/endwise.vim
source ~/.vim/commentary.vim
source ~/.vim/fugitive.vim
source ~/.vim/colors.vim
source ~/.vim/tabular.vim
source ~/.vim/region.vim

source ~/.vim/markdown.vim
source ~/.vim/ruby.vim
source ~/.vim/javascript.vim
source ~/.vim/haml.vim
source ~/.vim/php.vim


" color schemes
colorscheme mustang
" colorscheme digerati
" colorscheme github
" colorscheme railscasts
" silent! colorscheme molokai

" enable syntax
syntax on
" enable indentation
filetype plugin indent on

" functions
function! DisableCursorLineWhenInQuickfix()
  if &buftype=="quickfix"
    setlocal nocursorline
  endif
endfunction

function! RemoveTrailingWhitespaces()
    let l:save_cursor = getpos(".")
    silent! execute ':%s/\s\+$//'
    call setpos('.', l:save_cursor)
endfunction
