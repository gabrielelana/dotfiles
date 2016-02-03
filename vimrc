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
" complete options (disable preview scratch window)
set completeopt=menu,menuone,longest
" faster Esc in visual/insert mode
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
" don't redraw the screen while executing a macro
set lazyredraw

" taken from https://github.com/dduponchel/dotfiles/blob/master/vim/vimrc
" Indicates a fast terminal connection. More characters will be sent to the
" screen for redrawing, instead of using insert/delete line commands. Improves
" smoothness of redrawing when there are multiple windows and the terminal
" does not support a scrolling region. Also enables the extra writing of
" characters at the end of each screen line for lines that wrap. This helps
" when using copy/paste with the mouse in an xterm and other terminals.
set ttyfast

" keep more context when scrolling
set scrolloff=5
set sidescrolloff=15
set sidescroll=1

" when break a line then continue visually indentent as the beginning of that
" line, do not break with less than 20 characters to the end of the line, show
" ↳ at the beginning of the line to indicate that is not a new line but the
" continuation of the previous (visual) line, show that synbol in the column
" used for the line numbers (cpoptions+=n)
if has('linebreak')
  set showbreak=↳
  if exists('&breakindent')
    set breakindent
    set breakindentopt=min:20,shift:0,sbr
  endif
  set cpoptions+=n
endif

" make Y behave consistently with D and C
nnoremap Y y$

" map Leader and LocalLeader to 0x00A0 (NO_BREAK_SPACE)
let mapleader=' '
let maplocalleader=' '
inoremap <Leader> <Nop>
inoremap <LocalLeader> <Nop>

" disable backup and swap files
set nobackup noswapfile nowritebackup

" persistent undo
if exists('+undofile')
  " undofile - This allows you to use undos after exiting and restarting
  " This, like swap and backups, uses .vim-undo first, then ~/.vim/.undo
  " :help undo-persistence
  if isdirectory($HOME . '/.vim/.undo') == 0
    :silent !mkdir -p ~/.vim/.undo > /dev/null 2>&1
  endif
  set undodir=./.vim-undo//,~/.vim/.undo//
  set undofile
endif

" folding
set foldopen=all
set foldclose=all
set nofoldenable

" autocommands
if has('autocmd')
  autocmd Filetype html setlocal ts=4 sts=4 sw=4
  autocmd Filetype php setlocal ts=4 sts=4 sw=4
  autocmd Filetype javascript setlocal ts=2 sts=2 sw=2
  autocmd Filetype json setlocal ts=2 sts=2 sw=2
  autocmd Filetype scheme setlocal ts=2 sts=2 sw=2
  autocmd Filetype sh setlocal ts=2 sts=2 sw=2
  autocmd Filetype gitcommit setlocal spell complete+=kspell
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

" for some reason it doesn't work with engelbart keyboard
nnoremap g] <C-]>
nnoremap g[ <C-T>
nnoremap <C-]> :echo("It doesn't work sorry, use g] instead")
nnoremap <C-T> :echo("It doesn't work sorry, use g[ instead")

" easily move and resize windows
nnoremap <C-H> <C-W>h
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l

" yank the current file path in default registry
nnoremap <silent><Leader>cp :let @" = expand('%:p')<CR>
" yank the current file name without extension in default registry
" (useful when the file name without extension is name of the class/module)
nnoremap <silent><Leader>cn :let @" = expand('%:t:r')<CR>
" paste the current file name without extension
" (useful when the file name without extension is name of the class/module)
inoremap <silent><Leader>cn <C-R>=expand('%:t:r')<CR>

" make Shift-Tab works
map <Esc>[Z <S-Tab>
ounmap <Esc>[Z

" keeps cursor in the middle of the window after jump
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <C-O> <C-O>zz
nnoremap <C-I> <C-I>zz

" visual shifting does not exit visual mode
vnoremap < <gv
vnoremap > >gv

" zt is okay for putting something at the top of the screen, but when I'm
" writing prose I often want to put something at not-quite-the-top of the
" screen.  zh is 'zoom to head level'
nnoremap zh mzzt10<C-U>`z

" redraw all the screen
nnoremap U :syntax sync fromstart<CR>:redraw!<CR>:nohl<CR>

" replace EX mode with repeat the last recorded macro
map Q @@



" fix terminal problems
if !has('gui_running')
  set t_RV=
  " explicitly tell vim that the terminal supports 256 colors
  set t_Co=256
  " prevent vim from clobbering the scrollback buffer
  " see http://www.shallowsky.com/linux/noaltscreen.html
  set t_ti= t_te=

  set t_ZH=[3m
  set t_ZR=[23m
endif


filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" {{{ ack
Plugin 'mileszs/ack.vim'
let g:ackprg = 'ack -H --nocolor --nogroup --column'
" }}}

" {{{ ctrlp
Plugin 'kien/ctrlp.vim'
let g:ctrlp_custom_ignore = '\.git$\|\.tmp$\|\.hg$\|\.svn$\|.work$\|.keep$'
let g:ctrlp_root_markers = ['.root', '.ctrlp']
let g:ctrlp_open_new_file = 'r' " <c-y> open new file in the current window
" }}}

" {{{ syntastic
Plugin 'scrooloose/syntastic'
let g:syntastic_mode_map = { 'mode': 'passive' }
" }}}

" {{{ airline
Plugin 'bling/vim-airline'
let g:airline_powerline_fonts = 1
let g:airline_theme = 'powerlineish'
" }}}

" {{{ local
Plugin 'MarcWeber/vim-addon-local-vimrc'
" }}}

" {{{ pope barrage
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-repeat'
" }}}

" {{{ colors
Plugin 'godlygeek/csapprox'
Plugin 'tomasr/molokai'
" }}}

" {{{ tabular
Plugin 'Tabular'
" }}}

" {{{ regions and text objects
Plugin 'kana/vim-textobj-user'
Plugin 'julian/vim-textobj-variable-segment'

" ]v jump to the next variable segment
" [v jump to the previous variable segment
function! s:GoToVariableSegment(backward, visual)
  if a:visual
    normal! gv
  endif
  let direction = a:backward ? '?' : '/'
  let pattern = join(['[-_.]\+\i', '\<\i', '\<\k', '\l\u', '\u\u\ze\l', '\a\d', '\d\a'], '\|')
  execute 'silent normal! ' . l:direction . l:pattern . l:direction . "e\<CR>"
endfunction
noremap <silent> ]v :call <SID>GoToVariableSegment(0, 0)<CR>
noremap <silent> [v :call <SID>GoToVariableSegment(1, 0)<CR>
vnoremap <silent> ]v :<C-U>call <SID>GoToVariableSegment(0, 1)<CR>
vnoremap <silent> [v :<C-U>call <SID>GoToVariableSegment(1, 1)<CR>

Plugin 'terryma/vim-expand-region'
" }}}

" {{{ go
Plugin 'fatih/vim-go'
" }}}

" {{{ elixir
Plugin 'elixir-lang/vim-elixir'
" }}}

" {{{ rust
Plugin 'wting/rust.vim'
" }}}

" {{{ json
Plugin 'tpope/vim-jdaddy'
Plugin 'leshill/vim-json'
" }}}

" {{{ javascript
Plugin 'pangloss/vim-javascript'
" }}}

" {{{ markdown
let g:markdown_enable_mappings = 1
let g:markdown_enable_insert_mode_mappings = 1
let g:markdown_include_jekyll_support = 1
Plugin 'gabrielelana/vim-markdown'
" }}}

" {{{ cucumber
Plugin 'tpope/vim-cucumber'
hi def link cucumberGiven Conditional
hi def link cucumberWhen  Conditional
hi def link cucumberThen  Conditional
" }}}

" {{{ ruby
Plugin 'vim-ruby/vim-ruby'
Plugin 'thoughtbot/vim-rspec'

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

  au FileType ruby :nnoremap <Leader>t :call RunCurrentSpecFile()<CR>
  au FileType ruby :nnoremap <Leader>s :call RunNearestSpec()<CR>
  au FileType ruby :nnoremap <Leader>l :call RunLastSpec()<CR>
  au FileType ruby :nnoremap <Leader>a :call RunAllSpecs()<CR>
augroup END
" }}}

" {{{ haml
Plugin 'tpope/vim-haml'
" }}}

" {{{ php
Plugin 'php.vim'
Plugin 'phpcomplete.vim'
" }}}

call vundle#end()

" enable syntax
syntax on

" This is the place to fix syntax issues from plugins
" {{{ vim-javascript
highlight   def   link      jsGlobalObjects          Text
highlight   def   link      jsFutureKeys             Text
" }}}

" enable indentation, this is important to do *after* `vundle#end()`
filetype plugin indent on

" color schemes
colorscheme mustang
" colorscheme digerati
" colorscheme github
" colorscheme railscasts

" enable matchit
runtime macros/matchit.vim

" spelling
set spelllang=en
set spellfile=~/.vim/spell/en.utf-8.add
" fix the previous mispelled world and jump back
inoremap <Leader>z <Esc>[s1z=A

" functions
function! DisableCursorLineWhenInQuickfix()
  if &buftype=='quickfix'
    setlocal nocursorline
  endif
endfunction

function! RemoveTrailingWhitespaces()
    let l:save_cursor = getpos('.')
    silent! execute ':%s/\s\+$//'
    call setpos('.', l:save_cursor)
endfunction
