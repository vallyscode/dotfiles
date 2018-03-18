language en_US

" Start of dein scripts---------------------"
if &compatible
  set nocompatible
endif

set runtimepath+=/Users/valeriy/.cache/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('/Users/valeriy/.cache/dein')
  call dein#begin('/Users/valeriy/.cache/dein')
  call dein#add('/Users/valeriy/.cache/dein/repos/github.com/Shougo/dein.vim')

  " syntax
  call dein#add('scrooloose/syntastic')

  " javascript
  call dein#add('pangloss/vim-javascript')

  " typescript
  call dein#add('quramy/tsuquyomi')
  call dein#add('shougo/vimproc.vim', {'build': 'make'})
  call dein#add('leafgarland/typescript-vim')

  " rust language
  call dein#add('rust-lang/rust.vim')
  call dein#add('racer-rust/vim-racer')

  " haskell setup
  call dein#add('eagletmt/ghcmod-vim')
  call dein#add('eagletmt/neco-ghc')
  call dein#add('tomtom/tlib_vim')
  call dein#add('marcweber/vim-addon-mw-utils')
  call dein#add('garbas/vim-snipmate')
  call dein#add('scrooloose/nerdcommenter')
  call dein#add('godlygeek/tabular')

  " json
  call dein#add('elzr/vim-json')

  " less
  call dein#add('groenewege/vim-less')

  " auto-complete
  call dein#add('shougo/deoplete.nvim')
  call dein#add('ervandew/supertab')

  " fs
  call dein#add('shougo/unite.vim')     " dependency for vimfiler
  call dein#add('shougo/vimfiler.vim')
  call dein#add('mhinz/vim-grepper')
  call dein#add('ctrlpvim/ctrlp.vim')

  " git
  call dein#add('tpope/vim-fugitive')
  call dein#add('airblade/vim-gitgutter')

  " indent line
  call dein#add('yggdroot/indentline')

  " status line
  call dein#add('vim-airline/vim-airline')
  call dein#add('vim-airline/vim-airline-themes')

  " devicons
  call dein#add('ryanoasis/vim-devicons')

  " color theme
  call dein#add('iCyMind/NeoSolarized')

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable

" Install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

" End of dein scripts-----------------------"

set t_Co=256
set encoding=utf8
set number

set mouse=a             " Enable mouse.
set showmatch

set autoindent          " Indent according to previous line.
set softtabstop=4       " Tab key indents by 4 spaces.
set tabstop=4           " Render tabs using this many spaces.
set shiftwidth=4        " >> indents by 4 spaces.
set expandtab           " Insert spaces when tab is pressed.

set backspace=indent,eol,start " Make backspace work as you would expect.
set display=lastline           " Show as much as possible of the last line.

set showmode                    " Show current mode in command-line.
set showcmd                     " Show already typed keys when more are expected.

set incsearch                   " Hilight while searching with / or ?
set hlsearch                    " Keep matches hilighted.

" set ttyfast                     " Faster redrawing.
" set lazyredraw                  " Only redraw when necessary.

set splitbelow                  " Open new windows below the current window.
set splitright                  " Open new windows right of the current window.

set cursorline                  " Find the cursor line quickly.
set wrapscan                    " Search wrap around end-of-line.
set report=0                    " Always report changed lines.
set synmaxcol=200               " Only hilight 200 columns.

set list                        " Show non-printable characters
if has('multi_byte') && &encoding ==# 'utf-8'
  let &listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±'
else
  let &listchars = 'tab:> ,extends:>,precedes:<,nbsp:.'
endif

" neovim python-----------------------------"
let g:python3_host_prog='/usr/local/bin/python3'
let g:python2_host_prog='/usr/local/bin/python2'

" theme-------------------------------------"
set termguicolors
set background=dark
colorscheme NeoSolarized

" airline-----------------------------------"
set laststatus=2
let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#left_sep=' '
let g:airline#extensions#tabline#left_alt_sep='|'
let g:airline_powerline_fonts=1
" set guifont=DroidSansMonoForPowerline\ Nerd\ Font:h11

" toggle relative numbers-------------------"
function! NumberToggle()
  if(&relativenumber == 1)
     set noru
     set number
  else
     set rnu
  endif
endfunc

nnoremap <leader>r :call NumberToggle()<cr>

" ctrlp ------------------------------------"
let g:ctrlp_by_filename=1
let g:ctrlp_max_files=0

" deoplete----------------------------------"
let g:deoplete#enable_at_startup=1

" indent line-------------------------------"
let g:indentLine_char='|'
let g:indentLine_enabled=1

" syntastic---------------------------------"
map <leader>s :SyntasticToggleMode<cr>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_loc_list=0
let g:syntastic_check_on_open=0
let g:syntastic_check_on_wq=0

" ghc-mod-----------------------------------"
map <silent> tw :GhcModTypeInsert<cr>
map <silent> ts :GhcModSplitFunCase<cr>
map <silent> tq :GhcModType<cr>
map <silent> te :GhcModTypeClear<cr>

" supertab----------------------------------"
let g:haskellmode_completion_ghc=1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" tabularize--------------------------------"
let g:haskell_tabular=1
vmap a= :Tabularize /=<cr>
vmap a; :Tabularize /::<cr>
vmap a- :Tabularize /-><cr>

" rust--------------------------------------"
set hidden
let g:racer_cmd="/Users/valeriy/.cargo/bin/racer"
let g:rustc_path="/Users/valeriy/.cargo/bin/rustc"

au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)

