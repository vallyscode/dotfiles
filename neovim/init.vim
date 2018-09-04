" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.local/share/nvim/plugged')

Plug 'iCyMind/NeoSolarized'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Initialize plugin system
call plug#end()

syntax enable
colorscheme NeoSolarized
set background=light

let g:airline#extensions#tabline#enabled = 1
