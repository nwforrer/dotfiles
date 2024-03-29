"
" general config
"

set number
set relativenumber
set cursorline
set mouse=a
set autoindent
set copyindent
set showmatch
set ignorecase
set smartcase
set visualbell
set noerrorbells
let mapleader = ' '

set nobackup
set noswapfile

syntax on

"
" vim-plug section
"

" install vim-plug if not found
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
	silent !sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/plugged')

" helpers
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'

" color schemes
Plug 'sainnhe/sonokai'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'morhetz/gruvbox'

" search
Plug 'ctrlpvim/ctrlp.vim'

" git stuff
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" tests
Plug 'vim-test/vim-test'

" intellisense
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" syntax highlighting
Plug 'styled-components/vim-styled-components', {'branch': 'main' }
Plug 'sheerun/vim-polyglot'

" debugging
Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh' }

" building
Plug 'cdelledonne/vim-cmake'

" java
Plug 'artur-shaik/vim-javacomplete2'

call plug#end()

let g:ctrlp_working_path_mode = 0
let g:ctrlp_custom_ignore = {
			\ 'dir': '\v[\/](target|\.git|zig-cache)$',
			\ }


"
" color scheme
"
if (has('termguicolors'))
	set termguicolors
endif

"let g:sonokai_style = 'shusia'
"let g:airline_theme = 'sonokai'
"colorscheme sonokai
let g:gruvbox_italic=1
autocmd vimenter * ++nested colorscheme gruvbox

"
" autocommands
"

augroup filetype_rust
	autocmd!
	autocmd FileType rust setlocal makeprg=cargo\ build
	autocmd FileType rust let b:RunCmd="!cargo\ run"
augroup end

autocmd! BufNewFile,BufRead *.vert,*.frag set ft=glsl

"
" java configuration
"
autocmd FileType java setlocal omnifunc=javacomplete#Complete
nmap <F4> <Plug>(JavaComplete-Imports-AddSmart)
imap <F4> <Plug>(JavaComplete-Imports-AddSmart)

nmap <F5> <Plug>(JavaComplete-Imports-Add)
imap <F5> <Plug>(JavaComplete-Imports-Add)

nmap <F6> <Plug>(JavaComplete-Imports-AddMissing)
imap <F6> <Plug>(JavaComplete-Imports-AddMissing)

nmap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
imap <F7> <Plug>(JavaComplete-Imports-RemoveUnused)

"
" keybindings
"

" Compilation
nmap <C-m> :make<CR>
nmap <C-x> :execute b:RunCmd<CR>
nmap <leader>g :!make && make run<CR>

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" coc
let g:coc_disable_startup_warning = 1
nmap <silent> <leader>gd <Plug>(coc-definition)
nmap <silent> <leader>gr <Plug>(coc-references)
nmap <silent> <leader>gi <Plug>(coc-implementation)
nmap <silent> <leader>gy <Plug>(coc-type-definition)
nmap <silent> <leader>rn <Plug>(coc-rename)
nnoremap <silent><nowait> <leader>o :<C-u>CocList outline<cr>
