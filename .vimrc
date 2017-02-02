set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-fugitive'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'sheerun/vim-polyglot'
Plugin 'kovetskiy/sxhkd-vim'
Plugin 'nvie/vim-togglemouse'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" Put your non-Plugin stuff after this line

let mapleader=","		" change map leader from \ to ,

set hidden

set nowrap        " don't wrap lines
set tabstop=4     " a tab is four spaces
set backspace=indent,eol,start
                    " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set number        " always show line numbers
set shiftwidth=4  " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch     " set show matching parenthesis
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                    "    case-sensitive otherwise
set smarttab      " insert tabs on the start of a line according to
                	"    shiftwidth, not tabstop
set hlsearch      " highlight search terms
set incsearch     " show search matches as you type

set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class,*.o
set title                " change the terminal's title
set visualbell           " don't beep
set noerrorbells         " don't beep

set nobackup
set noswapfile

set pastetoggle=<F2>

syntax on

"
" autocommands
"

augroup filetype_rust
	autocmd!
	autocmd FileType rust setlocal makeprg=cargo\ build
	autocmd FileType rust let b:RunCmd="!cargo\ run"
augroup end

"
" functions
"
function Date()
	put =strftime('%c')
endfunction

"
" keybindings
"

nnoremap ; :

" Toggle NERDTree
map <C-n> :NERDTreeToggle<CR>

" Quickly edit/reload the vimrc
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" Compilation
nmap <C-m> :make<CR>
nmap <C-x> :execute b:RunCmd<CR>

" Use Q for formatting the current paragraph or selection
vmap Q gq
nmap Q gqap

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Clear highlighted searches
nmap <silent> ,/ :nohlsearch<CR>

" With "w!!" use sudo to save file if you forgot to open as root
cmap w!! w !sudo tee % >/dev/null

" Serch for visually selected text
vnoremap // y/<C-R>"<CR>
