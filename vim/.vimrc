set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-fugitive' 	" Git
Plugin 'ctrlpvim/ctrlp.vim'		" Fuzzy search
Plugin 'scrooloose/nerdtree'	" File tree
Plugin 'sheerun/vim-polyglot'	" Syntax
Plugin 'morhetz/gruvbox'		" Colorscheme
Plugin 'craigemery/vim-autotag'	" Auto-update ctags
Plugin 'chrisbra/csv.vim'		" CSV file handling
Plugin 'benmills/vimux'			" Run commands in tmux

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" Put your non-Plugin stuff after this line

let mapleader=","		" change map leader from \ to ,

let g:ctrlp_working_path_mode = 0
let g:ctrlp_custom_ignore = {
			\ 'dir': '\v[\/](target|\.git)$',
			\ }

"let g:VimuxOrientation = "h"

set hidden

set background=dark

set nowrap        " don't wrap lines
set tabstop=4     " a tab is four spaces
set backspace=indent,eol,start
                    " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
"set number        " always show line numbers
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

" Don't save backups of *.gpg files
set backupskip+=*.gpg
" To avoid that parts of the file is saved to .viminfo when yanking or
" deleting, empty the 'viminfo' option.
set viminfo=

augroup encrypted
	au!
	" Disable swap files, and set binary file format before reading the file
	autocmd BufReadPre,FileReadPre *.gpg
				\ setlocal noswapfile bin
	" Decrypt the contents after reading the file, reset binary file format
	" and run any BufReadPost autocmds matching the file name without the .gpg
	" extension
	autocmd BufReadPost,FileReadPost *.gpg
				\ execute "'[,']!gpg --decrypt --default-recipient-self" |
				\ setlocal nobin |
				\ execute "doautocmd BufReadPost " . expand("%:r")
	" Set binary file format and encrypt the contents before writing the file
	autocmd BufWritePre,FileWritePre *.gpg
				\ setlocal bin |
				\ '[,']!gpg --encrypt --default-recipient-self
	" After writing the file, do an :undo to revert the encryption in the
	" buffer, and reset binary file format
	autocmd BufWritePost,FileWritePost *.gpg
				\ silent u |
				\ setlocal nobin
augroup END

"
" functions
"
function! Date()
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

" Prompt for a command to run in tmux
map <leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <leader>vl :VimuxRunLastCommand<CR>
" Inspect runner pane
map <leader>vi :VimuxInspectRunner<CR>
" Zoom the tmux runner pane
map <leader>vz :VimuxZoomRunner<CR>

" Compilation
nmap <C-m> :make<CR>
nmap <C-x> :execute b:RunCmd<CR>
nmap <leader>g :!make && make run<CR>

" Use Q for formatting the current paragraph or selection
vmap Q gq
nmap Q gqap

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" vv to create a new vertical split
nnoremap <silent> vv <C-w>v

" Clear highlighted searches
nmap <silent> ,/ :nohlsearch<CR>

" Toggle line numbers
nmap <silent> ,n :set number!<CR>

map ,e :e <C-R>=expand("%:p:h") . "/" <CR>
map ,t :tabe <C-R>=expand("%:p:h") . "/" <CR>
map ,s :split <C-R>=expand("%:p:h") . "/" <CR>

" With "w!!" use sudo to save file if you forgot to open as root
cmap w!! w !sudo tee % >/dev/null

" Serch for visually selected text
vnoremap // y/<C-R>"<CR>

" Autocomplete using ctags
inoremap <C-]> <C-x><C-]>

