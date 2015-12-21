execute pathogen#infect()
filetype plugin indent on

syntax on

set background=dark

if has('gui_running')
	let base16colorspace=256
	set t_Co=256
	colorscheme base16-oceanicnext
endif

if has('vim_starting')
	if &compatible
		set nocompatible
	endif
endif

set cursorline

set exrc
set secure

set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab

set number

set ignorecase
set smartcase
set incsearch

augroup vimrc_autocmds
	autocmd BufEnter * highlight OverLength ctermbg=darkgrey guibg=#592929
	autocmd BufEnter * match OverLength /\%80v.*/
augroup END

" Use HTML syntax for EJS files
autocmd BufNewFile,BufRead *.ejs set syntax=html

set wrap
set textwidth=80
set colorcolumn=80
highlight ColorColumn ctermbg=lightgray

set pastetoggle=<F2>

" allow changing buffers without saving
set hidden

" Ctrl+kjhl navigation between windows
map <C-J> <C-W>j<C-W>_
map <C-K> <C-W>k<C-W>_
map <C-H> <C-W>h<C-W>_
map <C-L> <C-W>l<C-W>_

" Set F4 to compile program (can set makeprg in individual project .vimrc's)
nnoremap <F4> :make!<cr>
