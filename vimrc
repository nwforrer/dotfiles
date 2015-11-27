execute pathogen#infect()
filetype plugin indent on

set background=dark

if has('gui_running')
	let base16colorspace=256
	syntax on
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

set wrap
set textwidth=80
set colorcolumn=80
highlight ColorColumn ctermbg=lightgray

set pastetoggle=<F2>

" allow changing buffers without saving
set hidden

