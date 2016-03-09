execute pathogen#infect()
filetype plugin indent on

" Syntax Color/Theming ------- {{{

syntax on

set background=dark

if has('gui_running')
	let base16colorspace=256
	set t_Co=256
	colorscheme base16-oceanicnext

	set guifont=Hack\ 11

	" remove the menu and toolbar
	set guioptions-=T
	set guioptions-=m
endif

" }}}

" Basic Settings -------------- {{{

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
	autocmd!
	autocmd BufEnter * highlight OverLength ctermbg=darkgrey guibg=#592929
	autocmd BufEnter * match OverLength /\%80v.*/
augroup END

" Use HTML syntax for EJS files
augroup syntax_filetypes
	autocmd!
	autocmd BufNewFile,BufRead *.ejs set syntax=html
augroup END

set wrap

" Automatically break long lines
"set textwidth=80

set colorcolumn=80
"highlight ColorColumn ctermbg=lightgray
highlight ColorColumn ctermbg=darkgray

set pastetoggle=<F2>

" allow changing buffers without saving
set hidden

" automatically place ending bracket on next line
let delimitMate_expand_cr = 1

let mapleader = ","

" }}}

" Mappings ---------------------------------- {{{

" Ctrl+kjhl navigation between windows
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-H> <C-W>h
nnoremap <C-L> <C-W>l

" Ctrl+Alt+kjhl navigation and expansion between windows
nnoremap <C-A-J> <C-W>j<C-W>_
nnoremap <C-A-K> <C-W>k<C-W>_
nnoremap <C-A-H> <C-W>h<C-W>_
nnoremap <C-A-L> <C-W>l<C-W>_

" Set F4 to compile program (can set makeprg in individual project .vimrc's)
nnoremap <F4> :make!<cr>

" Uppercase the current word while in insert mode
inoremap <leader><c-u> <esc>viwUea
" Uppercase the current word while in normal mode
nnoremap <leader><c-u> viwUe

" Open the vimrc file in a split window to edit
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
" Source the vimrc file
nnoremap <leader>sv :source $MYVIMRC<cr>

" Surround the current word with quotes
nnoremap <leader>" viw<esc>a"<esc>hbi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>hbi'<esc>lel

" Surround the visually selected text with quotes
vnoremap <leader>" <esc>`<i"<esc>`>la"<esc>
vnoremap <leader>' <esc>`<i'<esc>`>la'<esc>

" Move to the beginning of the line
nnoremap H ^
" Move to the end of the line
nnoremap L $

" Use jk to escape to normal mode
inoremap jk <esc>
inoremap <esc> <nop>

nnoremap <leader><c-n> :NERDTreeToggle<cr>

" Open Terminator in the current buffers directory
nnoremap <leader>t :silent !terminator --working-directory=%:h &<cr>

"nnoremap <leader>g :silent execute "grep! -R " . shellescape(expand("<cWORD>")) . " ."<cr>:copen<cr>


" }}}

" Abbreviations -------------- {{{

" Abbreviations to fix common typos
iabbrev adn and
iabbrev tehn then
iabbrev waht what
iabbrev teh the

" Abbreviations for email and signature
iabbrev @@ nwforrer@gmail.com
iabbrev ssig -- <cr>Nick Forrer<cr>nwforrer@gmail.com

" Insert .h include guard with ifndefh
iabbrev ifndefh #ifndef <c-r>=expand("%:t:r")<cr>_h<leader><c-u><cr>#define <c-r>=expand("%:t:r")<cr>_h<leader><c-u><cr><cr><cr><cr>#endif /* <c-r>=expand("%:t:r")<cr>_h<leader><c-u> */jkkki

" Insert GPL copyright notice
iabbrev gplcc /* Foobar.<cr>Copyright (C) 2016<cr>Nick Forrer <nwforrer@gmail.com><cr>This file is part of the Foobar program.<cr><cr>Foobar is free software: you can redistribute it and/or modify<cr>it under the terms of the GNU General Public License as published by<cr>the Free Software Foundation, either version 3 of the License, or<cr>(at your option) any later version.<cr>Foobar is distributed in the hope that it will be useful,<cr>but WITHOUT ANY WARRANTY; without even the implied warranty of<cr>MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the<cr>GNU General Public License for more details.<cr><cr>You should have received a copy of the GNU General Public License<cr>along with Foobar.  If not, see <http://www.gnu.org/licenses/>.<cr>*/<cr>jk

" }}}

" FileType-specific Settings ----------------- {{{

" Comment out lines based on filetype
augroup filetype_comments
	autocmd!
	autocmd FileType c nnoremap <buffer> <localleader>c I//<esc>
	autocmd FileType python nnoremap <buffer> <localleader>c I#<esc>
augroup END

augroup filetype_snippets
	autocmd!
	autocmd FileType python :iabbrev <buffer> iff if:<left>
	autocmd FileType javascript :iabbrev <buffer> iff if ()<left>
augroup END

" Replace text within previous parentheses
onoremap in( :<c-u>normal! f(vi(<cr>
" Replace text within previous parenthases
onoremap il( :<c-u>normal! F)vi(<cr>

augroup filetype_markdown
	autocmd!
	" Operate on the paragraphs header (denoted by being underscored with ==)
	autocmd FileType markdown :onoremap <buffer> ih :<c-u>execute "normal! ?^[==\|--]\\+$\r:nohlsearch\rkvg_"<cr>
	" Operate on the paragraphs header, including the underscore (denoted by being underscored with ==)
	autocmd FileType markdown :onoremap <buffer> ah :<c-u>execute "normal! ?^[==\|--]\\+$\r:nohlsearch\rg_vk0"<cr>
augroup END

augroup json_autocmd
	autocmd!
	autocmd FileType json set autoindent
	autocmd FileTYpe json set formatoptions=tcq2l
	autocmd FileTYpe json set textwidth=78 shiftwidth=2
	autocmd FileTYpe json set softtabstop=2 tabstop=8
	autocmd FileTYpe json set expandtab
	autocmd FileTYpe json set foldmethod=syntax
augroup END


" }}}

" Vimscript file settings --------------------- {{{

augroup filetype_vim
	autocmd!
	autocmd FileType vim setlocal foldmethod=marker
	autocmd FileType vim setlocal foldlevel=0
augroup END

" }}}

" Status Line -------------------- {{{

set statusline=%.30f	" Path to the file
set statusline+=%m	" Modified?
set statusline+=%y	" Filetype
set statusline+=%=	" Switch to the right side
set statusline+=%l	" Current line
set statusline+=/	" Separator
set statusline+=%L	" Total lines
set statusline+=,	" Separator
set statusline+=%c	" Column number

" }}}

" Functions ------------------------------------ {{{

nnoremap <leader>q :call <SID>QuickFixToggle()<cr>

let g:quickfix_is_open = 0

function! s:QuickFixToggle()
	if g:quickfix_is_open
		cclose
		let g:quickfix_is_open = 0
		execute g:quickfix_return_to_window . "wincmd w"
	else
		let g:quickfix_return_to_window = winnr()
		copen
		let g:quickfix_is_open = 1
	endif
endfunction

" }}}
