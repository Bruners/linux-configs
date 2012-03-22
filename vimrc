" change the terminal's title
set title

" Disable vi support mode
set nocompatible

" Enable syntax highlighting
syntax enable

" Display what mode is active
set showmode

" Display partial commands in status line
set showcmd

" Swapfiles
set noswapfile
set nobackup

" Indenting
set autoindent
" Enable smart indenting
set smartindent

" Tabbing
set smarttab
" <tab> == 4 spaces
set tabstop=3
set softtabstop=4
" no spaces for tabbing
set expandtab

" Number of spaces to use for indenting
set shiftwidth=4

" UTF-8 encoding
set encoding=utf-8

set mouse=a
set mousehide

" Enable scrolling in screen (xterm2) or tmux (xterm) session
set ttymouse=xterm2

" Show the line and column number of the cursor position
set ruler

" Search
" Hilight searches
set hlsearch
" Jump to the first match when searching
set incsearch

" line numbers
set number

"support modelines
set modeline

" Don't wrap ling lines
set nowrap

" tab-completion menu
set wildmenu

" Keep 10 lines on the screen
set scrolloff=10

" Show invisible characters
set list
set list listchars=nbsp:¬,tab:·⁖,trail:¶,extends:»,precedes:«

" 256 colors
set t_Co=256

" Folding
set foldenable
" Size of the fold columen
set foldcolumn=4
" Fold based on syntax
set foldmethod=syntax
" Create folds, but don't fold any of them by default
set foldlevel=0

" Filetype
filetype on
filetype plugin indent on

" Unload buffers when they are abandoned
set nohidden

set ttyfast

" set theme
set background=dark
colorscheme solarized

set showbreak=>\ \ \
" left: fileformat, type, encoding, RO/HELP/PREVIEW, modified flag, filepath
set statusline=%([%{&ff}]%)%(:[%{&fenc}]%)%(:%y%)\ \ %r%h%w\ %#Error#%m%#Statusline#\ %F\
" right: buffer num, lines/total, cols/virtual, display percentage
set statusline+=%=buff[%1.3n]\ \ %1.7c%V,%1.7l/%L\ \ [%P]
" set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ %{fugitive#statusline()}

" F5 toggles paste mode
set pastetoggle=<F5>

" Highligt whitespaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()
" ensure every file does syntax highlighting (full)
autocmd BufEnter * :syntax sync fromstart

" I want to save or quit too fast..
map :Q :q
map :W :w

" Pathogen Runetime Path Manipulation https://github.com/tpope/vim-pathogen
call pathogen#infect()
