set mouse=a                   " allow mouse input in all modes
syntax enable                 " enable syntax highlighting
set background=dark           " enable dark background in themes
colorscheme solarized         " set theme
set t_Co=256                  " force 256-color mode
set nobackup                  " disable backup files (filename~)
set encoding=utf-8            " UTF-8 encoding for all new files
set ttymouse=xterm            " enable scrolling in screen (xterm2) or tmux (xterm) sessions
set ruler                     " show cursor position in status line
set showmode                  " show mode in status line
set showcmd                   " show partial commands in status line


" left: fileformat, type, encoding, RO/HELP/PREVIEW, modified flag, filepath
set statusline=%([%{&ff}]%)%(:[%{&fenc}]%)%(:%y%)\ \ %r%h%w\ %#Error#%m%#Statusline#\ %F\ 
" right: buffer num, lines/total, cols/virtual, display percentage
set statusline+=%=buff[%1.3n]\ \ %1.7c%V,%1.7l/%L\ \ [%P]
" set statusline=[%l,%v\ %P%M]\ %f\ %r%h%w\ (%{&ff})\ %{fugitive#statusline()}

" F5 toggles paste mode
set pastetoggle=<F5>

