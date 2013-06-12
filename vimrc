" ~/.vimrc
scriptencoding utf-8

if (&termencoding == "")
    set termencoding=utf8
endif

if (&encoding ==# "latin1") && has("gui_running")
    set encoding=utf-8
endif

if (&term =~ "xterm")
    if has('title')
        set title
    endif

    " change cursor colour depending upon mode
    if exists('&t_SI')
        let &t_SI = "\<Esc>]12;lightgoldenrod\x7"
        let &t_EI = "\<Esc>]12;grey80\x7"
    endif
endif

" Indicates a fast terminal connection
set ttyfast

" Don't be compatible with vi
set nocompatible
" Call pathogen and bundle
filetype off
call pathogen#infect()
call pathogen#helptags()

" Enable a nice big viminfo file
set viminfo='1000,f1,:1000,/1000
set viminfo^=!
set history=500
" Make backspace delete lots of things
set backspace=indent,eol,start
" Create backups
set backup
set backupdir=~/.local/vim-backupdir
" No swapfiles
set noswapfile
" Show us the command we're typing
set showcmd
" Highlight matching parens
set showmatch
" Show full tags when doing search completion
set showfulltag
" Highlight search while typing
set hlsearch
set incsearch
" Speed up macros
set lazyredraw
" No annoying error noises
set noerrorbells
set visualbell t_vb=

if has("autocmd")
    " Enable auto indent plugin
    filetype on
    filetype plugin on
    filetype indent on

    autocmd FileType * setlocal colorcolumn=0
    autocmd GUIEnter * set visualbell t_vb=

    autocmd BufEnter *
                \ if &filetype == "cpp" || &filetype == "c" || &filetype == "haskell" || &filetype == "java" |
                \    set noignorecase noinfercase |
                \ else |
                \    set ignorecase infercase |
                \ endif
else
    set autoindent
    set ignorecase
    set infercase
endif

" Go for an ident of 4 by default
set shiftwidth=4
set tabstop=4
"set softtabstop=4
" screw you tabs
set expandtab

if has("folding")
    set foldenable
    set foldmethod=manual
    set foldlevelstart=99
endif

" syntax when printing
set popt+=syntax:y

if has('syntax') && !exists('g:syntax_on')
    " Syntax highlighting
    syntax enable
endif

" dusplay as much as possible of the last line
set display+=lastline

" Try to show at least n lines and columns when scrolling
set scrolloff=3
set sidescrolloff=5
" Wrap
set whichwrap+=<,>,[,]
" Completion menu
set wildmenu
set wildmode=longest:list:full
set wildignore+=*.o,*~,.lo,*.hi
set suffixes+=.in,.a,.1
" allow hidden buffers
set hidden
" tell buffers to use tabs
try
    "set switchbuf=useropen,usetab,newtab
    set switchbuf=usetab,newtab
catch
endtry
set winminheight=1

" enable virtual edit in vblock mode, and one past the end
set virtualedit=block,onemore

" Try to load a nice colourscheme
if has("eval")
    fun! LoadColourScheme(schemes)
        let l:schemes = a:schemes . ":"
        while l:schemes != ""
            let l:scheme = strpart(l:schemes, 0, stridx(l:schemes, ":"))
            let l:schemes = strpart(l:schemes, stridx(l:schemes, ":") + 1)
            try
                exec "colorscheme" l:scheme
                break
            catch
            endtry
        endwhile
    endfun

    let s:prefer_scheme = 'molokai'
    if -1 != match(getcwd(), 'inkpot')
        let s:prefer_scheme = 'inkpot$'
    endif

    if has('gui')
        call LoadColourScheme(s:prefer_scheme . ":elflord")
    else
        if has("autocmd")
            autocmd VimEnter *
                        \ if &t_Co == 88 || &t_Co == 256 |
                        \     call LoadColourScheme(s:prefer_scheme . ":darkblue:elflord") |
                        \ else |
                        \     call LoadColourScheme("darkblue:elflord") |
                        \ endif
        else
            if &t_Co == 88 || &t_Co == 256
                call LoadColourScheme(s:prefer_scheme . ":darkblue:elflord")
            else
                call LoadColourScheme("darkblue:elflord")
            endif
        endif
    endif
endif

" Disable modelines, use securemodelines.vim instead
set nomodeline
let g:secure_modelines_modelines=5

au VimEnter * call filter(exists("g:secure_modelines_allowed_items") ? g:secure_modelines_allowed_items : [],
            \ 'v:val != "fdm" && v:val != "foldmethod"')

set nrformats-=octal
" wrap, continue and remove comments in insertmode
set formatoptions=croqj
" ignore textwidth for long lines in insertmode
set formatoptions+=l

set shiftround
set complete-=i
set mouse=a
" autoread when file is changed after loading
set autoread

" timeoutlen is used for mapping and macro delays
set timeoutlen=1200
" ttimeoutlen is used for key code delays.
set ttimeoutlen=50

if has('gui')

    " GUI Tab settings
    fun! GuiTabLabel()
        let label = ''
        let buflist = tabpagebuflist(v:lnum)
        if exists('t:title')
            let label .= t:title . ' '
        endif
        let label .= '[ ' . bufname(buflist[tabpagewinnr(v:lnum) - 1]) . ' ]'
        for bufnr in buflist
            if getbufvar(bufnr, '&modified')
                let label .= '+'
                break
            endif
        endfor
        return label
    endfun

    " menus and toolbar off by default
    set guioptions-=m " menubar
    set guioptions-=T " toolbar
    set guioptions-=l " no left scroll
    set guioptions-=L " really! no left scroll
    set guioptions+=r " always show right scroll
    set guioptions+=a "
    set guioptions+=F " footer motif?
    set guioptions+=c " console messages
    " Guitab label (tab#. title)
    set guitablabel=%{GuiTabLabel()}

    " Gui mouse
    set mousehide
    behave mswin

    set noguipty

    set cmdheight=2
    set icon

    " Enable shift-insert paste in gui
    map <S-Insert> <MiddleMouse>
    map! <S-Insert> <MiddleMouse>
    "set guifont=ProggyCleanTT\ CE\ 12
    set cursorline
endif

if has('eval')

    command! KeepWindowBufferWipe enew|bw #

    " Delete a buffer but keep layout
    nmap <C-w>! KeepWindowBufferWipe<CR>

endif

" Kill line
nnoremap <C-k> "_dd
inoremap <C-k> <ESC>dd

" attempt to paste stuff
imap <C-v> <ESC>p_
noremap <C-c> y<CR>
noremap <C-x> x<CR>

" paste and indent lines
nnoremap p p`[v`]=
" New tab
map <C-t> :tabnew<CR>
" Close tab
map <C-q> :quit<CR>

if has('gui')
    " New window
    map <C-N> :!gvim &<CR><CR>
    "Open new file dialog (ctrl-o)
    map <C-O> :browse confirm e<cr>
    "Open save-as dialog (ctrl-shift-s)
    map <C-S-s> :browse confirm saveas<cr>
endif

" F1 is even more annoying
inoremap <F1> <ESC>

" F2 Toggle toolbar & menu
if has('gui')
    map <silent> <F2> :if &guioptions =~# 'T' <Bar>
    \   set guioptions-=T <Bar>
    \   set guioptions-=m <Bar>
    \ else <Bar>
    \   set guioptions+=T <Bar>
    \   set guioptions+=m <Bar>
    \ endif<CR>
endif

" F3 toggle highlight and clear with <C-l>
nnoremap <silent> <F3> :silent nohlsearch<CR>
inoremap <silent> <F3> <C-o>:silent nohlsearch<CR>
if maparg('<C-L>', 'n') ==# ''
    nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

" F5 toggles paste mode
set pastetoggle=<F5>

" F6 inserts generic copyright header
nmap <silent> <F6> :call <SID>MakeGenericCopyrightHeader()<CR>

" Annoying default mappings
noremap K k
noremap J j

nnoremap n nzz
nnoremap N Nzz

" * is silly
noremap * :let @/='\<'.expand('<cword>').'\>'<bar>:set hls<CR>
noremap g* :let @/=expand('<cword>')<bar>:set hls<CR>

map <Leader>n <plug>NERDTreeMirrorToggle<CR>

" tabs like a pro
nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

" tab completion
if has("eval")
    function! CleverTab()
        if strpart(getline('.'), 0, col('.') - 1) =~ '^\s*$'
            return "\<Tab>"
        else

            return "\<C-N>"
        endif
    endfun
    inoremap <Tab> <C-R>=CleverTab()<CR>
    inoremap <S-Tab> <C-P>
endif

" Map Alt-# to switch tabs
if has('gui_running')
    map <A-0> <ESC>0gt
    map <A-1> <ESC>1gt
    map <A-2> <ESC>2gt
    map <A-3> <ESC>3gt
    map <A-4> <ESC>4gt
    map <A-5> <ESC>5gt
    map <A-6> <ESC>6gt
endif

" Show tabs and trailing whitespace visually
if (&termencoding == "utf-8") || has("gui_running")
    if has("gui_running")
        set list listchars=tab:·⁖,trail:¶,extends:…,precedes:«,nbsp:‗
    else
        set list listchars=tab:·⁖,trail:¶,extends:»,precedes:«,nbsp:_
    endif
else
    set list listchars=tab:>-,trail:.,extends:>,nbsp:_
endif

set fillchars=fold:-

" Nice statusbar
set laststatus=2
set statusline=
" buffer name
set statusline+=%1*%-3.3n%0*\
" file name
set statusline+=%f\
" flags
set statusline+=%(%H%2*%M%R%W%*%\)
" fugitive
set statusline+=%1*%{fugitive#statusline()}\
set statusline+=%=
" filetype
set statusline+=\%4*[%1*%{strlen(&ft)?&ft:'none'}%4*/%1*
" encoding
set statusline+=%{&encoding}%4*/%1*
" file format
set statusline+=%{&fileformat}%4*]%1*
" line scroll
set statusline+=%4*[%1*%-4.(%l,%c%V%)%4*]%1*\%4*[%1*%P%4*]%1*

"filename
"hi default link User1 SignColumn
" flags Statement
"hi default link User2 GitGutterAdd
" errors
"hi default link User3 Error
" fugitive
"hi default link User4 Special

" Include $HOME in cdpath
if has("file_in_path")
    let &cdpath=','.expand("$HOME").','.expand("$HOME").'.bin'
endif

" Better include path handling
set path+=src/
let &inc.=' ["<]'

" completion
set dictionary=/usr/share/dict/words

" Load matchit.vim to enable fancy % matching
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
    runtime! macros/matchit.vim
endif

if has("eval")
    " If we're in a wide window, enable line numbers.
    fun! <SID>WindowWidth()
        if winwidth(0) > 90
            setlocal foldcolumn=0
            setlocal number
        else
            setlocal nonumber
            setlocal foldcolumn=0
        endif
    endfun

    " Force active window to the top of the screen without losing its size.
    fun! <SID>WindowToTop()
        let l:h=winheight(0)
        wincmd K
        execute "resize" l:h
    endfun

    fun! <SID>MakeGenericCopyrightHeader()
        if input("Insert copyright header? (y/N) ") == "y"
            0 put ='# Copyright '.strftime('%Y').' '.g:full_identity
            put ='# Distributed under the terms of the GNU General Public License v2'
            $
        endif
    endfun

    fun! <SID>UpdateCopyrightHeaders()
        let l:a = 0
        for l:x in getline(1, 10)
            let l:a = l:a + 1
            if -1 != match(l:x, ' Copyright [- 0-9,]*20\(0[456789]\|1[01]\) Lasse Brun')
                if input("Update copyright header? (y/N) ") == "y"
                    call setline(l:a, substitute(l:x, '\(20[01][0123456789]\) Lasse',
                                \ '\1, '.strftime('%Y').' Lasse', ""))
                endif
            endif
        endfor
    endfun
endif

if has("autocmd") && has("eval")
    augroup ciaranm
    autocmd!

    " Automagic line numbers
    autocmd BufEnter * :call <SID>WindowWidth()
    " Always do a full syntax refresh
    autocmd BufEnter * syntax sync fromstart
    " For help files, move them to the top window and make <Return> behave like <C-]> (jump to tag)
    "autocmd FileType help :call <SID>WindowToTop()
    autocmd FileType help :set nonumber
    autocmd FileType help nmap <buffer> <Return> <C-]>
    " For svn-commit, don't create backups
    autocmd BufRead svn-commit.tmp :setlocal nobackup
    " m4 matchit support
    autocmd FileType m4 :let b:match_words="(:),`:',[:],{:}"
    " bash-completion ftdetects
    autocmd BufNewFile,BufRead /*/*bash*completion*/*
                \ if expand("<amatch>") !~# "ChangeLog"   |
                \     let b:is_bash = 1 | set filetype=sh |
                \ endif

    " update copyright headers
    autocmd BufWritePre * call <SID>UpdateCopyrightHeaders()

    try
        autocmd Syntax *
                    \ syn match VimModelineLine /^.\{-1,}vim:[^:]\{-1,}:.*/ contains=VimModeline |
                    \ syn match VimModeline contained /vim:[^:]\{-1,}:/
        hi def link VimModelineLine comment
        hi def link VimModeline     special
    catch
    endtry
    augroup END

    augroup bruners
    autocmd!

    " fugitive buffers
    autocmd User Fugitive
          \ if filereadable(fugitive#buffer().repo().dir('fugitive.vim')) |
          \   source `=fugitive#buffer().repo().dir('fugitive.vim')`      |
          \ endif
    " Remove 'dead' fugitive buffers
    autocmd BufReadPost fugitive://* set bufhidden=delete

    " Highlight whitespace in super annoying colors
    fun! <SID>WhiteSpace()
        hi ExtraWhitespace ctermbg=red guibg=red
        match ExtraWhiteSpace /\s\+$/
        match ExtraWhiteSpace /\s\+\%#\@<!$/
    endfun
    autocmd BufWinEnter,InsertEnter,InsertLeave * :call <SID>WhiteSpace()

    " Clear all matches previously defined by match
    autocmd BufWinLeave * call clearmatches()

    " Return to last edit position when opening files
    autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$")    |
        \   exe "normal! g`\"zvzz"                          |
        \ endif

    " Reload vimrc on save
    autocmd BufWritePost vimrc,.vimrc source ~/.vimrc

    " Reload .Xdefaults on save
    autocmd BufWritePost,FileWritePost ~/.Xdefaults,~/.Xresources silent! !xrdb -merge % >/dev/null 2>&1

    " Populate omnifunc
    autocmd FileType *
        \ if exists("+omnifunc") && &omnifunc == ""         |
        \     setlocal omnifunc=syntaxcomplete#Complete     |
        \ endif

    " Populate completefunc
    autocmd FileType *
        \ if exists("+completefunc") && &completefunc == "" |
        \     setlocal completefunc=syntaxcomplete#Complete |
        \ endif

    augroup END
endif

if has("autocmd")
    augroup content
    autocmd!

    autocmd BufNewFile *.hs 0put ='-- vim: set sw=4 sts=4 et tw=80 :' |
                \ set sw=4 sts=4 et tw=80 |
                \ norm G

    autocmd BufNewFile *.rb 0put ='# vim: set sw=4 sts=4 et tw=80 :' |
                \ 0put ='#!/usr/bin/ruby' | set sw=4 sts=4 et tw=80 |
                \ norm G

    autocmd BufNewFile *.hh 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
                \ 1put ='' | call MakeIncludeGuards() |
                \ set sw=4 sts=4 et tw=80 | norm G

    autocmd BufNewFile *.cc 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
                \ 1put ='' | 2put ='' | call setline(3, '#include "' .
                \ substitute(expand("%:t"), ".cc$", ".hh", "") . '"') |
                \ set sw=4 sts=4 et tw=80 | norm G

    autocmd BufNewFile *.pml 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
                \ set sw=4 sts=4 et tw=80 | norm G

    autocmd BufNewFile *.java 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
                \ 1put ='' |
                \ set sw=4 sts=4 et tw=80 | norm G

    autocmd BufNewFile *.sh 0put ='# vim: set sw=4 sts=4 et tw=100 foldmethod=syntax :' |
                \ 0put ='#!/bin/sh' | set sw=4 sts=4 et tw=100 |
                \ norm G | call <SID>MakeGenericCopyrightHeader()

    autocmd BufNewFile /home/lasseb/.bin/** 0put ='# vim: set sw=4 sts=4 et tw=100 foldmethod=syntax :' |
                \ 0put ='#!/bin/sh' | set sw=4 sts=4 et tw=100 |
                \ norm G | call <SID>MakeGenericCopyrightHeader()

    autocmd BufNewFile,BufRead *.md,*.markdown set filetype=ghmarkdown |
                \ norm G

    autocmd BufNewFile,BufRead *.txt,README,INSTALL,NEWS,TODO
                \ if &ft == "" | set ft=text | endif

    augroup END
endif

if has("eval") && has("autocmd")
    fun! <SID>check_pager_mode()
        if exists("g:loaded_less") && g:loaded_less
            " we're in vimpager / less.sh / man mode
            set laststatus=0
            set ruler
            set foldmethod=manual
            set foldlevel=99
            set nolist
        endif
    endfun
    autocmd VimEnter * :call <SID>check_pager_mode()
endif

if has("eval")

    " vim-bufferline
    let g:bufferline_echo = 1
    let g:bufferline_active_buffer_left=""
    let g:bufferline_active_buffer_right=""

    " Vim specific options
    let g:vimsyntax_noerror=1
    let g:vimembedscript=0

    " doxygen
    let g:load_doxygen_syntax=1
    let g:doxygen_end_punctuation='[.?]'

    " memememe
    let g:full_name='Lasse Brun'
    let g:full_identity=g:full_name.' <bruners@gmail.com>'

    " exheres syntax
    let g:exheres_author_name=g:full_identity
    let g:package_create_on_empty=1
    let g:common_metadata_create_on_empty=1

    " changelog syntax
    let g:changelog_username=g:full_identity

    " Settings for :TOhtml
    let html_number_lines=1
    let html_use_css=1
    let use_xhtml=1

    " Settings for NERDtree file browser
    let g:NERDTreeMouseMode=2
    let g:NERDTreeWinSize=20
    let g:NERDTreeMinimalUI=2
    let g:NERDTreeStatusline="%1*%{strftime('%H:%M')}"
    let g:nerdtree_tabs_open_on_gui_startup=0
    let g:nerdtree_tabs_focus_on_files=1

    " Git line status
    let g:gitgutter_enabled=1
    let g:gitgutter_highlight_lines=0

    " inkpot colors
    let g:inkpot_highlight_gitgutter=0
    let g:inkpot_black_background=0

    " Allow horizontal split in Gitv
    let g:Gitv_OpenHorizontal = 1
    cabbrev git Git

    if has("gui")
        " Enable showmarks.vim
        let g:showmarks_enable=1

        " Indent lines settings
        let g:indent_guides_enable_on_vim_startup=1
        let g:indent_guides_guide_size=1
        let g:indent_guides_start_level=2
    else
        let g:showmarks_enable=0
        let loaded_showmarks=1
    endif

    let g:showmarks_include="abcdefghijklmnopqrstuvwxyz"

endif

" turn off any existing search
if has("autocmd")
    au VimEnter * nohls
endif

"-----------------------------------------------------------------------
" vim: set shiftwidth=4 softtabstop=4 expandtab tw=120
