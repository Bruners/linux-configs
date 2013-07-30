scriptencoding utf-8

" ~.vimrc mostly stolen from Ciaran McCreesh - https://github.com/ciaranm/

"-----------------------------------------------------------------------
" terminal setup
"-----------------------------------------------------------------------

if (&term =~ "xterm") && (&termencoding == "")
    set termencoding=utf8
endif

if &term =~ "xterm"
    if has('title')
        set title
    endif

    " change cursor colour depending upon mode
    if exists('&t_SI')
        let &t_SI = "\<Esc>]12;lightgoldenrod\x7"
        let &t_EI = "\<Esc>]12;grey80\x7"
    endif
endif

"-----------------------------------------------------------------------
" unbundle init
"-----------------------------------------------------------------------

" Call ubundle
runtime bundle/vim-unbundle/unbundle.vim

"-----------------------------------------------------------------------
" settings
"-----------------------------------------------------------------------

" Don't be compatible with vi
set nocompatible

" Enable a nice big viminfo file
set viminfo='1000,f1,:1000,/1000
set viminfo^=!
set history=500
" Make backspace delete lots of things
set backspace=indent,eol,start

" Create backups and undofile
set backup
set backupdir=~/.vim/tmp/backups

" Create a undofile
set undofile
set undodir=~/.vim/tmp
set undolevels=50
set updatetime=1000

" No swapfiles
set noswapfile
" Show us the command we're typing
set showcmd
" Highlight matching parens
set showmatch

if has("autocmd")
    autocmd BufEnter *
                \ if &filetype == "cpp" || &filetype == "c" || &filetype == "haskell" || &filetype == "java" |
                \    set noignorecase noinfercase |
                \ else |
                \    set ignorecase infercase |
                \ endif
else
    set ignorecase
    set infercase
endif

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
    autocmd GUIEnter * set visualbell t_vb=
endif

" Copy indent from the current line
set autoindent

" Opens new panes to right/bottom, which is more natural
set splitbelow
set splitright

if has('gui_gtk')
    set lines=42
    set columns=150
endif

" screw you tabs
set expandtab

" dusplay as much as possible of the last line
set display+=lastline

" Try to show at least n lines and columns when scrolling
set scrolloff=3
set sidescrolloff=5
" Wrap
set whichwrap+=<,>,[,]
" Completion menu
set wildmenu
set wildchar=<tab>
set wildmode=longest:list:full
set wildignore+=*.o,*~,.lo,*.hi
set wildignore+=.git,.svn,.hg
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.egg-info/*
set suffixes+=.in,.a,.1

" allow hidden buffers
set hidden

" 1 height windows
set winminheight=1

" tell buffers to use tabs
try
    set switchbuf=usetab,newtab
catch
endtry

if has('syntax') && !exists('g:syntax_on')
    " Syntax highlighting
    syntax enable
endif

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

" Folding
if has("folding")
    set foldenable
    set foldmethod=manual
    set foldlevelstart=99
endif

" syntax when printing
set popt+=syntax:y

if has('eval')
    filetype on
    filetype plugin on
    filetype indent on
endif

" Disable modelines, use securemodelines.vim instead
set nomodeline
let g:secure_modelines_verbose = 0
let g:secure_modelines_modelines = 5
au VimEnter * call filter(exists("g:secure_modelines_allowed_items") ? g:secure_modelines_allowed_items : [],
            \ 'v:val != "fdm" && v:val != "foldmethod"')

" Nice statusbar
set laststatus=2
" Call powerline
set rtp+=~/github/powerline/powerline/bindings/vim
set noshowmode

" Nice window title
if has('title') && (has('gui_running') || &title)
    set titlestring=
    set titlestring+=%f\                                              " file name
    set titlestring+=%h%m%r%w                                         " flags
    set titlestring+=\ -\ %{v:progname}                               " program name
    set titlestring+=\ -\ %{substitute(getcwd(),\ $HOME,\ '~',\ '')}  " working directory
endif

" wrap, continue and remove comments in insertmode
set formatoptions=croqj
" ignore textwidth for long lines in insertmode
set formatoptions+=l

set shiftround
set complete-=i
set completeopt=longest,menuone
set mouse=a
" autoread when file is changed after loading
set autoread

" Show tabs and trailing whitespace visually
if (&termencoding == "utf-8") || has("gui_running")
    if has("gui_running")
        set list listchars=tab:⋮\ ,trail:⌴,extends:▸,precedes:◂,nbsp:‗
        set showbreak=↪
        set fillchars=vert:│,fold:┄,diff:╱
    else
        set list listchars=tab:·⁖,trail:¶,extends:»,precedes:«,nbsp:_
        set fillchars=fold:-
    endif
else
    set list listchars=tab:>-,trail:.,extends:>,nbsp:_
    set fillchars=fold:-
endif

" Include $HOME in cdpath
if has("file_in_path")
    let &cdpath=','.expand("$HOME").','.expand("$HOME").'.bin'
endif

"-----------------------------------------------------------------------
" completion
"-----------------------------------------------------------------------

set dictionary=/usr/share/dict/words

"-----------------------------------------------------------------------
" autocmds
"-----------------------------------------------------------------------

if has("eval")
    " If we're in a wide window, enable line numbers.
    fun! <SID>WindowWidth()
        if winwidth(0) > 90
            setlocal foldcolumn=1
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
            if -1 != match(l:x, 'Copyright [- 0-9,]*20\(0[456789]\|1[01]\) Lasse Brun')
                if input("Update copyright header? (y/N) ") == "y"
                    call setline(l:a, substitute(l:x, '\(20[01][0123456789]\) Lasse',
                                \ '\1, 2013 Lasse', ""))
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
        "fun! <SID>WhiteSpace()
        "    hi ExtraWhitespace cterm=bold ctermbg=darkgreen guibg=darkgreen
        "    match ExtraWhitespace / \+\ze\t/
        "    match ExtraWhiteSpace /\s\+$/
        "    match ExtraWhiteSpace /\s\+\%#\@<!$/
        "endfun
        "autocmd BufWinEnter,InsertEnter,InsertLeave * :call <SID>WhiteSpace()

        " Return to last edit position when opening files
        autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$")    |
            \   exe "normal! g`\"zvzz"                          |
            \ endif
    augroup END

    " timeoutlen is used for mapping and macro delays
    " ttimeoutlen is used for key code delays
    set ttimeoutlen=10
    augroup FastEscape
        autocmd!
        autocmd InsertEnter * set timeoutlen=0
        autocmd InsertLeave * set timeoutlen=1000
    augroup END

    augroup onwriteloaders
        autocmd!

        " Reload dircolors on save
        autocmd BufWritePost,FileWritePost *.zsh/dircolors silent! !eval `dircolors -b ~/.zsh/dircolors`
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
                    \ 1put ='' | "call MakeIncludeGuards() |
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

        autocmd BufNewFile,BufRead *.md,*.markdown
                    \ if &ft == "" | set filetype=ghmarkdown | norm G | endif

        autocmd BufNewFile,BufRead *.json
                    \ if &ft == "" | set filetype=json | endif
    augroup END

    augroup filetypes
        autocmd!

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

"-----------------------------------------------------------------------
" gui settings / gvimrc
"-----------------------------------------------------------------------

if has('gui_running')
    " menus and toolbar off by default
    set guioptions-=m " nomenubar
    set guioptions-=T " notoolbar
    set guioptions-=l " no left scroll
    set guioptions-=L " really! no left scroll
    set guioptions+=r " always show right scroll
    set guioptions+=a " autoselect

    set guicursor+=a:blinkon0

    " Gui mouse
    set mousehide
    behave mswin

    set cmdheight=2
    set cursorline

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
                let label .= '¿'
                break
            endif
        endfor
        return label
    endfun

    " Guitab label (tab#. title)
    set guitablabel=%{GuiTabLabel()}

    " F2 Toggle toolbar & menu
    noremap <silent> <F2> :if &guioptions =~# 'T' <Bar>
        \   set guioptions-=T <Bar>
        \   set guioptions-=m <Bar>
        \ else <Bar>
        \   set guioptions+=T <Bar>
        \   set guioptions+=m <Bar>
        \ endif<CR>

    " Enable shift-insert paste in gui
    noremap <S-Insert> <MiddleMouse>
    noremap! <S-Insert> <MiddleMouse>

    "Open new file dialog (ctrl-o)
    noremap <c-o> :browse confirm e<cr>
    "Open save-as dialog (ctrl-shift-s)
    noremap <c-s-s> :browse confirm saveas<cr>

    " Map Alt-# to switch tabs
    noremap <a-0> <esc>0gt
    noremap <a-1> <esc>1gt
    noremap <a-2> <esc>2gt
    noremap <a-3> <esc>3gt
    noremap <a-4> <esc>4gt
    noremap <a-5> <esc>5gt
    noremap <a-6> <esc>6gt
    noremap <a-7> <esc>7gt

endif

"-----------------------------------------------------------------------
" mappings
"-----------------------------------------------------------------------

" Make space more useful they said
nnoremap <space> za

" Kill line
noremap <c-k>      "_dd
noremap <s-del>    "_dd
inoremap <c-k>  <esc>ddi

" CTRL-X and SHIFT-Del are Cut
vnoremap     <c-x>      "+x
vnoremap     <s-del>    "+x

" CTRL-C and CTRL-Insert are Copy
vnoremap    <c-c>       "+y

" SHIFT-Insert is Paste
noremap     <s-insert>  "+gP
cnoremap    <s-insert>  <c-r>+

" Change case
nnoremap <c-u> gUiw
inoremap <c-u> <esc>gUiwea

" Use CTRL-Q to do what CTRL-V used to do
noremap         <c-q>       <c-v>

" paste and indent lines
nnoremap p p`[v`]=
" New tab
noremap <c-t> :tabnew<cr>
" Close tab
noremap <c-q>! :quit<cr>

" F1 is even more annoying
inoremap <F1> <esc>

" F3 toggle highlight and clear with <C-l>
nnoremap <silent> <F3> :silent nohlsearch<cr>
inoremap <silent> <F3> <c-o>:silent nohlsearch<cr>
if maparg('<C-L>', 'n') ==# ''
    nnoremap <silent> <c-l> :nohlsearch<cr><c-l>
endif

" F5 toggles paste mode
set pastetoggle=<F5>

" F6 inserts generic copyright header
nnoremap <silent> <F6> :call <SID>MakeGenericCopyrightHeader()<cr>

" Annoying default mappings
noremap K k
noremap J j
nnoremap n nzz
nnoremap N Nzz

nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" * is silly
noremap * :let @/='\<'.expand('<cword>').'\>'<bar>:set hls<cr>
noremap g* :let @/=expand('<cword>')<bar>:set hls<cr>

map <leader><leader> <plug>NERDTreeMirrorToggle<cr>

"tabs like a pro
nnoremap <tab> >>_
nnoremap <s-tab> <<_
vnoremap <tab> >gv
vnoremap <s-tab> <gv

" Write with sudo.
cnoremap w!! w !sudo tee % >/dev/null

" Load matchit.vim to enable fancy % matching
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
    runtime! macros/matchit.vim
endif

if has('eval')
    command! KeepWindowBufferWipe enew|bw #

    " Delete a buffer but keep layout
    nnoremap <c-w>! KeepWindowBufferWipe<cr>
endif

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
endif

"-----------------------------------------------------------------------
" special less.sh and man modes
"-----------------------------------------------------------------------

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

"-----------------------------------------------------------------------
" plugin / script / app settings
"-----------------------------------------------------------------------

if has("eval")

    " Set default tab width
    let g:tabwidth = 4

    exec 'set shiftwidth=' . g:tabwidth
    exec 'set tabstop=' . g:tabwidth
    exec 'set softtabstop=' . g:tabwidth

    " vim-pager
    let vimpager_use_gvim = 1
    let vimpager_passthrough = 0
    let vimpager_scrolloff = 5
    let vimpager_disable_x11 = 1

    " vim-bufferline
    let g:bufferline_echo = 0

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
    let g:systemd_create_on_empty=1

    " changelog syntax
    let g:changelog_username=g:full_identity

    " Settings for :TOhtml
    let g:html_number_lines=1
    let g:html_use_css=1
    let g:html_use_encoding = "utf8"
    let use_xhtml=1

    " Settings for NERDtree file browser
    let NERDCreateDefaultMappings = 0
    let NERDTreeDirArrows = 1
    let NERDTreeWinPos = 'right'
    let NERDTreeMinimalUI = 1
    let NERDTreeAutoDeleteBuffer=0
    let NERDTreeShowBookmarks = 1
    let g:nerdtree_tabs_focus_on_files=0
    let g:nerdtree_tabs_no_startup_for_diff=1

    " Git line status
    let g:gitgutter_sign_added = '∷'
    let g:gitgutter_sign_modified = '≞'
    let g:gitgutter_sign_removed = '∸'
    let g:gitgutter_sign_modified_removed = '∵'

    " inkpot colors
    let g:inkpot_highlight_gitgutter=0
    let g:inkpot_black_background=0

    " haskellmode
    let g:haddock_browser="/usr/bin/firefox"
    let g:haddock_docdir="/usr/local/share/doc/ghc/html/"
    let g:haddock_indexfiledir="~/.vim/tmp/"

    " Allow horizontal split in Gitv
    let g:Gitv_OpenHorizontal = 1
    cabbrev git Git

    if has("gui_running")
        " Enable showmarks.vim
        let g:showmarks_enable=1
        let g:indentLine_char = '│'
    else
        let g:showmarks_enable=0
        let loaded_showmarks=1
        let g:indentLine_enabled = 0
    endif

    let g:showmarks_include="abcdefghijklmnopqrstuvwxyz"

    if has("autocmd")
        fun! <SID>FixShowmarksColours()
            if has('gui')
                hi ShowMarksHLl gui=bold guifg=#a0a0e0 guibg=#232526
                hi ShowMarksHLu gui=none guifg=#a0a0e0 guibg=#232526
                hi ShowMarksHLo gui=none guifg=#a0a0e0 guibg=#232526
                hi ShowMarksHLm gui=none guifg=#a0a0e0 guibg=#232526
            endif
        endfun
        if v:version >= 700
            autocmd VimEnter,Syntax,ColorScheme * call <SID>FixShowmarksColours()
        else
            autocmd VimEnter,Syntax * call <SID>FixShowmarksColours()
        endif
    endif

endif

"-----------------------------------------------------------------------
" final commands
"-----------------------------------------------------------------------

" turn off any existing search
if has("autocmd")
    autocmd VimEnter * nohls
endif

"-----------------------------------------------------------------------
" vim: set shiftwidth=4 softtabstop=4 expandtab tw=120
