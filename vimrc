" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
    finish
endif

set nocompatible
" Disable modelines, use securemodelines.vim instead
set nomodeline
let g:secure_modelines_verbose = 0
let g:secure_modelines_modelines = 15

au VimEnter * call filter(exists("g:secure_modelines_allowed_items") ? g:secure_modelines_allowed_items : [],
            \ 'v:val != "fdm" && v:val != "foldmethod"')

if (&term =~ "xterm") && (&termencoding == "")
    set termencoding=utf-8
endif
if has('title')
    set title
endif

set history=50
set showmode
set showfulltag
set showcmd
set showmatch
set nohidden

set lazyredraw
set whichwrap+=<,>,[,]
set noswapfile
set nobackup

set smarttab
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4

set showbreak=>\ \ \
set fileformat=unix
set fileformats=unix,dos

let g:full_name='Lasse Brun'
let g:full_identity=g:full_name.' <bruners@gmail.com>'
let g:exheres_author_name=g:full_identity
let g:package_create_on_empty=1
let g:common_metadata_create_on_empty=1

"let g:vimembedscript=0
"let g:haskell_indent_if = 2

" Settings for showmarks.vim
if has("gui_running")
    let g:showmarks_enable=1
else
    let g:showmarks_enable=0
    let loaded_showmarks=1
endif

let g:showmarks_include="abcdefghijklmnopqrstuvwxyz"

" I want to save or quit too fast..
nmap :Q :q
nmap :W :w
nmap :Wq :wq
nmap q: :q

" c^w buffer next
nmap <C-w> :bn<CR>

" Kill line
vmap K k
noremap <C-k> "_dd
inoremap <C-k> <C-o>dd

noremap <C-y> "_P
inoremap <C-y> <C-o>P

" F5 toggles paste mode
set pastetoggle=<F5>

noremap <silent> <F3> :silent nohlsearch<CR>
inoremap <silent> <F3> :silent nohlsearch<CR>

" Toggle toolbar, menu in the GUI
if has('gui')
    nmap <F2> :set guioptions+=mT<CR>
    nmap <c-F2> :set guioptions-=mT<CR>

    " menus and toolbar off by default
    set guioptions+=m
    set guioptions+=T
    "set guioptions-=lL
    "set guioptions-=rR

    set mousehide
else
    if has('mouse')
        set mouse=a
        set ttymouse=xterm2
    endif
endif

" Hilight searches
if &t_Co > 2 || has("gui_running")
    syntax on
    set hlsearch
endif

" Try to show at least three lines and two columns of context when scrolling
set scrolloff=3
set sidescrolloff=2
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

" Enable folds
if has("folding")
    set foldenable
    set foldmethod=marker
endif

" Nice statusbar
set laststatus=2
set statusline=
set statusline+=%2*%-3.3n%0*\                " buffer number
set statusline+=%f\                          " file name
if has("eval")
    let g:scm_cache = {}
    fun! ScmInfo()
        let l:key = getcwd()
        if ! has_key(g:scm_cache, l:key)
            if (isdirectory(getcwd() . "/.git"))
                let g:scm_cache[l:key] = "[" . substitute(readfile(getcwd() . "/.git/HEAD", "", 1)[0],
                            \ "^.*/", "", "") . "] "
            else
                let g:scm_cache[l:key] = ""
            endif
        endif
        return g:scm_cache[l:key]
    endfun
    set statusline+=%{ScmInfo()}             " scm info
endif

set statusline+=%h%1*%m%r%w%0*               " flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}, " filetype
set statusline+=%{&encoding},                " encoding
set statusline+=%{&fileformat}]              " file format
set statusline+=%=                           " right align
set statusline+=%2*0x%-8B\                   " current char
set statusline+=%<%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
"set statusline+=%-14.(%l,%c%V%)\ %<%P        " offset

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

    let s:prefer_scheme = "inkpot"

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

" If possible and in gvim with inkpot, use cursor row highlighting
if has("gui_running") && v:version >= 700 && s:prefer_scheme == "inkpot"
    set cursorline
end

if has("eval")
    filetype on
    filetype plugin on
    filetype indent on

    fun! FixShowmarksColours()
        if has('gui')
            hi ShowMarksHLl gui=bold guifg=#a0a0e0 guibg=#2e2e2e
            hi ShowMarksHLu gui=none guifg=#a0a0e0 guibg=#2e2e2e
            hi ShowMarksHLo gui=none guifg=#a0a0e0 guibg=#2e2e2e
            hi ShowMarksHLm gui=none guifg=#a0a0e0 guibg=#2e2e2e
            hi SignColumn   gui=none guifg=#f0f0f8 guibg=#2e2e2e
        endif
    endfun
    " If we're in a wide window, enable line numbers.
    fun! WindowWidth()
        if winwidth(0) > 90
            setlocal number
        else
            setlocal nonumber
        endif
    endfun
    " Force active window to the top of the screen without losing its size.
    fun! WindowToTop()
        let l:h=winheight(0)
        wincmd K
        execute "resize" l:h
    endfun

    fun! MakeGenericCopyrightHeader()
        if input("Insert copyright header? (y/N) ") == "y"
            0 put ='# Copyright '.strftime('%Y').' '.g:full_identity
            put ='# Distributed under the terms of the GNU General Public License v2'
            $
        endif
    endfun

    fun! UpdateCopyrightHeaders()
        let l:a = 0
        for l:x in getline(1, 10)
            let l:a = l:a + 1
            if -1 != match(l:x, ' Copyright [- 0-9,]*20\(0[456789]\|1[01]\) '.g:full_identity)
                if input("Update copyright header? (y/N) ") == "y"
                    call setline(l:a, substitute(l:x, '\(20[01][0123456789]\) '.g:full_name,
                                \ '\1, '.strftime('%Y').' '.g:full_name,""))
                endif
            endif
        endfor
    endfun

    if has("autocmd")

        if v:version >= 700
            autocmd VimEnter,Syntax,ColorScheme * call FixShowmarksColours()
        else
            autocmd VimEnter,Syntax * call FixShowmarksColours()
        endif

        augroup content
            autocmd!
            autocmd BufNewFile *.hs 0put ='-- vim: set sw=4 sts=4 et tw=80 :' |
                \ set sw=4 sts=4 et tw=80 |
                \ norm G
            " use ghc functionality for haskell files
            "autocmd BufEnter *.hs compiler ghc
            autocmd BufNewFile,BufRead /{etc,lib*}/systemd/**.{conf,target,service,socket,mount,automount,swap,path,timer,snapshot,device} setl ft=desktop
            autocmd BufNewFile /home/lasseb/** call MakeGenericCopyrightHeader()
            autocmd BufNewFile *.sh* call MakeGenericCopyrightHeader()
            autocmd FileType text setlocal textwidth=100
            autocmd BufReadPost **.git/COMMIT_EDITMSG exe "normal gg"
        augroup END

        augroup bruners
            autocmd!
            highlight ExtraWhitespace ctermbg=red guibg=red
            match ExtraWhitespace /\s\+$/
            autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
            autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
            autocmd InsertLeave * match ExtraWhitespace /\s\+$/
            autocmd BufWinLeave * call clearmatches()
            " Automagic line numbers
            autocmd BufEnter * :call WindowWidth()
            " Always do a full syntax refresh
            autocmd BufEnter * syntax sync fromstart
            " For help files, move them to the top window and make <Return> behave like <C-]> (jump to tag)
            autocmd FileType help :call WindowToTop()
            autocmd FileType help nmap <buffer> <Return> <C-]>
            " update copyright headers
            autocmd BufWritePre * call UpdateCopyrightHeaders()

            try
                autocmd Syntax *
                    \ syn match VimModelineLine /^.\{-1,}vim:[^:]\{-1,}:.*/ contains=VimModeline |
                    \ syn match VimModeline contained /vim:[^:]\{-1,}:/
                hi def link VimModelineLine comment
                hi def link VimModeline     special
            catch
            endtry

            autocmd vimEnter * nohls
        augroup END
    else
        set autoindent
    endif
endif

if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
        \ | wincmd p | diffthis
endif

" vim: set shiftwidth=4 softtabstop=4 expandtab tw=120
