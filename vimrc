" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
    finish
endif

call pathogen#infect()

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
" extra commandline menu
set wildmenu
set wildmode=longest:list:full

set lazyredraw
set whichwrap=b,s,<,>,[,]
set noswapfile
set nobackup

set smarttab
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=2

" autoread when file is changed after loading
set autoread

" timeoutlen is used for mapping delays, ttimeoutlen is used for key code delays.
set timeoutlen=1000 ttimeoutlen=0

set showbreak=>\ \ \
set fileformat=unix
set fileformats=unix,dos

let g:full_name='Lasse Brun'
let g:full_identity=g:full_name.' <bruners@gmail.com>'
let g:exheres_author_name=g:full_identity
let g:changelog_username=g:full_identity
let g:package_create_on_empty=1
let g:my_theme='molokai'
let g:common_metadata_create_on_empty=1
let g:SuperTabNoCompleteAfter=[',', '\s']
let g:NERDTreeMouseMode=2
let g:NERDTreeWinSize=20
let g:NERDTreeMinimalUI=2

" I want to save or quit too fast..
nmap :Q :q
nmap :W :w
nmap :Wq :wq
nmap q: :q

nmap <silent> <leader>/ :set invhlsearch<CR>

" Kill line
nnoremap <C-k><C-k> "_dd
inoremap <C-k> <C-o>dd

inoremap <S-V> <C-o>"+p<CR>
vnoremap <C-c> "+y<CR>
vnoremap <C-x> "+x<CR>

" F5 toggles paste mode
set pastetoggle=<F5>
" F3 toggle highlight
nnoremap <silent> <F3> :silent set invhlsearch<CR>
inoremap <silent> <F3> <C-o>:silent set invhlsearch<CR>

nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz

map <C-n> <plug>NERDTreeMirrorToggle<CR>

" tabs like a pro
nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv
vnoremap > >gv
vnoremap < <gv

" Map Control-# to switch tabs
map  <A-0> 0gt
imap <A-0> <Esc>0gt
map  <A-1> 1gt
imap <A-1> <Esc>1gt
map  <A-2> 2gt
imap <A-2> <Esc>2gt
map  <A-3> 3gt
imap <A-3> <Esc>3gt
map  <A-4> 4gt
imap <A-4> <Esc>4gt
map  <A-5> 5gt
imap <A-5> <Esc>5gt

inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

    " GUI stuff
if has('gui_running')
    " Enable showmarks plugin
    let g:showmarks_enable=1
    let g:showmarks_include="abcdefghijklmnopqrstuvwxyz"

    " Enable gitgutter (git line indicator)
    let g:gitgutter_enabled = 1
    " Allow horizontal split Gitv
    let g:Gitv_OpenHorizontal = 1
    cabbrev git Git

    " Indent lines settings
    let g:indent_guides_guide_size=1
    let g:indent_guides_start_level=2

    " Toggle toolbar & menu
    nmap <F2> :set guioptions-=mT<CR>
    nmap <c-F2> :set guioptions+=mT<CR>
    " Enable shift-insert paste in gui
    nnoremap <silent> <S-Insert> "+gP
    inoremap <silent> <S-Insert> <Esc>"+gP

    nmap <C-T> :tabnew<CR>
    nmap <C-W><C-W> :tabclose<CR>

    " menus and toolbar off by default
    set guioptions-=m " menubar
    set guioptions-=T " toolbar
    set guioptions-=l " no left scroll
    set guioptions-=L " really! no left scroll
    set guioptions+=r " always show right scroll
    set guioptions+=a "
    set guioptions+=F " footer motif?
    " Guitab label (tab#. title)
    set guitablabel=%N\ %t

    " Gui mouse
    set mousehide
    behave mswin
    set cmdheight=2
else
    let g:showmarks_enable=0
    let loaded_showmarks=1
    let g:gitgutter_enabled = 0
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
set scrolloff=4
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
set statusline+=%2*%-3.3n%0*                    " buffer number
set statusline+=%F\                             " file name
set statusline+=%h%1*%m%r%w%0*                  " flags
set statusline+=%1*%{exists('g:loaded_fugitive')?fugitive#statusline():''}%*
set statusline+=\[%{strlen(&ft)?&ft:'none'}%*/  " filetype
set statusline+=%{&encoding}/                   " encoding
set statusline+=%{&fileformat}]                 " file format
set statusline+=%=                              " right align
set statusline+=%2*0x%-8B\                      " current char
set statusline+=%-14.(%l,%c%V%)\ %P             " line, scroll

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

    let s:prefer_scheme = g:my_theme

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
if has("gui_running") && v:version >= 700 && s:prefer_scheme == g:my_theme
    set cursorline
end

if has("eval")
    filetype on
    filetype plugin on
    filetype indent on

    if has("gui_running") && v:version >=703 && s:prefer_scheme == g:my_theme
        let g:indent_guides_enable_on_vim_startup=1
    end
else
    set autoindent
    set smartindent
endif

if has("eval") && has("autocmd")
    if &omnifunc == "" |
      setlocal omnifunc=syntaxcomplete#Complete |
    endif
endif

if has("eval")
    fun! FixShowmarksColours()
        if has('gui')
            hi ShowMarksHLl gui=bold guifg=#a0a0e0 guibg=#2e2e2e
            hi ShowMarksHLu gui=none guifg=#a0a0e0 guibg=#2e2e2e
            hi ShowMarksHLo gui=none guifg=#a0a0e0 guibg=#2e2e2e
            hi ShowMarksHLm gui=none guifg=#a0a0e0 guibg=#2e2e2e

            hi FoldColumn guibg=grey guifg=blue
            hi Folded ctermfg=11 ctermbg=8 guibg=#444444 guifg=#cccccc
        endif
    endfun
    " If we're in a wide window, enable line numbers.
    fun! WindowWidth()
        if winwidth(0) > 90 && &ft !='help'
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
            autocmd BufNewFile,BufRead /{etc,lib*}/systemd/**.{conf,target,service,socket,mount,automount,swap,path,timer,snapshot,device} setl ft=desktop
            autocmd BufNewFile /home/lasseb/** call MakeGenericCopyrightHeader()
            autocmd BufNewFile *.sh* call MakeGenericCopyrightHeader()
            autocmd FileType text setlocal textwidth=100

            augroup markdown
                au!
                au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
            augroup END

            " Use omnifunc for tab completion
            autocmd FileType *
                \ if &omnifunc != '' |
                \   call SuperTabChain(&omnifunc, "<c-p>") |
                \   call SuperTabSetDefaultCompletionType("<c-x><c-u>") |
                \ endif
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
            autocmd FileType help :set nonumber
            autocmd FileType help nmap <buffer> <Return> <C-]>
            " Update copyright headers
            autocmd BufWritePre * call UpdateCopyrightHeaders()
            " Reload vimrc on save
            autocmd bufwritepost vimrc,.vimrc source ~/.vimrc
            " Return to last edit position when opening files
            autocmd BufReadPost *
             \ if line("'\"") > 0 && line("'\"") <= line("$") |
             \   exe "normal! g`\"zvzz" |
             \ endif
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
    endif
endif

" vim: set shiftwidth=4 softtabstop=4 expandtab tw=120
