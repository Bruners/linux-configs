" Vim plugin
" Language:     Create new mybin script
" Author:       Lasse Brun
" Copyright:    Copyright 2013 Lasse Brun
" License:      You may redistribute this under the same terms as Vim itself

let g:package_create_executable=0

fun! <SID>GenerateNewMybin()
    let l:pastebackup = &paste
    set nopaste

    if exists("*strftime")
        let l:year = strftime("%Y")
    else
        let l:year = "<year>"
    endif

    put! ='# Copyright ' . l:year . ' ' . g:exheres_author_name
    put ='# Distributed under the terms of the GNU General Public License v2'
    put ='#'
    put ='# Description'
    put =''
    put =''

    nohls

    if pastebackup == 0
        set nopaste
    endif
endfun

com! -nargs=0 NewMybin call <SID>GenerateNewMybin()

if !exists("g:package_create_on_empty")
    let g:package_create_on_empty = 1
endif

if v:progname =~ "vimdiff"
    let g:package_create_on_empty = 0
endif

if !exists("g:exheres_author_name")
    let g:exheres_author_name = "<name>"
endif

augroup NewMybin
    au!
    autocmd BufNewFile ~/.bin/*.sh
        \ if g:package_create_on_empty |
        \   call <SID>GenerateNewMybin() |
        \   silent execute '!chmod u+x %' |
        \   silent edit |
        \   syntax enable |
        \ endif
augroup END

" vim: set et ts=4 :
