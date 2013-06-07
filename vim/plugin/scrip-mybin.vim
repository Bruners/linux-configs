" Vim plugin
" Language:     Create new mybin script
" Author:       Lasse Brun
" Copyright:    Copyright 2013 Lasse Brun
" License:      You may redistribute this under the same terms as Vim itself

fun! <SID>GenerateMybinScript()
    let l:pastebackup = &paste
    set nopaste

    if exists("*strftime")
        let l:year = strftime("%Y")
    else
        let l:year = "<year>"
    endif

    put ='#/bin/zsh'
    put ='#vim: set et ts=4 sw=4 :'
    put! ='# Copyright ' . l:year . ' ' . g:exheres_author_name
    put ='# Distributed under the terms of the WTFPL v2 <http://sam.zoy.org/wtfpl/> Public License'
    put ='#'
    put ='# Description'
    put ='# Usage'
    put =''
    put =''

    nohls

    if pastebackup == 0
        set nopaste
    endif
endfun

com! -nargs=0 NewMybinScript call <SID>GenerateMybinScript()

if !exists("g:package_create_on_empty")
    let g:package_create_on_empty = 1
endif

if v:progname =~ "vimdiff"
    let g:package_create_on_empty = 0
endif

if !exists("g:exheres_author_name")
    let g:exheres_author_name = "<name>"
endif

augroup NewMybinScript
    au!
    autocmd BufNewFile ~/.bin/*.zsh
        \ if g:package_create_on_empty |
        \   call <SID>GenerateMybinScript() |
        \ endif
augroup END

" vim: set et ts=4 :
