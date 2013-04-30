" Vim syntaxfile
" Language:     Common code for mybin style scripts
" Author:       Lasse Brun
" Copyright:    Copyright 2013 Lasse Brun
" License:      You may redistribute this under the same terms as Vim itself

let is_bash = 1
runtime! syntax/sh.vim
unlet b:current_syntax

syn region MybinHeader contained start=/^#/ end=/$/ contains=MybinCopyHeader
syn region MybinHeaderBlock start=/\%^\(#\)\@=/ end=/^$/ contains=MybinHeader

" Unfilled copyright notice
syn region MybinCopyHeader contained start=/^#\s*Copyright/ end=/$/ contains=MybinCopyError
syn match MybinCopyError contained  /<\(name\|year\)>/

" Nifty funcs with colors
syn keyword MybinEssential      edo nonfatal diesafe

syn keyword MybinInform         notifyosd syslogger

syn keyword MybinChecks         option optionq optionfmt

" Highlight tabs and trailing whitespace as errors
syn match MybinError            "·⁖⁖"
syn match MybinError            "\s\+$"

" Highlight last line if it's not empty
syn match MybinError            /^.\+\%$/

" Bad variable assignments
syn match MybinError2           /^s*=\s*\(""\|''\|$\)/
syn match MybinError2           /\${\[- a-z,A-Z]}/

" Highlight it
syn cluster MybinContents       contains=MybinEssential,MybinInform,MybinTests
syn cluster MybinContents       add=MybinError,MybinError2

syn cluster shCommandSubList    add=@MybinContents
syn cluster shDblQuoteList      add=MybinError,MybinError2

" ExheresZeroCoreKeyword
hi def link MybinEssential      Keyword
" ExheresZeroFunctions
hi def link MybinInform         Special
" ExheresZeroRequire
hi def link MybinChecks         Include
" ExheresZeroError[c]
hi def link MybinError          Error
hi def link MybinError2         Error
" ExheresHeader, ExheresCopyrightHeader, ExheresCopyrightError
hi def link MybinHeader         Comment
hi def link MybinCopyHead       Comment
hi def link MybinCopyError      Error

" vim: set et ts=4 :
