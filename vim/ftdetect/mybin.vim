" Vim filetype detection file
" Language:     Shell scripts
" Author:       Lasse Brun
" Copyright:    Copyright 2012 Lasse Brun
" License:      You may redistribute this under the same terms as Vim itself

au BufNewFile,BufRead ~/.bin/*.sh set filetype=mybin-sh
"au BufNewFile,BufRead ~/.config/systemd/*/*.service set filetype=systemd-user-service

" vim: set et ts=4
