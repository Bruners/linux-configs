fpath=($HOME/.zsh/func $HOME/.zsh/completions $fpath)
typeset -U fpath

## load completion system
skip_global_compinit=1
autoload run-help
autoload run-help-git

## make color constants available
autoload -U colors; colors

## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

autoload -U zutil
autoload -U compinit
compinit

## load the extract module from oh-my-zsh
autoload -U extract

export TERM=rxvt-unicode-256color

for part in $HOME/.zsh/rc.d/??_*; do
    source $part
done

# vim:set ft=zsh:
