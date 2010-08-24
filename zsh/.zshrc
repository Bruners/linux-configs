fpath=($HOME/.zsh/functions $fpath)

# colors
eval `dircolors $HOME/.zsh/colors`

autoload -U zutil
autoload -U compinit
autoload -U complist
zstyle ':completion::complete:*' use-cache 1
# mad I know, prefering emacs bindings when I'm a vim user!
bindkey -e

#bindkey '\e[A' history-search-backward
#bindkey '\e[B' history-search-forward
bindkey '^K' kill-whole-line
bindkey "\e[H" beginning-of-line        # Home (xorg)
bindkey "\e[1~" beginning-of-line       # Home (console)
bindkey "\e[4~" end-of-line             # End (console)
bindkey "\e[F" end-of-line              # End (xorg)
bindkey "\e[2~" overwrite-mode          # Ins
bindkey "\e[3~" delete-char             # Delete
#bindkey '\eOH' beginning-of-line
#bindkey '\eOF' end-of-line

# Activation
compinit

# Resource files
for file in $HOME/.zsh/rc/*.rc; do
	source $file
done
