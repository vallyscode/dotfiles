autoload -Uz promptinit
promptinit
prompt adam2 # walters # adam1

setopt histignorealldups sharehistory

# Keep 1k lines of history within the shell and save it to ~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

alias ls='ls --color=auto'
alias pacman='sudo pacman --color=auto'
alias vim='nvim'

