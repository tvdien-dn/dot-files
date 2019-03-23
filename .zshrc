HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
CONFIG_DIR=$HOME/.config
setopt append_history
setopt extended_history
setopt share_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt auto_cd
setopt auto_pushd
setopt magic_equal_subst
setopt print_eight_bit

# rootは履歴を残さないようにする
if [ $UID = 0 ]; then
    unset HISTFILE
    SAVEHIST=0
fi
## zplug setting https://github.com/zplug/zplug
export ZPLUG_HOME=/usr/local/opt/zplug
typeset -U path PATH
if [ -z $TMUX ]; then
  export ANYENV_ROOT="$HOME/.anyenv"
  custom_paths=($HOME/.anyenv/bin(N-/)
                $HOME/Library/Android/sdk/platform-tools(N-/)
                $HOME/go/bin(N-/)
                $HOME/.gem/ruby/2.3.0/bin
                $HOME/Library/Python/3.7/bin(N-/)
                $HOME/Library/Python/2.7/bin(N-/)
                /usr/local/sbin(N-/)
                /usr/local/aws/bin(N-/)
                /usr/local/bin(N-/)
                /usr/local/opt/qt@5.5/bin(N-/)
               )
  export path=($custom_paths $path)
fi
if [ -e $ZPLUG_HOME/init.zsh ]; then
  source $ZPLUG_HOME/init.zsh
  zplug "zsh-users/zsh-syntax-highlighting", defer:2
  zplug "zsh-users/zsh-history-substring-search"
  zplug "zsh-users/zsh-completions"
  zplug "zsh-users/zsh-autosuggestions"
  zplug "b4b4r07/enhancd", use:init.sh
  export ENHANCD_FILTER=fzf
  zplug "plugins/git", from:oh-my-zsh, as:plugin
  zplug "plugins/common-aliases", from:oh-my-zsh, as:plugin
  zplug "plugins/emacs", from:oh-my-zsh, as:plugin
  # zplug 'zplug/zplug', hook-build:'zplug --self-manage'
  zplug load
else
  echo 'NO ZPLUG :('
fi
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# load theme
source $ZDOTDIR/customized.zsh-theme

# Prevent duplicate defined when use tmux

eval "$(anyenv init - --no-rehash)"
eval "$(pyenv virtualenv-init -)"
eval "$(direnv hook zsh)"

typeset -U path PATH

export fpath=($fpath $HOME/.config/.zsh/completion(N-/))

if [ -e $CONFIG_DIR/iterm2/iterm2_shell_integration.zsh ]; then
  source $CONFIG_DIR/iterm2/iterm2_shell_integration.zsh
fi

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export LESS='-gj10 -RNC'
export LESSOPEN="|$HOME/.config/less_pygmentize.sh %s"
# alias tmux='direnv exec / tmux'

# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' menu select=2
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle ':completion:*:manuals' separate-sections true
zstyle :compinstall filename '/Users/mars_tran/dotfiles/.zshrc'

autoload -Uz compinit
compinit -i
# End of lines added by compinstall

if [ -e $HOME/Library/Python/3.7/bin/aws_zsh_completer.sh ]; then
  source $HOME/Library/Python/3.7/bin/aws_zsh_completer.sh
fi

if which pip 1>/dev/null 2>&1; then
  if [ ! -e $HOME/.config/.zsh/completion/_pip ]; then
    pip completion --zsh >> $HOME/.zsh/completion/_pip
  fi
  source $HOME/.config/.zsh/completion/_pip 
fi

export ZSH_AUTOSUGGEST_USE_ASYNC=true
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
export GPG_TTY=$(tty)
export PAGER='LESS= less -FXR'

if [ ~/dotfiles/.zshrc -nt ~/dotfiles/.zshrc.zwc ]; then
  zcompile ~/dotfiles/.zshrc
fi
