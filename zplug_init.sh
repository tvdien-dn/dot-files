if [ -e $ZPLUG_HOME/init.zsh ]; then
  source $ZPLUG_HOME/init.zsh
  zplug 'zplug/zplug', hook-build:'zplug --self-manage'
  zplug "zsh-users/zsh-syntax-highlighting", defer:2
  zplug "zsh-users/zsh-history-substring-search"
  zplug "zsh-users/zsh-completions"
  zplug "zsh-users/zsh-autosuggestions"
  zplug "b4b4r07/enhancd", use:init.sh
  zplug "plugins/git", from:oh-my-zsh, as:plugin
  zplug "plugins/common-aliases", from:oh-my-zsh, as:plugin
  zplug "plugins/emacs", from:oh-my-zsh, as:plugin
  zplug mafredri/zsh-async, from:github
  zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme

  if ! zplug check; then
    zplug install
  fi
  zplug load

  bindkey -M emacs '^P' history-substring-search-up
  bindkey -M emacs '^N' history-substring-search-down
else
  echo 'NO ZPLUG :('
fi
