#!/bin/bash

if [ -d $HOME/.config ]; then
  config_dir="$HOME/.config"
else
  exit 1;
fi

targets=(
  .editorconfig
  .tmux.conf
  .emacs.d
  .gitconfig
  .pryrc
  .vimrc
)

for target in ${targets[@]}; do
  if [ -e "$config_dir/$target" ]; then
    if [ ! -e "$HOME/$target" ]; then
      ln -s "$config_dir/$target" "$HOME/$target"
      echo 'Added link:' $target
    else
      echo 'Already exists: ' $target
    fi
  else
    echo 'No source:' $target
  fi
done
