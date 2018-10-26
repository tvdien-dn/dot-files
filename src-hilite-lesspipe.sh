#! /bin/sh

for source in "$@"; do
    case $source in
        *ChangeLog|*changelog)
        source-highlight --failsafe -f esc --lang-def=changelog.lang --style-file=esc.style -i "$source" ;;
        *Makefile|*makefile)
        source-highlight --failsafe -f esc --lang-def=makefile.lang --style-file=esc.style -i "$source" ;;
        *.rake)
        source-highlight --failsafe --infer-lang -f esc --style-file=esc.style --src-lang=rb -i "$source" ;;
        *.tar|*.tgz|*.gz|*.bz2|*.xz)
        lesspipe "$source" ;;
        *) source-highlight --failsafe --infer-lang -f esc --style-file=esc.style -i "$source" ;;
        # *) source-highlight --lang-map=$(brew --prefix source-highlight)/share/source-highlight/lang.map --outlang-map=$(brew --prefix source-highlight)/share/source-highlight/outlang.map --failsafe --infer-lang -f esc --style-file=esc.style -i "$source" ;;
    esac
done
