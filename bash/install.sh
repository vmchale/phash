#!/usr/bin/env sh

set -e
set pipefail

getTarget() {
    if [ "$(uname)" = "Darwin" ]
    then
        echo "phash-$(uname -m)-apple-darwin"
    else
        echo "phash-$(uname -m)-unknown-linux-gnu"
    fi
}

addBin() {

    printf 'export PATH=$HOME/.local/bin:$PATH' >> "$HOME"/.bashrc
    export PATH=$HOME/.local/bin:$PATH

}

main() {


    mkdir -p "$HOME/.local/bin"

    latest="$(curl -Ls -o /dev/null -w %\{url_effective\} https://github.com/vmchale/phash/releases/latest | cut -d'"' -f2 | rev | cut -d'/' -f1 | rev)"
    binname=$(getTarget)

    url="https://github.com/vmchale/phash/releases/download/$latest/$binname"

    dest=$HOME/.local/bin/phash

    if command -v wget > /dev/null ; then
        wget "$url" -O "$dest"
    else
        curl -L "$url" -o "$dest"
    fi

    chmod +x "$dest"

    case :$PATH: in 
        *:$HOME/.local/bin:*) ;;
        *) addBin ;;
    esac

}

main
