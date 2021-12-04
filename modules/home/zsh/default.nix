{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    defaultKeymap = "emacs";
    plugins = (with pkgs; [
      {
        name = "zsh-powerlevel10k";
        src = zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
      {
        name = "zsh-history-substring-search";
        src = zsh-history-substring-search;
        file =
          "share/zsh-history-substring-search/zsh-history-substring-search.zsh";
      }
    ]);
    initExtra = ''
      if [[ "$INSIDE_EMACS" = 'vterm' ]] \
         && [[ -n "$EMACS_VTERM_PATH" ]] \
            && [[ -f "$EMACS_VTERM_PATH"/etc/emacs-vterm-zsh.sh ]]; then
               source "$EMACS_VTERM_PATH"/etc/emacs-vterm-zsh.sh
      fi
      weather () {
              curl https://wttr.in/"$1"
      }
      idot () {
              dot -Tpng $*
      }
      nr () {
              nix run nixpkgs#"$1"
      }
      nsh () {
              nix shell --impure --expr "with (import (builtins.getFlake \"nixpkgs\") {}); $*"
      }
      ni () {
              nix flake init -t my#"$1"
      }
      nn () {
              nix flake new $1 -t my#"$1"
      }
      bindkey -M emacs '^P' history-substring-search-up
      bindkey -M emacs '^N' history-substring-search-down
      if [[ -r "$\{XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-$\{(%):-%n}.zsh" ]]; then
              source "$\{XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-$\{(%):-%n}.zsh"
      fi
      source ${./p10k.zsh}
    '';
    shellAliases = {
      octal = "stat -c '%a %n'";
      cp = "cp --reflink=auto";
      ec = "TERM=xterm-256color emacsclient -c -nw";
      ns = "nix search nixpkgs";
    };
    shellGlobalAliases = { G = "| grep -i"; };
  };
}
