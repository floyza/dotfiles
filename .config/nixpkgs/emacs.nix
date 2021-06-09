/*
This is a nix expression to build Emacs and some Emacs packages I like
from source on any distribution where Nix is installed. This will install
all the dependencies from the nixpkgs repository and build the binary files
without interfering with the host distribution.

To build the project, type the following from the current directory:

$ nix-build emacs.nix

To run the newly compiled executable:

$ ./result/bin/emacs
*/
{ pkgs ? import <nixpkgs> {} }:



let
  myEmacs = pkgs.emacs;


  emacsWithPackages = (pkgs.emacsPackagesGen myEmacs).emacsWithPackages;


in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    magit          # ; Integrate git <C-x g>
  ]) ++ (with epkgs.melpaPackages; [
  ]) ++ (with epkgs.elpaPackages; [
  ]) ++ [
  ])
