{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "shell";

  buildInputs = [
    sbcl
    ccl
    ecl
    cmucl_binary
    clisp
    mkcl
    rlwrap
  ];

  LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
    openssl
    mesa_noglu
    SDL2
    SDL2_image
    SDL2_ttf
    libffi
  ];

  shellHook = ''
    export PS1="\[\033[1;32m\][$name \w]\n>\[\033[0m\] "
    function rl () { rlwrap -s 1000000 -c -b "(){}[].,=&^%0\;|" $@; }
  '';
}
