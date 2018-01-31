{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "lisp-shell";
  buildInputs = [ bash coreutils sbcl rlwrap openssl git nix-repl ];

  LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
    pkgs.openssl
  ];
}
