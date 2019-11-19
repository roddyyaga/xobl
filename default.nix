{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation rec {
  name = "aaaaaaa";

  buildInputs = [ clang m4 binutils pkgconfig gmp autoconf libseccomp ];

  shellHook = ''
    eval $(opam env)
  '';
}

