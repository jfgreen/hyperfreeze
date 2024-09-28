# nixos-24.05
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/752c634c09ceb50c45e751f8791cb45cb3d46c9e.tar.gz") {} }:
  pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [
    cargo
    clippy
    rustc
    rust-analyzer
    rustfmt
    static-web-server
  ];
}
