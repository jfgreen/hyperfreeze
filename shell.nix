# nixos-24.11
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/3f0a8ac25fb674611b98089ca3a5dd6480175751.tar.gz") {} }:
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
