# nixos-23.11
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/219951b495fc2eac67b1456824cc1ec1fd2ee659.tar.gz") {} }:
  pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [
    cargo
    rustc
    rust-analyzer
    rustfmt
  ];
}
