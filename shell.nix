{ pkgs ? import <nixpkgs> {} }:
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
