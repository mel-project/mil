{
  description = "Mel Intermediate Lisp";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
  inputs.mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; };
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { self
    , nixpkgs
    , mozilla
    , flake-utils
    , ...
    } @inputs:
    let rustOverlay = final: prev:
          let rustChannel = prev.rustChannelOf {
            channel = "1.49.0";
            sha256 = "sha256-KCh2UBGtdlBJ/4UOqZlxUtcyefv7MH1neoVNV4z0nWs=";
          };
          in
          { inherit rustChannel;
            rustc = rustChannel.rust;
            cargo = rustChannel.rust;
          };
    in flake-utils.lib.eachDefaultSystem
      (system:
        let
        inherit system;
        pkgs = import nixpkgs {
          overlays = [
            (import "${mozilla}/rust-overlay.nix")
            rustOverlay
          ];
        };
        in {
          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              clang
              openssl
              (rustChannel.rust.override { extensions = [ "rust-src" ]; })
            ];
          };
        });
}
