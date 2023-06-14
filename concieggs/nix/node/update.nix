# To update the Nix sources after the dependencies in package.json have changed, run:
#
#   nix-shell update.nix --run 'npm install --package-lock-only && node2nix -18 -l'
let
  sources = import ../sources.nix;
  pkgs = import sources.nixpkgs {};
in
with pkgs;
mkShell {
  buildInputs = [
    nodejs_18
    node2nix
  ];
}
