{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
# let nixpkgs_source = nixpkgs.fetchFromGitHub {
#       owner = "NixOS";
#       repo = "nixpkgs";
#       rev = "c3aa1090413feda4778a95991a46fd566be76fb4";
#       sha256 = "0x26bkaxxbx2rlp72ls2b2c4kswkxgqgx1myl5ld7wchs85kwz3h";
#     };
#     nixpkgs' = (import nixpkgs_source){};
# in with nixpkgs'.pkgs;
with nixpkgs;
let hp = haskell.packages.${compiler}.override{
    overrides = self: super: {
      weather = self.callPackage ./default.nix {};
      };};
     getHaskellDeps = ps: path:
        let f = import path;
            gatherDeps = {buildDepends ? [], libraryHaskellDepends ? [], executableHaskellDepends ? [], ...}:
               libraryHaskellDepends ++ executableHaskellDepends;
            x = f (builtins.intersectAttrs (builtins.functionArgs f) ps // {stdenv = stdenv; mkDerivation = gatherDeps;});
        in x;
ghc = hp.ghcWithPackages (ps: with ps; stdenv.lib.lists.subtractLists
[weather]
([
  hdevtools
  ghc-mod
  ]  ++ getHaskellDeps ps ./default.nix));
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
