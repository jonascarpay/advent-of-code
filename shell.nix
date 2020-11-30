let
  hsPkgs = import ./.;
  pkgs = import <nixpkgs> { };
in
hsPkgs.shellFor {
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.2";
    ghcid = "0.8.7";
    ghcide = "0.2.0";
    # ormolu = "0.1.4.1";
    fourmolu = "0.3.0.0";
  };
  buildInputs = [
    (pkgs.writeShellScriptBin "ormolu" ''
      fourmolu --indentation=2 --ghc-opt=-XImportQualifiedPost $@
    '')
  ];
  exactDeps = true;
}
