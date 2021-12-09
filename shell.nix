with (import <nixpkgs-master> { });

let
  ghcPackages = haskellPackages.ghcWithHoogle
    (haskellPackages: with haskellPackages; [
      pandoc
  ]);

in

with pkgs;

runCommand "dummy" {
  buildInputs = [
    ghcPackages
  ];
  shellHook = ''
    export NIX_GHC="${ghcPackages}/bin/ghc"
    export NIX_GHCPKG="${ghcPackages}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${ghcPackages}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
} ""
