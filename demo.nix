#with import <nixpkgs> {};
#let
#  file = runCommand "dynamic.nix" {} ''
#    exec 1>$out
#    i=1
#
#    echo "{"
#    for x in a b c d e; do
#      echo "  $x = $i;"
#      i=$((i+1))
#    done
#    echo "}"
#  '';
#  evaluated = import file;
#
#in
#  evaluated.c

with import <nixpkgs> {};
let
  file = writeText "demo.nix" ''
    {
      a = 1;
      b = 2;
      c = 3;
      d = 4;
      e = 5;
    }
  '';
  evaluated = import file;

in
  evaluated.c

