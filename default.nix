{ pkgs ? import <nixpkgs> {} }:
let
  hsc = pkgs.haskell.compiler.ghc910;
  hsp = pkgs.haskell.packages.ghc910;
  hs-shuffdle = hsp.callCabal2nix "shuffled" ./. { };
in
{
  inherit hs-shuffdle;
  site = pkgs.stdenv.mkDerivation {
    name = "shuffdle-html";
    src = ./.;

    buildPhase = ''
      set -e

      # Updates the shuffdle game board and goal word.

      # Generate a new game
      set -- $(${hs-shuffdle}/bin/shuffled 2>&1 | \
          grep -e "target-word:" -e "board-id" -e "hard-board-id" -e "solved" | \
          sed "s/target-word://" | sed "s/hard-board-id://" | sed "s/board-id://")

      WORD=$1
      BOARD=$2
      HARDBOARD=$3
      echo "WORD: $WORD"
      echo "BOARD: $BOARD"
      echo "HARDBOARD: $HARDBOARD"

      sed -i.bkp "s/the_word_goal = .*$/the_word_goal = \"$WORD\"/" index.html
      sed -i.bkp "s/the_board_id = .*$/the_board_id = $BOARD/" index.html
      sed -i.bkp "s/the_hard_board_id = .*$/the_hard_board_id = $HARDBOARD/" index.html

    '';

    installPhase = ''
      mkdir -p $out

      # Main
      cp main.v2.js $out
      cp style.v2.css $out
      cp index.html $out
      
      # Favicon
      cp *.png $out
      cp favicon.ico $out
      cp site.webmanifest $out
    '';
  };
}
