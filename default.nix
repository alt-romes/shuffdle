{ pkgs ? import <nixpkgs> {} }:
let
  hsc = pkgs.haskell.compiler.ghc910;
  hsp = pkgs.haskell.packages.ghc910;
  hs-shuffdle = hsp.callCabal2nix "shuffled" ./. { };
  site-attrs = {
    name = "shuffdle-html";
    src = ./.;

    # Updates the shuffdle game board and goal word.
    buildPhase = ''
      set -e

      # Generate a new game
      # Interpret the result as a shell environment that we source
      . <(${hs-shuffdle}/bin/shuffled)

      echo "WORD: $WORD"
      echo "BOARD: $BOARD"
      echo "HARDBOARD: $HARDBOARD"

      if [ ! -z "$BOARD" ]; then
        sed -i.bkp "s/the_word_goal = .*$/the_word_goal = \"$WORD\"/" index.html
        sed -i.bkp "s/the_board_id = .*$/the_board_id = $BOARD/" index.html
        sed -i.bkp "s/the_hard_board_id = .*$/the_hard_board_id = $HARDBOARD/" index.html
      fi
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
in
{
  exe = hs-shuffdle;
  inherit site-attrs;
  site = pkgs.stdenv.mkDerivation site-attrs;
}
