#!/bin/sh

set -e

# Updates the shuffdle game board and goal word.

# Generate a new game
set -- $(cabal run shuffled 2>&1 | \
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

