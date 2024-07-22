#!/bin/sh

# Updates the shuffdle game board and goal word.

# Generate a new game
set -- $(cabal run shuffled 2>&1 | \
    grep -e "target-word:" -e "board-id" | \
    sed "s/target-word://" | sed "s/board-id://")

WORD=$1
BOARD=$2
echo "WORD: $WORD"
echo "BOARD: $BOARD"

sed -i.bkp "s/the_word_goal = .*$/the_word_goal = \"$WORD\"/" index.html
sed -i.bkp "s/the_board_id = .*$/the_board_id = $BOARD/" index.html

