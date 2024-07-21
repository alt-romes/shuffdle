const print    = console.log
const solution = document.getElementById("solution").innerHTML
const size     = 5
const tiles    = document.getElementsByClassName("tile")
const setActive = el => {
    [...tiles].map(t => t.setAttribute("active", false))
    el.setAttribute("active", true)
}
const keydown = e => {
    switch (e.key) {
        case "ArrowLeft" : break;
        case "ArrowRight": break;
        case "ArrowUp"   : break;
        case "ArrowDown" : break;
        default          : return;
    }
    move(document.querySelector("[active=true]"), e.key)
}
const previousLetter = l => l == "A" ? "Z" : String.fromCharCode(l.charCodeAt(0) - 1);
const nextLetter = l => l == "Z" ? "A" : String.fromCharCode(l.charCodeAt(0) + 1);
const chLetter = (l, dir) => {
    const c =
        { "ArrowUp": previousLetter,
          "ArrowRight": nextLetter,
          "ArrowDown" : nextLetter,
          "ArrowLeft" : previousLetter
        }[dir]
    return c(l)
}
const checkWin = () => {
    for (let i = size*(size - 1); i<size*size; i++) {
        if (tiles[i].getAttribute("correct") != "true")
            return
    }
    print("WIN!")
}
const move = (el, dir) => {
    const ix = Number(el.getAttribute("ix"))
    const col_ix = ix % size
    const tgt_ix =
      { "ArrowUp"   : ix - size >= 0 ? ix - size : null,
        "ArrowRight": col_ix + 1 < size ? ix + 1 : null,
        "ArrowDown" : ix + size < size*size ? ix + size : null,
        "ArrowLeft" : col_ix - 1 >= 0 ? ix - 1 : null,
      }[dir]
    if (tgt_ix != null && tiles[tgt_ix].getAttribute("hole") == "true" && tiles[ix].getAttribute("hole") != "true") {
        // Update board
        tiles[tgt_ix].innerHTML = chLetter(tiles[ix].innerHTML, dir)
        tiles[ix].innerHTML = "_"
        tiles[tgt_ix].setAttribute("hole", false)
        tiles[ix].setAttribute("hole", true)
        // Set the target as the active piece
        setActive(tiles[tgt_ix])
        // Highlight green right letters
        const tgt_col_ix = tgt_ix % size
        if (tiles[tgt_ix].innerHTML == solution[tgt_col_ix] && tgt_ix >= size*(size-1))
            tiles[tgt_ix].setAttribute("correct", true)
        // Clear existing highlight
        tiles[ix].setAttribute("correct", false)
    }
}

for (let i=0; i<tiles.length; i++) {
    const t = tiles[i]
    t.setAttribute("ix", i)
    t.addEventListener("click", e => setActive(e.target))
}
document.addEventListener("keydown", keydown)

