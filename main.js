const print    = console.log
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
    }
}

for (let i=0; i<tiles.length; i++) {
    const t = tiles[i]
    t.setAttribute("ix", i)
    t.addEventListener("click", e => setActive(e.target))
}
document.addEventListener("keydown", keydown)
