const print    = console.log
const solution = document.getElementById("solution").innerHTML
const moves    = document.getElementById("counter")
const size     = 5
const tiles    = document.getElementsByClassName("tile")
const help     = document.getElementById("help-modal")
const victory  = document.getElementById("victory-modal")
const overlay  = document.getElementById("wall")

// --------------------------------------------------------------------------------
// Modals

const dim = () => [...document.getElementsByClassName("dimmable")].map(e => e.style.opacity = 0.3)
const undim = () => [...document.getElementsByClassName("dimmable")].map(e => e.style.opacity = 1)
const openModal = (m) => {
    dim()
    m.style.opacity = 1;
    m.style.pointerEvents = "inherit"
    wall.hidden = false;
}
const closeModal = (m) => {
    undim()
    m.style.opacity = 0;
    m.style.pointerEvents = "none"
    wall.hidden = true;
}
const closeModals = () => {
    closeModal(help)
    closeModal(victory)
}

// --------------------------------------------------------------------------------

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
        case "w"         : break;
        case "d"         : break;
        case "s"         : break;
        case "a"         : break;
        default          : return;
    }
    const t = document.querySelector("main .tile[active=true]")
    if (t != null)
        move(t, e.key)
}
const previousLetter = l => l == "A" ? "Z" : String.fromCharCode(l.charCodeAt(0) - 1);
const nextLetter = l => l == "Z" ? "A" : String.fromCharCode(l.charCodeAt(0) + 1);
const chLetter = (l, dir) => {
    const c =
        { "ArrowUp"   : previousLetter,
          "ArrowRight": nextLetter,
          "ArrowDown" : nextLetter,
          "ArrowLeft" : previousLetter,
          "w": previousLetter,
          "d": nextLetter,
          "s": nextLetter,
          "a": previousLetter,
        }[dir]
    return c(l)
}
const checkWin = () => {
    for (let i = size*(size - 1); i<size*size; i++) {
        if (tiles[i].getAttribute("correct") != "true")
            return
    }
    // Win!
    [...document.getElementsByClassName("last-row-tile")].map(e => {
        e.classList.remove("winanim") // We want to restart the animation if someone moves after the solution.
        void e.offsetWidth // Trigger reflow: necessary to restart animation if class was already set.
        e.classList.add("winanim")
    })
    setTimeout(() => openModal(victory), 3500 /* delay to finish winanim animation */)
}
const countMove = () => {
    moves.innerHTML = Number(moves.innerHTML) + 1
}
const move = (el, dir) => {
    const ix = Number(el.getAttribute("ix"))
    const col_ix = ix % size
    const tgt_ix =
      { "ArrowUp"   : ix - size >= 0 ? ix - size : null,
        "w"         : ix - size >= 0 ? ix - size : null,

        "ArrowRight": col_ix + 1 < size ? ix + 1 : null,
        "d"         : col_ix + 1 < size ? ix + 1 : null,

        "ArrowDown" : ix + size < size*size ? ix + size : null,
        "s"         : ix + size < size*size ? ix + size : null,

        "ArrowLeft" : col_ix - 1 >= 0 ? ix - 1 : null,
        "a"         : col_ix - 1 >= 0 ? ix - 1 : null,
      }[dir]
    if (tgt_ix != null && tiles[tgt_ix].getAttribute("hole") == "true" && tiles[ix].getAttribute("hole") != "true") {
        countMove()
        // Update board
        tiles[tgt_ix].innerHTML = chLetter(tiles[ix].innerHTML, dir)
        tiles[ix].innerHTML = "_"
        tiles[tgt_ix].setAttribute("hole", false)
        tiles[ix].setAttribute("hole", true)
        // Set the target as the active piece
        setActive(tiles[tgt_ix])
        // Highlight green right letters
        const tgt_col_ix = tgt_ix % size
        if (tiles[tgt_ix].innerHTML == solution[tgt_col_ix] && tgt_ix >= size*(size-1)) {
            tiles[tgt_ix].setAttribute("correct", true)
            checkWin()
        }
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

// Modal
document.getElementById("help").addEventListener("click", () => openModal(help));
wall.addEventListener("click", e => closeModals());
[...document.getElementsByClassName("close-modal-btn")].map(e => e.addEventListener("click", () => closeModals()));
document.addEventListener("keydown", e => { /* event listeners stack */
  if (e.key === "Escape" && (help.style.opacity == "1" || victory.style.opacity == "1")) {
    closeModals();
  }
})
