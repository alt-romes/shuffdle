const print    = console.log
const solution = document.getElementById("solution").innerHTML
const moves    = document.getElementById("moves")
const counter  = document.getElementById("counter")
const restartq = document.getElementById("restart-qm")
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
const updateTimeToMidnight = () => {
    const now = new Date();
    const midnight = new Date(now);
    midnight.setHours(24)
    midnight.setMinutes(1)
    midnight.setSeconds(0)
    const diff = new Date(midnight - now)
    document.getElementById("time-left-for-daily").innerHTML =
        diff.getHours() + "h " + diff.getMinutes() + "m"
}
const maxMoves = Number(document.getElementById("maxMoves").innerHTML)
const getMoves = () => Number(moves.innerHTML)
const getHighScore = () => 14
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
    setTimeout(() => openModal(victory), 3500 /* delay until winanim animation finishes */)

    /* Populate win screen */
    const myMoves = getMoves()
    const highScore = getHighScore()
    document.getElementById("victory-count").innerHTML = myMoves
    document.getElementById("highscore-count").innerHTML = highScore
    
    const now = new Date();
    const secondsToNextMinute = 60 - now.getSeconds()
    updateTimeToMidnight()
    setTimeout(() => {
        updateTimeToMidnight(),
        setInterval(updateTimeToMidnight, 60*1000 /* repeat every minute */)},
    secondsToNextMinute * 1000);

    document.getElementById("custom-victory-msg").innerHTML =
        myMoves > highScore ?
            "It looks like you could still do it in fewer moves!" :
        myMoves == highScore ?
            "You've done as well as the best solution so far. Is it possible to do better?" :
        /* otherwise */
            "You've just set a <em>new highscore</em> for minimum number of moves!"

    const shareVictory = document.getElementById("share-victory")

    const updateShareText = () => {
        shareVictory.innerHTML = "<small style=\"color:var(--gray)\">Copied to clipboard</small>"
        setTimeout(() => {
            shareVictory.innerHTML = "Share"
        }, 2000)
    }
    const sharePhrase = "Shuffdle " + now.getDate() + "/" + now.getMonth() + " " + solution + " in " + myMoves + "/50 moves"
    const writeToClipboard = () => shareVictory.innerHTML = sharePhrase

    shareVictory.addEventListener("click", writeToClipboard)
}
const flashMaxReached = () => {
    counter.classList.remove("gelatine")
    void counter.offsetWidth // Trigger reflow
    counter.classList.add("gelatine")

    restartq.hidden = false;
    void restartq.offsetWidth;
    restartq.style.opacity = 1;
}
const countMove = () => {
    moves.innerHTML = getMoves() + 1
}
const move = (el, dir) => {
    if (getMoves() >= maxMoves) {
        flashMaxReached();
        return;
    }
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
    if (tgt_ix != null && tiles[tgt_ix].getAttribute("hole") == "true" &&
            tiles[ix].getAttribute("hole") != "true") {
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
});

// Drag and drop
const validDropDirection = (orig, tgt) => {
    /* no need to check out of bounds since only possible for drag targets */
    if (orig + 1 == tgt)
        return "ArrowRight"
    else if (orig - 1 == tgt)
        return "ArrowLeft"
    else if (orig + 5 == tgt)
        return "ArrowDown"
    else if (orig - 5 == tgt)
        return "ArrowUp"
    else
        return null
}
[...document.querySelectorAll("#board-container .tile")].forEach(tile => {
    tile.draggable = true;
    tile.addEventListener("dragstart", ev => {
        ev.dataTransfer.effectAllowed = "move"
        ev.dataTransfer.setData("text/plain", ev.target.getAttribute("ix"))
        setActive(ev.target)
    });

    tile.addEventListener("dragover", ev => {
        ev.preventDefault();
    });

    tile.addEventListener("drop", ev => {
        ev.dataTransfer.dropEffect = "move"
        const origin_ix = Number(ev.dataTransfer.getData("text/plain"));
        const target_ix = Number(ev.target.getAttribute("ix"));
        const dir = validDropDirection(origin_ix, target_ix)
        if (dir != null && ev.target.getAttribute("hole") == "true") {
            ev.preventDefault();
            ev.dataTransfer.dropEffect = "move"
            move(document.querySelector("#board-container .tile[ix=\""+origin_ix+"\"]"), dir)
        }
    });

    /* Touch Events */
    tile.addEventListener("touchstart", ev => {
        setActive(ev.target)
    });

    tile.addEventListener("touchend", ev => {
        const origin_ix = Number(ev.target.getAttribute("ix"));
        const touchAtEnd = ev.changedTouches[0]
        const targetEl = document.elementFromPoint(touchAtEnd.pageX, touchAtEnd.pageY)
        const target_ix = Number(targetEl.getAttribute("ix"))
        const dir = validDropDirection(origin_ix, target_ix)
        if (dir != null && targetEl.getAttribute("hole") == "true") {
            ev.preventDefault();
            move(document.querySelector("#board-container .tile[ix=\""+origin_ix+"\"]"), dir)
        }
    });

});
