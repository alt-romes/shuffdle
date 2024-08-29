const print    = console.log
const solution = document.getElementById("solution").innerHTML
const moves    = document.getElementById("moves")
const counter  = document.getElementById("counter")
const restartq = document.getElementById("restart-qm")
const size     = 5
const tiles    = document.getElementsByClassName("tile")
const helpBtn  = document.getElementById("help")
const help     = document.getElementById("help-modal")
const victory  = document.getElementById("victory-modal")
const overlay  = document.getElementById("wall")
const defaultCorrectness = hardcoreMode ? "possibly-correct" : "correct" /* easy mode there's only one correct location */

// --------------------------------------------------------------------------------
// Modals

const dim = () => [...document.getElementsByClassName("dimmable")].map(e => e.style.opacity = 0.3)
const undim = () => [...document.getElementsByClassName("dimmable")].map(e => e.style.opacity = 1)
const openModal = (m) => {
    dim()
    m.style.pointerEvents = "inherit"
    wall.hidden = false;

    if (m.id == "victory-modal")
        /* Sometime right before the victory-modal animation finishes, but
         * after the animation has set the opacity to 1 (1300/2), set more
         * permanently the opacity to 1. */
        setTimeout(() => m.style.opacity = 1, 1000)
    else
        m.style.opacity = 1;
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
    midnight.setHours(23)
    midnight.setMinutes(0)
    midnight.setSeconds(0)
    const diff = new Date(midnight - now)
    document.getElementById("time-left-for-daily").innerHTML =
        diff.getHours() + "h " + diff.getMinutes() + "m"
}
const maxMoves = Number(document.getElementById("maxMoves").innerHTML)
const getMoves = () => Number(moves.innerHTML)
const getHighScore = () => 14
const playAnim = (e, a) => {
    e.classList.remove(a)
    void e.offsetWidth // Trigger reflow
    e.classList.add(a)
}
const reportWin = () => {
    const now = new Date();
    const datePath = now.getDate() + "-" + (now.getMonth()+1) + "-" + now.getFullYear()
    plausible('pageview', { u: "https://www.shuffdle.com/win/" + (hardcoreMode ? "hard/" : "") + solution + "/" + datePath + "/" + getMoves() + "-moves" })

    let data =
            { word: solution,
              mode: hardcoreMode? "Hard" : "Normal",
              moves: getMoves(), time: 0, restarts: 0 }

    // Get winscreen username if visiting again
    const existUser = localStorage.getItem("shuffdle-username")
    if (!!existUser) // User exists
        data["username"] = existUser

    return fetch("https://win.shuffdle.com/integrationm/concurrent/SHUFDLE_create_win", {
        method: "POST",
        headers: {
          "Content-Type": `application/x-www-form-urlencoded`,
        },
        body: new URLSearchParams(data).toString()
    }).then(r => {
        // Always set username (makes sure to update storage even if something invalid during development got written there)
        localStorage.setItem("shuffdle-visited", window.btoa(r.body.results[0]["Username"]))
    });
}
const checkWin = () => {
    /* This function doesn't need to handle the easy case because it will check only all tiles that have the defaultCorrectness
     * In hardest mode, the correctness will be "possibly-correct", and we'll traverse all of those.
     *      "possibly-correct" gets set only on hardest mode.
     * In easy mode, the correctness will be "correct". "correct" will be set
     * only for tiles on the last row when in easy mode.
     *
     * Basically, this will sweep from all tiles we've marked: as long as we
     * mark them appropriately for each difficulty, this is fine.
     *
     * NB: I suppose this would allow the word to be accepted backwards in the
     * last row! That's fine, we've actually never said it couldn't be
     * backwards, just that it had to be in the last row.
     *
     * NB: Actually, let's not allow backwards for the hard mode. Perhaps we
     * can eventually create an even harder which has all the ways, but that
     * seems unplayable.
     */
    const solutions =
            [...document.querySelectorAll("#board-container .tile["+defaultCorrectness+"=\"true\"]")].map(c => {
                const ix = Number(c.getAttribute("ix"))
                const col_ix = ix % size
                const row_ix = ix / size >> 0 // integer division
                let hasWon = false;
                // From the start of the column to the end, all must be correct.
                if (col_ix == 0) {
                    for (let i = 0; i<5; i++)
                        if (tiles[row_ix*5 + i].innerHTML != solution[i])
                            return false
                    for (let i = 0; i<5; i++)
                        tiles[row_ix*5 + i].setAttribute("correct", true)
                    hasWon = true;
                }
                // From the start of the row to the end, all must be correct.
                if (row_ix == 0) {
                    for (let i = 0; i<5; i++)
                        if (tiles[i*5 + col_ix].innerHTML != solution[i])
                            return false
                    for (let i = 0; i<5; i++)
                        tiles[i*5 + col_ix].setAttribute("correct", true)
                    hasWon = true;
                }
                // From the end of the row to the start, all must be correct.
                // if (row_ix == 5) {
                //     for (let i = 0; i<5; i++)
                //         if (tiles[i*5 + col_ix].innerHTML != solution[4-i])
                //             return false
                //     for (let i = 0; i<5; i++)
                //         tiles[i*5 + col_ix].setAttribute("correct", true)
                //     hasWon = true;
                // }
                // // From the end of the column to the start, all must be correct.
                // if (col_ix == 5) {
                //     for (let i = 0; i<5; i++)
                //         if (tiles[row_ix*5 + i].innerHTML != solution[4 - i])
                //             return false
                //     for (let i = 0; i<5; i++)
                //         tiles[row_ix*5 + i].setAttribute("correct", true)
                //     hasWon = true;
                // }
                // Only return at the end because if somehow both a column and
                // row are correct, they should all be marked as correct.

                return hasWon;
            })

    if (!solutions.some(x => x)) {
        // No correct solution
        return;
    }

    // The function above will also mark as "correct" the possibly-correct
    // tiles that were actually correct for a solution.

    // Win!
    showWin()

    // Report win
    reportWin()
}
const showWin = () => {
    [...document.querySelectorAll("main .tile[correct=\"true\"]")].map(e => {
        playAnim(e, "winanim") // (Yes, we want to restart the animation if someone moves after the solution.)
    })
    setTimeout(() => {

        openModal(victory)

        // Bounce in win anim
        playAnim(victory, "win-modal-anim")

    }, 3500 /* delay until winanim animation finishes */)


    /* Populate win screen */
    const myMoves = getMoves()
    document.getElementById("victory-count").innerHTML = myMoves;
    if (hardcoreMode)
        document.getElementById("victory-line").innerHTML = "You've completed today's <strong>hardest</strong> puzzle in";
    
    const now = new Date();
    const secondsToNextMinute = 60 - now.getSeconds()
    updateTimeToMidnight()
    setTimeout(() => {
        updateTimeToMidnight(),
        setInterval(updateTimeToMidnight, 60*1000 /* repeat every minute */)},
    secondsToNextMinute * 1000);

}
const flashMaxReached = () => {
    playAnim(counter, "gelatine")
}
const countMove = () => {
    moves.innerHTML = getMoves() + 1
}
const isTileCorrect = t => {
    const ix = Number(t.getAttribute("ix"))
    const col_ix = ix % size
    const row_ix = ix / size >> 0 // integer division
    const tval = t.innerHTML

    /*
     * Note: Here we must be careful:
     * If on easy mode, we can only allow solutions in the last row.
     * Otherwise we can mark all possibly correct letters
     */
    if (hardcoreMode)
        return tval == solution[col_ix] || tval == solution[row_ix]
                // No longer do backwards words for hard.
                // || tval == solution[4 - col_ix] || tval == solution[4 - row_ix]
    else
        return row_ix == 4 && tval == solution[col_ix]
}
const markAllPCs = () => {
    // Called at start to mark existing possibly correct letters
    [...tiles].forEach(t => {
        if (isTileCorrect(t)) {
            t.setAttribute(defaultCorrectness, true)
        }
    })
}
const move = (orig, dir) => {
    if (getMoves() >= maxMoves) {
        flashMaxReached();
        return;
    }
    const ix = Number(orig.getAttribute("ix"))
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

    if (tgt_ix == null) return;

    const tgt = tiles[tgt_ix]

    if (tgt.getAttribute("hole") == "true" && orig.getAttribute("hole") != "true") {
        countMove()
        // Update board
        tgt.innerHTML = chLetter(orig.innerHTML, dir)
        orig.innerHTML = "_"
        tgt.setAttribute("hole", false)
        orig.setAttribute("hole", true)
        // Set the target as the active piece
        setActive(tgt)

        // Clear existing highlight
        orig.setAttribute("correct", false)
        if (hardcoreMode) {
            orig.setAttribute("possibly-correct", false);
            // On hardcore mode, moving a single piece makes the remaining no longer correct but only possibly-correct.
            [...document.querySelectorAll("#board-container .tile[correct=true]")].forEach(at => {
                at.setAttribute("correct", false)
                at.setAttribute("possibly-correct", true)
            })
        }

        if (isTileCorrect(tgt)) {
            // Highlight correct/possibly-correct right letters
            tgt.setAttribute(defaultCorrectness, true)
            checkWin()
        }
    }
}

for (let i=0; i<tiles.length; i++) {
    const t = tiles[i]
    t.setAttribute("ix", i)
    t.addEventListener("click", e => setActive(e.target))
}
document.addEventListener("keydown", keydown)

// Mark all already possibly correct tiles
markAllPCs()

// Animate "Tutorial" button if visiting for the first time
if (!localStorage.getItem("shuffdle-visited")) {
    helpBtn.classList.add("hithere")
    localStorage.setItem("shuffdle-visited", "yes")
}

// Modal
helpBtn.addEventListener("click", () => openModal(help));
wall.addEventListener("click", e => closeModals());
[...document.getElementsByClassName("close-modal-btn")].map(e => e.addEventListener("click", () => closeModals()));
document.addEventListener("keydown", e => { /* event listeners stack */
  if (e.key === "Escape" && (help.style.opacity == "1" || victory.style.opacity == "1")) {
    closeModals();
  }
});

// Drag and drop / Touch events
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
    var startX, startY

    tile.addEventListener("touchstart", ev => {
        setActive(ev.target)
        const touch = ev.changedTouches[0]
        startX = touch.pageX
        startY = touch.pageY
    });

    tile.addEventListener("touchend", ev => {
        const origin_ix = Number(ev.target.getAttribute("ix"));
        const touchAtEnd = ev.changedTouches[0]
        const vectorAngle = (x, y) =>
          Math.acos(
            x.reduce((acc, n, i) => acc + n * y[i], 0) /
              (Math.hypot(...x) * Math.hypot(...y))
          );

        const swipeVec = [touchAtEnd.pageX - startX, touchAtEnd.pageY - startY]
        const swipeAngle = vectorAngle(swipeVec, [1, 0])

        const pi = Math.PI
        const dir = 
                swipeAngle < pi/4 ? "ArrowRight" :
                swipeAngle > pi/4 && swipeAngle < 3*pi/4 ?
                    (swipeVec[1] < 0 ? "ArrowUp" :
                     swipeVec[1] > 0 ? "ArrowDown" :
                     null) :
                swipeAngle > 3*pi/4 ? "ArrowLeft" :
                null

        if (dir != null) {
            ev.preventDefault();
            move(document.querySelector("#board-container .tile[ix=\""+origin_ix+"\"]"), dir)
        }

    });

});

const share = async () => {
    const data =
            { url: ""+window.location.origin
            , text: "I finished today's" +
                        (hardcoreMode?" hard-mode " : " ") +
                        "Shuffdle puzzle in " + getMoves() + " moves."
            , title: "Shuffdle"
            }
    try {
        if (navigator.canShare && navigator.share && navigator.canShare(data))
            await navigator.share(data).then(() => print("Shared!")).catch(err => console.warn(err))
    } catch (err) {
        console.warn(err);
    }
}

document.getElementById("share").addEventListener("click", share)

/* Show a Hint */
const showABC = () => {
    const h = document.getElementById("hint")
    h.classList.remove("small-option")
    // But keep the properties we want updated from small-option
    h.style.cursor = "default";
    h.style.fontSize = "0.9em";
    h.innerHTML = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    h.onclick = "";
}

const hmt = document.getElementById("hardmode-toggle")
const leadTuto = document.getElementById("lead-tutorial")
if (hardcoreMode) {
    hmt.innerHTML = "Easy Mode"
    hmt.setAttribute("href", "/")
    document.getElementById("header-line").innerHTML += ". Write it <em>anywhere</em>."
    leadTuto.innerHTML = "Reconstruct the daily Shuffdle word <em>anywhere</em> with a minimal number of moves. Welcome to Hard Mode."
}
else {
    hmt.innerHTML = "Hard Mode"
    hmt.setAttribute("href", "/?hard")
    leadTuto.innerHTML = "Reconstruct the daily Shuffdle word in the <em>bottom row</em> with a minimal number of moves"
}

[...document.getElementsByClassName("hard-only")].forEach(el => el.hidden = false);

