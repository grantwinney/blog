const GRID_SIZE = 16;

const TILE_EMPTY = 100;
const TILE_MISS = 102;
const TILE_HIT = 103;

/* Information used to draw the ships */
const SHIP = [
  [[1, 5], [1, 2, 5], [1, 2, 3, 5], [1, 2, 3, 4, 5]],     // horizontal
  [[6, 10], [6, 7, 10], [6, 7, 8, 10], [6, 7, 8, 9, 10]]  // vertical
];

/* Information used to draw sunk ships */
const DEAD = [
  [[201, 203], [201, 202, 203], [201, 202, 202, 203], [201, 202, 202, 202, 203]],  // horizontal sunk
  [[204, 206], [204, 205, 206], [204, 205, 205, 206], [204, 205, 205, 205, 206]]   // vertical sunk
];

/* Information used to describe ships */
// [name, length, count]
const SHIP_TYPES = [
  ["Minesweeper", 2, 4],
  ["Frigate", 3, 4],
  ["Cruiser", 4, 2],
  ["Battleship", 5, 1]
];

class BattleshipGame {
  constructor() {
    this.playerGrid = [];
    this.computerGrid = [];
    this.playerShips = [];
    this.computerShips = [];
    this.playerLives = 0;
    this.computerLives = 0;
    this.playing = true;

    this.computerGridEl = document.getElementById("computer-grid");
    this.playerGridEl = document.getElementById("player-grid");
    this.statusEl = document.getElementById("status");
    this.msgEl = document.getElementById("message");

    this.preloaded = [];
    this.init();
  }

  async init() {
    this.playerGrid = this.setupPlayer(false);
    this.computerGrid = this.setupPlayer(true);
    this.buildGrids();
    this.updateStatus();
  }

  createEmptyGrid() {
    const grid = [];
    for (let y = 0; y < GRID_SIZE; y++) {
      const row = [];
      for (let x = 0; x < GRID_SIZE; x++) {
        // [tileId, shipIndex, deadTileId]
        row.push([TILE_EMPTY, -1, 0]);
      }
      grid.push(row);
    }
    return grid;
  }

  /* Function to place the ships in the grid */
  setupPlayer(isComputer) {
    const grid = this.createEmptyGrid();
    let shipNo = 0;

    for (let s = SHIP_TYPES.length - 1; s >= 0; s--) {
      const [name, length, count] = SHIP_TYPES[s];
      for (let i = 0; i < count; i++) {
        const dir = Math.floor(Math.random() * 2); // 0 horizontal, 1 vertical
        let lx = GRID_SIZE;
        let ly = GRID_SIZE;
        let dx = 0;
        let dy = 0;

        if (dir === 0) {
          lx = GRID_SIZE - length;
          dx = 1;
        } else {
          ly = GRID_SIZE - length;
          dy = 1;
        }

        let x, y, ok;
        do {
          y = Math.floor(Math.random() * ly);
          x = Math.floor(Math.random() * lx);
          ok = true;
          let cx = x;
          let cy = y;
          for (let j = 0; j < length; j++) {
            if (grid[cy][cx][0] < 100) {
              ok = false;
              break;
            }
            cx += dx;
            cy += dy;
          }
        } while (!ok);

        let cx = x;
        let cy = y;
        for (let j = 0; j < length; j++) {
          grid[cy][cx][0] = SHIP[dir][s][j];
          grid[cy][cx][1] = shipNo;
          grid[cy][cx][2] = DEAD[dir][s][j];
          cx += dx;
          cy += dy;
        }

        if (isComputer) {
          this.computerShips[shipNo] = [s, length];
          this.computerLives++;
        } else {
          this.playerShips[shipNo] = [s, length];
          this.playerLives++;
        }
        shipNo++;
      }
    }
    return grid;
  }

  buildGrids() {
    this.computerGridEl.innerHTML = "";
    this.playerGridEl.innerHTML = "";

    for (let y = 0; y < GRID_SIZE; y++) {
      for (let x = 0; x < GRID_SIZE; x++) {
        // Computer cell (clickable)
        const cCell = document.createElement("img");
        cCell.className = "cell";
        cCell.dataset.y = y;
        cCell.dataset.x = x;
        cCell.src = `batt${TILE_EMPTY}.gif`;
        cCell.addEventListener("click", () => this.onComputerCellClick(y, x));
        this.computerGridEl.appendChild(cCell);

        // Player cell (shows ships)
        const pCell = document.createElement("img");
        pCell.className = "cell player";
        pCell.dataset.y = y;
        pCell.dataset.x = x;
        pCell.src = `batt${this.playerGrid[y][x][0]}.gif`;
        this.playerGridEl.appendChild(pCell);
      }
    }
  }

  /* Function to change an image shown on a grid */
  setImage(grid, y, x, id, isComputer) {
    grid[y][x][0] = id;
    const selector = isComputer ? "#computer-grid" : "#player-grid";
    const gridEl = isComputer ? this.computerGridEl : this.playerGridEl;
    const index = y * GRID_SIZE + x;
    const img = gridEl.children[index];
    img.src = `batt${id}.gif`;
  }

  onComputerCellClick(y, x) {
    if (!this.playing) return;

    const cell = this.computerGrid[y][x][0];

    if (cell < 100) {
      // hit
      this.setImage(this.computerGrid, y, x, TILE_HIT, true);
      const shipNo = this.computerGrid[y][x][1];
      if (--this.computerShips[shipNo][1] === 0) {
        this.sinkShip(this.computerGrid, shipNo, true);
        this.alertUser(`You sank my ${SHIP_TYPES[this.computerShips[shipNo][0]][0]}!`);
        this.updateStatus();
        if (--this.computerLives === 0) {
          this.alertUser("You win! Press \"New Game\" to play again.");
          this.playing = false;
          return;
        }
      }
      if (this.playing) this.computerMove();
    } else if (cell === TILE_EMPTY) {
      // miss
      this.setImage(this.computerGrid, y, x, TILE_MISS, true);
      this.computerMove();
    }
  }

  /* Function to make the computers move. Note that the computer does not cheat, oh no! */
  computerMove() {
    let sx, sy;
    let selected = false;

    /* Make two passes during 'shoot to kill' mode */
    for (let pass = 0; pass < 2 && !selected; pass++) {
      for (let y = 0; y < GRID_SIZE && !selected; y++) {
        for (let x = 0; x < GRID_SIZE && !selected; x++) {
          if (this.playerGrid[y][x][0] === TILE_HIT) {
            sx = x;
            sy = y;

            const nup = (y > 0 && this.playerGrid[y - 1][x][0] <= 100);
            const ndn = (y < GRID_SIZE - 1 && this.playerGrid[y + 1][x][0] <= 100);
            const nlt = (x > 0 && this.playerGrid[y][x - 1][0] <= 100);
            const nrt = (x < GRID_SIZE - 1 && this.playerGrid[y][x + 1][0] <= 100);

            if (pass === 0) {
              const yup = (y > 0 && this.playerGrid[y - 1][x][0] === TILE_HIT);
              const ydn = (y < GRID_SIZE - 1 && this.playerGrid[y + 1][x][0] === TILE_HIT);
              const ylt = (x > 0 && this.playerGrid[y][x - 1][0] === TILE_HIT);
              const yrt = (x < GRID_SIZE - 1 && this.playerGrid[y][x + 1][0] === TILE_HIT);

              if (nlt && yrt) { sx = x - 1; selected = true; }
              else if (nrt && ylt) { sx = x + 1; selected = true; }
              else if (nup && ydn) { sy = y - 1; selected = true; }
              else if (ndn && yup) { sy = y + 1; selected = true; }
            } else {
              if (nlt) { sx = x - 1; selected = true; }
              else if (nrt) { sx = x + 1; selected = true; }
              else if (nup) { sy = y - 1; selected = true; }
              else if (ndn) { sy = y + 1; selected = true; }
            }
          }
        }
      }
    }

    // random potshot if nothing selected
    if (!selected) {
      do {
        sy = Math.floor(Math.random() * GRID_SIZE);
        sx = Math.floor(Math.random() * (GRID_SIZE / 2)) * 2 + (sy % 2);
      } while (this.playerGrid[sy][sx][0] > 100);
    }

    const cell = this.playerGrid[sy][sx][0];
    if (cell < 100) {
      // hit
      this.setImage(this.playerGrid, sy, sx, TILE_HIT, false);
      const shipNo = this.playerGrid[sy][sx][1];
      if (--this.playerShips[shipNo][1] === 0) {
        this.sinkShip(this.playerGrid, shipNo, false);
        this.alertUser(`I sank your ${SHIP_TYPES[this.playerShips[shipNo][0]][0]}!`);
        if (--this.playerLives === 0) {
          this.knowYourEnemy();
          this.alertUser("I win! Press \"New Game\" to play again.");
          this.playing = false;
        }
      }
    } else {
      // miss
      this.setImage(this.playerGrid, sy, sx, TILE_MISS, false);
    }
  }

  /* When whole ship is hit, show it using changed graphics */
  sinkShip(grid, shipNo, isComputer) {
    for (let y = 0; y < GRID_SIZE; y++) {
      for (let x = 0; x < GRID_SIZE; x++) {
        if (grid[y][x][1] === shipNo) {
          const deadId = grid[y][x][2];
          this.setImage(grid, y, x, deadId, isComputer);
        }
      }
    }
  }

  /* Show location of all the computers ships - when player has lost */
  knowYourEnemy() {
    for (let y = 0; y < GRID_SIZE; y++) {
      for (let x = 0; x < GRID_SIZE; x++) {
        const cell = this.computerGrid[y][x][0];
        if (cell === TILE_HIT) {
          this.setImage(this.computerGrid, y, x, this.computerGrid[y][x][2], true);
        } else if (cell < 100) {
          this.setImage(this.computerGrid, y, x, cell, true);
        }
      }
    }
  }

  /* Show how many ships computer has left */
  updateStatus() {
    let s = "Computer has ";
    let first = true;
    for (let i = 0; i < this.computerShips.length; i++) {
      if (this.computerShips[i][1] > 0) {
        if (!first) s += ", ";
        first = false;
        s += SHIP_TYPES[this.computerShips[i][0]][0];
      }
    }
    if (first) s += "nothing left, thanks to you!";
    this.statusEl.textContent = s;
  }

  alertUser(msg) {
    this.msgEl.textContent = msg;
  }
}

function startNewGame() {
  new BattleshipGame();
}

window.addEventListener("DOMContentLoaded", () => {
  document.getElementById('newgame').onclick = startNewGame;
  startNewGame();
});