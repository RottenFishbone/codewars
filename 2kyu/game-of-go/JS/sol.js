const State = Object.freeze({
    Empty: '.',
    White: 'o',
    Black: 'x',

    opposite: function (state) {
        switch (state) {
        case State.Black:
            return State.White;
        case State.White:
            return State.Black;
        default:
            return State.Empty;
        }
    },
});

const Nbr = Object.freeze({
    Up:     Object.freeze({x:  0, y: -1}),
    Down:   Object.freeze({x:  0, y:  1}),
    Left:   Object.freeze({x: -1, y:  0}),
    Right:  Object.freeze({x:  1, y:  0}),
});

class Vec2 {
    constructor(x, y){
        this.x = x;
        this.y = y;
    }

    toString() {
        return `(${this.x}, ${this.y})`;
    }
}

class Move {
    constructor(moveStr) {
        this.str = moveStr.toUpperCase();
        if (this.str.length == 2 || this.str.length == 3) {
            this.y = parseInt(this.str.substring(0, this.str.length - 1)) - 1;
            this.x = this.str.charCodeAt(this.str.length - 1) - 65;
            if (this.x > 8) {
                --this.x;
            } else if (this.x == 8) {
                throw new Error("I is an invalid column.");
            }
        }
    }
}

class Go {
    constructor(h, w = null) {
        if (w === null) w = h;
        if (h < 0 || h > 25 || w < 0 || w > 25)
            throw new Error("Invalid board size");

        this._size = new Vec2(w, h);
        this._board = new Board(this._size);
        this._curPly = State.Black;
        this._history = [];
    }

    get turn() {
        return this._curPly === State.Black ? 'black' : 'white';
    }

    get board() {
        return this._board.cells;
    }

    get size() {
        return { height: this._size.y, width: this._size.x };
    }

    toString() {
        return this._board.toString();
    }

    pass() {
        this._curPly = State.opposite(this._curPly);
        this._history.push({
            board: structuredClone(this._board),
            move: null,
        });
    }

    reset() {
        this._history = [];
        this._board = new Board(this._size);
        this._curPly = State.Black;
    }

    getPosition(move){
        return this._board.getPosition(move);
    }

    move(...moveStrs) {
        moveStrs.forEach(str => {
            let move = new Move(str);

            this._board.move(move, this._curPly);
            this._curPly = State.opposite(this._curPly);
            
            this._history.push({
                board: this._board.clone(),
                move: move,
            });

            // Invalidate Simple Ko
            if (this._history.length > 2) {
                let last = this._history[this._history.length - 3];
                if (last.move !== null && last.board.eq(this._board)){
                    this.rollback(1);
                    throw new Error('Simple Ko detected');
                }
            }
            
        })
    }


    handicapStones(handicap) {
        if (this._history.length !== 0)
            throw new Error("Cannot add handicap after game started.");
        
        if (this._size.x !== this._size.y)
            throw new Error("Can only add handicaps to square boards.");

        if (!this._board.eq(new Board(this._size)))
            throw new Error("Cannot add handicap more than once.");

        let pattern;
        switch (this._size.x) {
        case 9:
            pattern = [[6,2],[2,6],[6,6],[2,2],[4,4]];
            break;
        case 13:
            pattern = [[9,3],[3,9],[9,9],[3,3],[6,6],[3,6],[9,6],[6,3],[6,9]];
            break;
        case 19:
            pattern = [[15,3],[3,15],[15,15],[3,3],[9,9],[3,9],[15,9],[9,3],[9,15]];
            break;
        default:
            throw new Error("Handicap only available on size 9, 13 and 19.");
        }
        
        if (handicap > pattern.length)
            throw new Error("Too many handicap stones");

        for (let i = 0; i < handicap; ++i) {
            let pos = new Vec2(
                pattern[i][0],
                pattern[i][1],
            );
            this._board._setCell(pos, State.Black);
        }
    }


    rollback(count) {
        if (count > this._history.length) 
            throw new Error(`Cannot roll back: ${count} turns`);
        
        if (count == this._history.length) {
            this.reset();
            return;
        }

        while (count-- > 0) {
            this._history.pop();
            this._curPly = State.opposite(this._curPly);
        }
        this._board = this._history[this._history.length-1].board.clone();
    }
}

class Board {
    constructor(sizeVec) {
        this.size = sizeVec;
        this.cells = Array(sizeVec.y)
            .fill()
            .map(() => Array(sizeVec.x)
                .fill()
                .map(() => State.Empty)
            );
    }

    toString() {
        let out = '';
        this.cells.forEach(row => {
            row.forEach(cell => {
                out += `${cell} `;
            });
            out += '\n';
        });

        return out;
    }

    eq(board) {
        if (board.size != this.size) return false;

        return this.cells.every((row, i) =>
            row.every((cell, j) => 
                cell === board.cells[i][j])
        )
    }

    clone() {
        let clone = new Board(this.size);
        clone.cells = structuredClone(this.cells);
        return clone;
    }

    move(move, state) {
        if (state === State.Empty)
            throw new Error('Empty cannot play');

        let pos = this._moveToVec(move);
        if (this._getCell(pos) !== State.Empty) 
            throw new Error('Cannot play atop another stone.');

        this._setCell(pos, state);
        let nbrGroups = this._findNbrGroups(pos)
            .filter(grp => grp.liberties === 0);
 
        // Capture possible neighbours
        let backup;
        if (nbrGroups.length > 0){
            backup = structuredClone(this.cells);
        }
        nbrGroups
            .forEach(grp => {
                grp.cells.forEach(cellPos => {
                    this._setCell(cellPos, State.Empty);
                });
            });
        
        // Prevent Suicide
        let selfGroup = this._findGroup(pos);
        if (selfGroup.liberties === 0) {
            this._setCell(pos, State.Empty);
            if (backup) this.cells = backup;
            throw new Error('Suicide is illegal');
        }
    }

    getPosition(moveStr){
        let pos = this._moveToVec(new Move(moveStr));
        return this._getCell(pos);
    }

    _getCell(pos) {
        if (!this._inBounds(pos)) 
            throw new Error('Cannot getCell: pos out of bounds.');
        return this.cells[pos.y][pos.x];
    }

    _setCell(pos, state){
        if (!this._inBounds(pos))
            throw new Error('Cannot setCell: pos out of bounds');
        this.cells[pos.y][pos.x] = state;
    }

    _inBounds(pos) {
        return pos.x >= 0 && pos.x < this.size.x &&
            pos.y >= 0 && pos.y < this.size.y;
    }
    
    _moveToVec(move) {
        let vec = new Vec2(
            move.x,
            (this.size.y-1) - move.y,
        );

        if (!this._inBounds(vec))
            throw new Error('Move not in bounds.');
        
        return vec;
    }

    _findNbrGroups(pos){
        let posState = this.cells[pos.y][pos.x];
        
        let nbrVisited = new Set();
        let nbrGroups = [];
        for (let k in Nbr){
            let offset = Nbr[k];
            let nbrPos = new Vec2(pos.x + offset.x, pos.y + offset.y);
            if (!this._inBounds(nbrPos)) continue;

            let nbr = this._getCell(nbrPos);
            if (nbr === State.opposite(posState) && 
                !nbrVisited.has(nbrPos.toString())) {
                
                let nbrGroup = this._findGroup(nbrPos);
                nbrGroups.push(nbrGroup);
            
                nbrGroup.cells.forEach(cell => {
                    nbrVisited.add(cell.toString());
                });
            }
        }

        return nbrGroups;
    }

    _findGroup(pos) {
        let group = [];
        let state = this._getCell(pos);
        if (state == null || state == State.Empty) { return group; }
        let liberties = new Set();
        let visited = new Set();
        visited.add(new Vec2(pos.x, pos.y).toString());
        
        let stack = [pos];
        while (stack.length > 0){
            let head = stack.pop();
            group.push(head);
            
            for (var k in Nbr){
                let v = Nbr[k];
                let nbrPos = new Vec2(head.x + v.x, head.y + v.y);

                // Avoid cycles
                if (visited.has(nbrPos.toString())) continue;
                visited.add(nbrPos.toString());
                
                // Skip out of bounds
                if (!this._inBounds(nbrPos)) continue;
                let nbr = this._getCell(nbrPos);
                
                // Add matching states to line
                if (nbr === state) {
                    stack.push(nbrPos);
                } else if (nbr === State.Empty){
                    liberties.add(nbrPos.toString());
                }
            }
        }
    
        return { cells: group, liberties: liberties.size };
    }
}

let game = new Go(9);
let moves = ["5C","5G","4D"];
game.move(...moves);
console.log(game.toString())
game.rollback(2);
console.log(game.toString())
game.move("8F");
game.move("5G");
console.log(game.toString());
/*
Move: 5C, x
Move: 5G, o
Move: 4D, x
Roll: 2
Move: 8F, o
Move: 5G, x
*/
