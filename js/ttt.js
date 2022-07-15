const playerX  = Symbol('X');
const playerO  = Symbol('O');
const noPlayer = Symbol(' ');

const constF = a => b => a;
const newBoard = Array.from(Array(9)).map(constF(noPlayer));

const projectToBoard = (x,y) => (x - 1) + (y - 1) * 3;

const putPiece = (board, piece, x, y) => {
  return board.map((p, idx) => {
    if (idx == projectToBoard(x,y)) {
      return piece;
    } else {
      return p;
    }});
};

const prettyPiece = p => {
    if (p === playerX) {
        return "X";
    } else if (p === playerO) {
        return "O";
    } else {
        return " ";
    }};

const prettyBoard = board => {
  return board.reduce((acc,p,idx) => {
    acc += "|";
    acc += prettyPiece(p);
    if (idx % 3 === 2) {
        acc += "|\n";
    }
    return acc;
  }, "");
}

const winner = board => {
  const reduceDiagonal = f => [f(board[projectToBoard(1,1)], board[projectToBoard(2,2)], board[projectToBoard(3,3)]),
                               f(board[projectToBoard(3,1)], board[projectToBoard(2,2)], board[projectToBoard(1,3)])]

  const reduceRow = f => Array.from(Array(3)).map((_, idx) =>
    f(board[projectToBoard(1,idx+1)], board[projectToBoard(2,idx+1)], board[projectToBoard(3,idx+1)]));

  const reduceColumn = f => Array.from(Array(3)).map((_, idx) =>
    f(board[projectToBoard(idx+1,1)], board[projectToBoard(idx+1,2)], board[projectToBoard(idx+1,3)]));

  const win = (a,b,c) => {
    if (a === noPlayer) {
      return null;
    }
    if (a === b && b === c) {
      return a;
    }
    return null;
  };

  return reduceDiagonal(win)
         .concat(reduceRow(win))
         .concat(reduceColumn(win))
         .reduce((acc, w) => {
                if (w) {
                    return w;
                } else {
                    return acc;
                }
         }, null);
}

const hasEmpty = board => board.find(p => p === noPlayer);

const isEmptyAt = (board, x, y) => board[projectToBoard(x,y)] === noPlayer;

const readPosition = p => {
  const r = prompt("It is player " + prettyPiece(p) + "'s turn.").split(',');
  if (r.length < 2
      || !parseInt(r[0])
      || !parseInt(r[1])
      || parseInt(r[0]) < 1
      || parseInt(r[1]) < 1
      || parseInt(r[0]) > 3
      || parseInt(r[1]) > 3
     ) {
    console.log("I did not understand this.")
    return readPosition(p);
  }

  return [ parseInt(r[0]), parseInt(r[1]) ];
};

const main = () => {
  let b = newBoard;
  let p = playerX;

  while(!winner(b) && hasEmpty(b)) {
    console.log(prettyBoard(b));

    let pos = null;
    do {
      [x,y] = readPosition(p);
    } while(!isEmptyAt(b, x, y))

    b = putPiece(b, p, x ,y);
    p = p === playerX ? playerO : playerX;
  }

  console.log(prettyBoard(b));
  let a = winner(b);
  if (a) {
    console.log(prettyPiece(a) + " has won!");
  } else {
    console.log("it's a draw!");
  }
}
main();
