% Utility function to check if a list contains a certain element
contains(List, Elem) :- member(Elem, List).

% Utility function to get the element at a specific index in a list
getElem(List, Index, Elem) :- nth0(Index, List, Elem).

% Utility function to replace an element at a specific index in a list
replaceElem(List, Index, Elem, Result) :-
    nth0(Index, List, _, Temp),
    nth0(Index, Result, Elem, Temp).

% Check if the player has won
checkWin(Board, Player) :-
    checkRows(Board, Player),
    checkColumns(Board, Player),
    checkDiagonals(Board, Player).

% Check rows for a win
checkRows(Board, Player) :-
    Board = [Row1, Row2, Row3, Row4, Row5, Row6],
    % Horizontal win
    (member([Player, Player, Player, Player, _, _, _], Board)).

% Check columns for a win
checkColumns(Board, Player) :-
    transpose(Board, Transposed),
    checkRows(Transposed, Player).

% Check diagonals for a win
checkDiagonals(Board, Player) :-
    % Check main diagonals
    checkMainDiagonal(Board, Player),
    checkSecondaryDiagonal(Board, Player).

% Check main diagonal for a win
checkMainDiagonal(Board, Player) :-
    append(_, [Diagonal | _], Board),
    append(_, [Player, Player, Player, Player | _], Diagonal).

% Check secondary diagonal for a win
checkSecondaryDiagonal(Board, Player) :-
    reverse(Board, Reversed),
    checkMainDiagonal(Reversed, Player).

% Check if the board is full
boardFull(Board) :- \+ contains(Board, [_, _, _, _, _, _, _]).

% Evaluate the utility of the board for a given player
evaluate(Board, Player, Score) :-
    checkWin(Board, Player),
    Score is 100.

evaluate(Board, Player, Score) :-
    playerSwitch(Player, Opponent),
    checkWin(Board, Opponent),
    Score is -100.

evaluate(_, _, Score) :-
    Score is 0.

% Get the opponent of a player
playerSwitch('X', 'O').
playerSwitch('O', 'X').

% Generate all possible moves on the board
generateMoves(Board, Moves) :-
    findall(Column, getElem(Board, Column, [_, _, _, _, _, _, _]), Moves).

% Make a move on the board
makeMove(Board, Column, Player, Result) :-
    validMove(Column),
    getElem(Board, Column, ColumnList),
    contains(ColumnList, '_'),
    replaceElem(ColumnList, Index, Player, NewColumnList),
    replaceElem(Board, Column, NewColumnList, Result),
    count('_', ColumnList, Count),
    Index is 5 - Count.

% Valid move check
validMove(Column) :- between(0, 6, Column).

% Count the occurrences of an element in a list
count(_, [], 0).

count(Elem, [Elem | T], N) :-
    count(Elem, T, N1),
    N is N1 + 1.

count(Elem, [_ | T], N) :-
    count(Elem, T, N).

% Minimax algorithm with alpha-beta pruning
minimax(Board, Depth, Alpha, Beta, Player, Move, Score) :-
    Depth = 0,
    evaluate(Board, Player, Score).

minimax(Board, Depth, Alpha, Beta, Player, Move, Score) :-
    Depth > 0,
    playerSwitch(Player, Opponent),
    generateMoves(Board, Moves),
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    NextDepth is Depth - 1,
    minimaxMove(Board, Moves, NextDepth, Alpha1, Beta1, Opponent, nil, -1000000, MoveOut, ScoreOut),
    Score is -ScoreOut,
    Move = MoveOut.

minimaxMove(_, [], _, _, Alpha, _, BestMove, BestScore, BestMove, BestScore).

minimaxMove(Board, [Move | Moves], Depth, Alpha, Beta, Player, BestMove, BestScore, MoveOut, ScoreOut) :-
    makeMove(Board, Move, Player, NewBoard),
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    minimax(NewBoard, Depth, Alpha1, Beta1, Player, _, Score),
    updateBestMoveScore(Move, Score, BestMove, BestScore, NewBestMove, NewBestScore),
    updateAlphaBeta(NewBestScore, Alpha, Beta, NewAlpha, NewBeta),
    (
        NewAlpha >= NewBeta ->
        MoveOut = NewBestMove,
        ScoreOut = NewBestScore
        ;
        minimaxMove(Board, Moves, Depth, NewAlpha, NewBeta, Player, NewBestMove, NewBestScore, MoveOut, ScoreOut)
    ).

updateBestMoveScore(_, Score, nil, _, _, Score).

updateBestMoveScore(Move, Score, BestMove, BestScore, Move, Score) :-
    Score > BestScore.

updateBestMoveScore(_, Score, BestMove, BestScore, BestMove, BestScore) :-
    Score =< BestScore.

updateAlphaBeta(Score, Alpha, Beta, Score, Beta) :-
    Score >= Beta.

updateAlphaBeta(Score, Alpha, Beta, Alpha, Score) :-
    Score > Alpha.

% Agent's move
agentMove(Board, Move) :-
    minimax(Board, 5, -1000000, 1000000, 'X', Move, _).

% Human's move
humanMove(Board, Move) :-
    repeat,
    writeln("Enter your move (0-6): "),
    read(Move),
    validMove(Move),
    getElem(Board, Move, Column),
    contains(Column, '_'),
    !.

% Display the board
displayBoard(Board) :-
    writeln(" 0 1 2 3 4 5 6"),
    writeln("----------------"),
    displayRows(Board).

displayRows([]).

displayRows([Row | Rows]) :-
    writeln(Row),
    displayRows(Rows).
% Game loop
playGame(Board, Player) :-
    displayBoard(Board),
    (
        Player = 'X' ->
        agentMove(Board, Move),
        makeMove(Board, Move, Player, NewBoard),
        (
            checkWin(NewBoard, Player) ->
            displayBoard(NewBoard),
            format("Congratulations! ~w wins!~n", [Player])
            ;
            (
                boardFull(NewBoard) ->
                displayBoard(NewBoard),
                writeln("It's a draw!")
                ;
                playerSwitch(Player, NextPlayer),
                playGame(NewBoard, NextPlayer)
            )
        )
        ;
        humanMove(Board, Move),
        makeMove(Board, Move, Player, NewBoard),
        (
            checkWin(NewBoard, Player) ->
            displayBoard(NewBoard),
            format("Congratulations! ~w wins!~n", [Player])
            ;
            (
                boardFull(NewBoard) ->
                displayBoard(NewBoard),
                writeln("It's a draw!")
                ;
                playerSwitch(Player, NextPlayer),
                playGame(NewBoard, NextPlayer)
            )
        )
    ).
% Start the game
startGame :-
    Board = [
        ['_', '_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_', '_'],
        ['_', '_', '_', '_', '_', '_', '_']
    ],
    playGame(Board, 'X').
