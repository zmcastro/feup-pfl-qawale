:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

% opposites(?CharCode, ?OppositeCharCode)
% For a given character code representing a cardinal direction, return the character code of its opposite.
opposites(110, 115).
opposites(115, 110).
opposites(101, 119).
opposites(119, 101).

% initial_state(+Size, -GameState)
% Initialize a board of size Size with one stack of 2 neutral pieces in each corner. 
% The Player in the GameState is bound to triangle, which corresponds to the piece of the first player to move.
initial_state(Size, Board-triangle) :- Size > 3,
                                       piece_char(neutral, Neutral),
                                       fill_edge_row(Size, Neutral, EdgeRow),
                                       fill_row(Size, Row),
                                       fill_board(Size, EdgeRow, Row, Board).

% fill_edge_row(+Size, +EdgeChar, -Row)
% Fill the first/last row: those have a stack of pieces represented by EdgeChar on each end.
fill_edge_row(Size, EdgeChar, Row) :- NewSize is Size-2, 
                                      fill_row(NewSize, MiddleRow),
                                      append(MiddleRow, [[EdgeChar, EdgeChar]], TailRow),
                                      append([[EdgeChar, EdgeChar]], TailRow, Row).

% fill_row(+Size, -Row)
% Fill a row with empty stacks.
fill_row(0, []).
fill_row(Size, [[] | T]) :- Size > 0,
                            NewSize is Size-1,
                            fill_row(NewSize, T).

% fill_board(+Size, +EdgeRow, +Row, -Board)
% Fills a board with the previously generated rows.
fill_board(Size, EdgeRow, Row, Board) :- NewSize is Size-2,
                                         fill_middle_board(NewSize, Row, MiddleBoard),
                                         append(MiddleBoard, [EdgeRow], TailBoard),
                                         append([EdgeRow], TailBoard, Board).

% fill_middle_board(+Size, +Row, -MiddleBoard)
% Fill the board except the first and last rows.
fill_middle_board(0, _, []).
fill_middle_board(Size, Row, [Row | T]) :- Size > 0,
                                           NewSize is Size-1,
                                           fill_middle_board(NewSize, Row, T).

%
% Movement
%

% move(+GameState, +Placement, -NewGameState)
% Place a piece on the given Placement coordinates.
move(Board-Player, Placement, NewBoard-Player) :- allowed_move(Board, Placement),
                                                  place_piece(Board, Player, Placement, NewBoard).

% move(+GameState, +Move, +Placement, -NewGameState)
% Move the stack in Placement, according to the Move string.
move(Board-Player, Move, Row/Col, NewBoard-NextPlayer) :- allowed_move(Board, Move, MoveList),
                                                          get_stack(Board, Row/Col, ChosenStack),
                                                          get_row(Board, Row, ChosenRow),
                                                          generate_board(Board, Row/Col, ChosenRow, [], MidBoard),
                                                          turn_change(Player, NextPlayer),
                                                          move_stack(MidBoard, MoveList, Row/Col, ChosenStack, BoardList),
                                                          last(BoardList, NewBoard).

% allowed_move(+Board, +Placement)
% Verify if the Placement is allowed, which means checking if the stack is not empty.
allowed_move(Board, Placement) :- non_empty_stack(Board, Placement).

% allowed_move(+Board, +Move, -MoveList)
% Verify if the Move is allowed, which means checking for backtracking, and return the list of character codes of the movement string.
allowed_move(Board, Move, MoveList) :- atom_codes(Move, MoveList),
                                       no_backtracking(PrevMove, MoveList).

% non_empty_stack(+Board, +Placement)
% Check if the stack at the Move coordinates is not empty.
non_empty_stack(Board, Row/Col) :- get_stack(Board, Row/Col, ChosenStack),
                                   length(ChosenStack, Size),
                                   Size > 0.

% no_backtracking(+PrevMove, +Move)
% Check if the movement string does not include backtracking (moving to the previous space immediatly after leaving it).
no_backtracking(_, []).
no_backtracking(PrevMove, [MoveHead | T]) :- (
                                                var(PrevMove);
                                                \+opposites(MoveHead, PrevMove)
                                             ),
                                            NewPrevMove is MoveHead, 
                                            no_backtracking(NewPrevMove, T).

% place_piece(+Board, +Player, +Placement, -NewBoard)
% Place a piece from Player in the stack at the Placement coordinates.
place_piece(Board, Player, Row/Col, NewBoard) :- get_stack(Board, Row/Col, ChosenStack),
                                                get_row(Board, Row, ChosenRow),
                                                (
                                                    piece_char(Player, Piece), append([Piece], ChosenStack, NewStack);
                                                    append([Player], ChosenStack, NewStack)
                                                ), !,
                                                generate_board(Board, Row/Col, ChosenRow, NewStack, NewBoard).

% move_stack(+Board, +Move, +Placement, +ChosenStack, -NewBoardList)
% Move the stack according to the movement string, returning a list of Boards, which stores the moves step-by-step.
move_stack(Board, [], Row/Col, [], [Board | NT]).
move_stack(Board, [MoveHead | MT], Row/Col, ChosenStack, [NewBoard | NT]) :- last(ChosenStack, LastChar),
                                                                            new_coordinates(Row/Col, MoveHead, NewRow/NewCol),
                                                                            place_piece(Board, LastChar, NewRow/NewCol, NewBoard),
                                                                            pop_back(ChosenStack, PoppedStack),
                                                                            move_stack(NewBoard, MT, NewRow/NewCol, PoppedStack, NT).
% new_coordinates(+Placement, +MoveHead, -NewPlacement)
% Convert the coordinates to new ones after applying a movement step, making sure they are valid.
new_coordinates(Row/Col, MoveHead, NewRow/NewCol) :- valid_code(MoveHead),
                                                     char_code(MoveChar, MoveHead),
                                                     calculate_coords(Row/Col, MoveChar, NewRow/NewCol),
                                                     valid_coords(NewRow/NewCol).


% calculate_coords(+Coordinates, +MoveChar, -NewCoordinates)
% Calculate the new coordinates after applying a movement step.
calculate_coords(Row/Col, 'n', NewRow/NewCol) :- NewRow is Row-1, NewCol is Col.
calculate_coords(Row/Col, 's', NewRow/NewCol) :- NewRow is Row+1, NewCol is Col.
calculate_coords(Row/Col, 'e', NewRow/NewCol) :- NewRow is Row, NewCol is Col+1.
calculate_coords(Row/Col, 'w', NewRow/NewCol) :- NewRow is Row, NewCol is Col-1.

% valid_code(MoveHead)
% Verify if the character code of the movement step is valid.
valid_code(MoveHead) :- (
                          MoveHead = 110;
                          MoveHead = 115;
                          MoveHead = 101;
                          MoveHead = 119
                        ).

% valid_coords(+NewCoordinates)
% Verify if the new coordinates are valid.
valid_coords(NewRow/NewCol) :- size(Size),
                               NewRow > 0, NewRow =< Size,
                               NewCol > 0, NewCol =< Size.

% generate_board(+Board, +Coordinates, +ChosenRow, +NewStack, -NewBoard)
% Refresh the board after applying a movement.
generate_board(Board, Row/Col, ChosenRow, NewStack, NewBoard) :- replace(ChosenRow, Col, NewStack, NewRow),
                                                                 replace(Board, Row, NewRow, NewBoard).

% get_stack(+GameState, +Coordinates, -Stack)
% get_stack(+Board, +Coordinates, -Stack)
% Get the stack at Coordinates.
get_stack(Board-Player, Row/Col, ChosenStack) :- get_row(Board, Row, ChosenRow),
                                                 nth1(Col, ChosenRow, ChosenStack).
get_stack(Board, Row/Col, ChosenStack) :- get_row(Board, Row, ChosenRow),
                                          nth1(Col, ChosenRow, ChosenStack).

% get_row(+Board, +RowCoordinate, -ChosenRow)
% Get the row of stacks at the Row coordinate.
get_row(Board, RowCoordinate, Row) :- nth1(RowCoordinate, Board, Row).

% turn_change(?Player, ?NewPlayer)
% Get the player that will play the next turn.
turn_change(triangle, circle).
turn_change(circle, triangle).

% next_player_type(+PlayerType, -NewPlayerType)
% Get the player type that will play the next turn.
next_player_type(PlayerType, NewPlayerType) :- gamemode(PlayerType/NewPlayerType); gamemode(NewPlayerType/PlayerType).


%
% Computer
%

% valid_moves(+GameState, -Moves)
% Given the current GameState, generate a list of all possible piece placements. Specifying the Player is irrelevant, since the placement possibilities 
% at the start of each turn are equal for any player.
valid_moves(GameState, Moves) :- findall(Move, move(GameState, Move, NewGameState), Moves).

% valid_moves(+GameState, +Placement, -Moves)
% Given the current GameState and previous Placement of the piece, generate a list of all possible stack movements.
valid_moves(Board-Player, Row/Col, Moves) :-  get_stack(Board, Row/Col, ChosenStack),
                                              findall(Move, move_stack(Board, Move, Row/Col, ChosenStack, NewGameState), AllMoves),
                                              include(no_backtracking(PrevMove), AllMoves, CodeMoves),
                                              convert_to_atom(CodeMoves, Moves).

% value(+GameState, +Placement, -Value)
% Attribute a value to the Board after placing the piece.
% This is based on the amount of Player pieces in the chosen stack, overriden if the move causes a victory for the player.
value(Board-Player, Row/Col, Value) :- ( 
                                        game_over(Board-Player, Winner, 1), Winner = Player, Value = 99
                                        ;
                                        get_stack(Board, Row/Col, Stack),
                                        piece_char(Player, PlayerChar),
                                        count(PlayerChar, Stack, Value), !
                                        ).

% value(+GameState, -Value)
% Attribute a value to the Board after moving a stack.
% This is based on the number of Player pieces and opponent pieces on top of each stack, overriden if the move causes a victory for the player.
value(Board-Player, Value) :- (
                                game_over(Board-Player, Winner, 1), Winner = Player, Value = 99
                                ; 
                                top_piece_list(Board, NestedLists),
                                append(NestedLists, List), % append/2 can convert a list of lists into a list
                                piece_char(Player, PlayerChar),
                                count(PlayerChar, List, PlayerValue),
                                turn_change(Player, Opponent),
                                piece_char(Opponent, OpponentChar),
                                count(OpponentChar, List, OpponentValue),
                                Value is PlayerValue-OpponentValue
                              ).

% value_moves(+GameState, +Placement, +MoveList, -ValuedMoveList)
% Attribute a value to each possible move (corresponding to the value of the board after executing the move).
value_moves(Board-Player, _/_, [], []).
value_moves(Board-Player, Row/Col, [Move | T], [Value-Move | T2]) :- move(Board-Player, Move, Row/Col, NewBoard-_),
                                                                     value(NewBoard-Player, Value),
                                                                     value_moves(Board-Player, Row/Col, T, T2).

% highest_value(+BestValue, +ValuedMove)
% Check if the valued move has the specified value (used to check for the best value).
highest_value(BestValue, Value-Move) :- Value = BestValue. 

%
% Game Over
%

% game_over(+GameState, -Winner, +TurnsLeft)
% Verifies the victory of any player or if the turn count has reached 0 (ending in a draw).
game_over(Board-Player, Winner, TurnsLeft) :- (
                                                four_in_line(Board, Winner);
                                                no_turns_left(TurnsLeft, Winner)
                                              ).

% no_turns_left(+TurnsLeft, ?Result)
% Check if the turn count dropped below 1.
no_turns_left(TurnsLeft, draw) :- TurnsLeft < 1.

% four_in_line(+Board, ?Player)
% Verify every row, column and diagonal for a player victory (4 player pieces in line).
four_in_line(Board, triangle) :- piece_char(triangle, Char),
                                 (
                                  row_check(Board, Char); 
                                  column_check(Board, Char); 
                                  diagonal_check(Board, Char)
                                 ).
four_in_line(Board, circle) :- piece_char(circle, Char),
                               (
                                row_check(Board, Char); 
                                column_check(Board, Char); 
                                diagonal_check(Board, Char)
                               ).

% row_check(+Board, +Winner)
% Check each row for a line of player pieces.
row_check(Board, Winner) :- member([[Winner | _], [Winner | _], [Winner | _], [Winner | _]], Board).

% column_check(+Board, +Winner)
% Check each column for a line of player pieces.
column_check(Board, Winner) :- transpose(Board, TransBoard), row_check(TransBoard, Winner).

% diagonal_check(+Board, +Winner)
% Check each diagonal for a line of player pieces.
diagonal_check(Board, Winner) :- (
                                    Board = [[[Winner | _], _, _, _],
                                            [_, [Winner | _], _, _],
                                            [_, _, [Winner | _], _],
                                            [_, _, _, [Winner | _]]]
                                ;
                                    Board = [[_, _, _, [Winner | _]],
                                            [_, _, [Winner | _], _],
                                            [_, [Winner | _], _, _],
                                            [[Winner | _], _, _, _]]
                                ).

%
% Utils
% 

% replace(+List, +Index, +Sub, -NewList)
% Replace an element in a list at the specified index, starting at 1.
replace([_|T], 1, Sub, [Sub|T]).
replace([Head|T], Index, Sub, [Head|T2]):- Index > 1, NewIndex is Index-1, replace(T, NewIndex, Sub, T2).

% pop_back(+List, -NewList)
% Remove the last element of a list.
pop_back([_], []).
pop_back([H|T], [H|T2]) :- pop_back(T, T2).

% count(+Element, +List, -N)
% Count the number of times Element appears in List.
count(_, [], 0).
count(Elem, [ Elem | T ], N) :- count(Elem, T, N1),
                                N is N1 + 1.
count(Elem, [ _ | T ], N) :- count(Elem, T, N).

% top_piece_list(+Board, +TopPieces)
% Return a list of rows, each containing the top piece of each of their stacks.
top_piece_list([], []).
top_piece_list([Row | T], [List | T2]) :- row_piece_list(Row, List), top_piece_list(T, T2).

% row_piece_list(+Row, +TopRow)
% Return a row with only the top piece of each stack.
row_piece_list([], []).
row_piece_list([[] | T], [' ' | T2]) :- row_piece_list(T, T2).
row_piece_list([[Piece | Pieces] | T], [Piece | T2]) :- row_piece_list(T, T2).

% convert_to_atom(+CodeList, -AtomList)
% Convert a list of character codes to their atom representation.
convert_to_atom([], []).
convert_to_atom([Code | T], [Atom | T2]) :- atom_codes(Atom, Code), convert_to_atom(T, T2).
