:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

opposites(110, 115).
opposites(115, 110).
opposites(101, 119).
opposites(119, 101).

initial_state(Size, Board-triangle) :- Size > 3,
                              stone_char(neutral, Neutral),
                              fill_edge_row(Size, Neutral, EdgeRow),
                              fill_row(Size, Row),
                              fill_board(Size, EdgeRow, Row, Board).

fill_edge_row(Size, EdgeChar, Row) :- NewSize is Size-2, 
                                      fill_row(NewSize, MiddleRow),
                                      append(MiddleRow, [['\x25B2\', '\x25B2\']], TailRow),
                                      append([[EdgeChar, EdgeChar]], TailRow, Row).

fill_row(0, []).
fill_row(Size, [[] | T]) :- Size > 0,
                            NewSize is Size-1,
                            fill_row(NewSize, T).


fill_board(Size, EdgeRow, Row, Board) :- NewSize is Size-2,
                                         fill_middle_board(NewSize, Row, MiddleBoard),
                                         append(MiddleBoard, [EdgeRow], TailBoard),
                                         append([EdgeRow], TailBoard, Board).

fill_middle_board(0, _, []).
fill_middle_board(Size, Row, [Row | T]) :- Size > 0,
                                           NewSize is Size-1,
                                           fill_middle_board(NewSize, Row, T).

%
% Movement
%

move(Board-Player, Row/Col, piece, NewBoard-Player) :- allowed_move(Board, Row, Col, piece),
                                                        place_piece(Board, Player, Row/Col, NewBoard).
move(Board-Player, Move, Row/Col, stack, NewBoard-NextPlayer) :- allowed_move(Board, Move, MoveList, stack),
                                                                 get_stack(Board, Row/Col, ChosenStack),
                                                                 get_row(Board, Row, ChosenRow),
                                                                 generate_board(Board, Row/Col, ChosenRow, [], MidBoard),
                                                                 turn_change(Player, NextPlayer),
                                                                 move_stack(MidBoard, MoveList, Row/Col, ChosenStack, BoardList),
                                                                 last(BoardList, NewBoard).

allowed_move(Board, Row, Col, piece) :- non_empty_stack(Board, Row/Col).
allowed_move(Board, Move, MoveList, stack) :- atom_codes(Move, MoveList),
                             no_backtracking(PrevMove, MoveList).

generate_board(Board, Row/Col, ChosenRow, NewStack, NewBoard) :- replace(ChosenRow, Col, NewStack, NewRow),
                                                      replace(Board, Row, NewRow, NewBoard).

no_backtracking(_, []).
no_backtracking(PrevMove, [MoveHead | T]) :- (
                                                var(PrevMove);
                                                \+opposites(MoveHead, PrevMove)
                                             ),
                                            NewPrevMove is MoveHead, 
                                            no_backtracking(NewPrevMove, T).

move_stack(Board, [], Row/Col, [], [Board | NT]).

move_stack(Board, [MoveHead | MT], Row/Col, ChosenStack, [NewBoard | NT]) :- last(ChosenStack, LastChar),
                                                                            new_coordinates(Row/Col, MoveHead, NewRow/NewCol),
                                                                            place_piece(Board, LastChar, NewRow/NewCol, NewBoard),
                                                                            pop_back(ChosenStack, PoppedStack),
                                                                            move_stack(NewBoard, MT, NewRow/NewCol, PoppedStack, NT).

new_coordinates(Row/Col, MoveHead, NewRow/NewCol) :- (
                                                      MoveHead = 110;
                                                      MoveHead = 115;
                                                      MoveHead = 101;
                                                      MoveHead = 119
                                                     ),
                                                     char_code(MoveChar, MoveHead),
                                                     calculate_coords(Row/Col, MoveChar, NewRow/NewCol),
                                                     valid_coords(NewRow/NewCol).

calculate_coords(Row/Col, 'n', NewRow/NewCol) :- NewRow is Row-1,
                                                 NewCol is Col.
calculate_coords(Row/Col, 's', NewRow/NewCol) :- NewRow is Row+1,
                                                 NewCol is Col.
calculate_coords(Row/Col, 'e', NewRow/NewCol) :- NewRow is Row,
                                                 NewCol is Col+1.
calculate_coords(Row/Col, 'w', NewRow/NewCol) :- NewRow is Row,
                                                 NewCol is Col-1.

valid_coords(NewRow/NewCol) :- size(Size),
                               NewRow > 0, NewRow =< Size,
                               NewCol > 0, NewCol =< Size.
                                                      

get_stack(Board, Row/Col, ChosenStack) :- nth1(Row, Board, ChosenRow),
                                          nth1(Col, ChosenRow, ChosenStack).

get_row(Board, Row, ChosenRow) :- nth1(Row, Board, ChosenRow).

place_piece(Board, Player, Row/Col, NewBoard) :- get_stack(Board, Row/Col, ChosenStack),
                                                get_row(Board, Row, ChosenRow),
                                                (
                                                    stone_char(Player, Piece), append([Piece], ChosenStack, NewStack);
                                                    append([Player], ChosenStack, NewStack)
                                                ), !,
                                                generate_board(Board, Row/Col, ChosenRow, NewStack, NewBoard).

turn_change(triangle, circle).
turn_change(circle, triangle).

next_player_type(PlayerType, NewPlayerType) :- gamemode(PlayerType/NewPlayerType); gamemode(NewPlayerType/PlayerType).

non_empty_stack(Board, Row/Col) :- get_stack(Board, Row/Col, ChosenStack),
                                   length(ChosenStack, Size),
                                   Size > 0.

reduce_pieces :- total_pieces(Pieces), NewPieces is Pieces-1, retract(total_pieces(_)), assertz(total_pieces(NewPieces)).

valid_moves(GameState, Moves) :- findall(Move, move(GameState, Move, piece, NewGameState), Moves).

valid_moves(Board-Player, Moves, Row/Col) :-  get_stack(Board, Row/Col, ChosenStack),
                                              findall(Move, move_stack(Board, Move, Row/Col, ChosenStack, NewGameState), AllMoves),
                                              include(no_backtracking(PrevMove), AllMoves, CodeMoves),
                                              convert_to_atom(CodeMoves, Moves).

convert_to_atom([], []).
convert_to_atom([Move | T], [AtomMove | T2]) :- atom_codes(AtomMove, Move), convert_to_atom(T, T2).

% AI

value(Board-Player, Row/Col, Value) :- get_stack(Board, Row/Col, Stack),
                                       stone_char(Player, PlayerChar),
                                       count(PlayerChar, Stack, Value), !.
value(Board-Player, Value) :- (
                                game_over(Board-Player, _, 1), Value = 99
                              ; top_piece_list(Board, NestedLists),
                                append(NestedLists, List),
                                stone_char(Player, PlayerChar),
                                count(PlayerChar, List, PlayerValue),
                                turn_change(Player, Opponent),
                                stone_char(Opponent, OpponentChar),
                                count(OpponentChar, List, OpponentValue),
                                Value is PlayerValue-OpponentValue
                              ).

value(Board-Player, Value) :- (
                                game_over(Board-Player, _, 1), Value = 99
                                ; 
                                top_piece_list(Board, NestedLists),
                                append(NestedLists, List),
                                stone_char(Player, PlayerChar),
                                count(PlayerChar, List, PlayerValue),
                                turn_change(Player, Opponent),
                                stone_char(Opponent, OpponentChar),
                                count(OpponentChar, List, OpponentValue),
                                Value is PlayerValue-OpponentValue
                              ).

game_over(Board-Player, Winner, TurnsLeft) :- (
                                                four_in_line(Board, Winner);
                                                no_turns_left(TurnsLeft, Winner)
                                              ).

no_turns_left(TurnsLeft, draw) :- TurnsLeft < 1.

four_in_line(Board, triangle) :- stone_char(triangle, Char),
                                 (
                                  row_check(Board, Char); 
                                  column_check(Board, Char); 
                                  diagonal_check(Board, Char)
                                 ).
four_in_line(Board, circle) :- stone_char(circle, Char),
                               (
                                row_check(Board, Char); 
                                column_check(Board, Char); 
                                diagonal_check(Board, Char)
                               ).

row_check(Board, Winner) :- member([[Winner | _], [Winner | _], [Winner | _], [Winner | _]], Board).

column_check(Board, Winner) :- transpose(Board, TransBoard), row_check(TransBoard, Winner).

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

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 1, I1 is I-1, replace(T, I1, X, R).

pop_back([_], []).
pop_back([H|T], [H|T2]) :- pop_back(T, T2).


% utils 

count(_, [], 0).
count(Elem, [ Elem | T ], N) :- count(Elem, T, N1),
                                N is N1 + 1.
count(Elem, [ _ | T ], N) :- count(Elem, T, N).

top_piece_list([], []).
top_piece_list([Row | T], [List | T2]) :- row_piece_list(Row, List), top_piece_list(T, T2).

row_piece_list([], []).
row_piece_list([[] | T], [' ' | T2]) :- row_piece_list(T, T2).
row_piece_list([[Piece | Pieces] | T], [Piece | T2]) :- row_piece_list(T, T2).
