:- use_module(library(lists)).
initial_state(Size, Board-triangle) :- Size > 3,
                              stone_char(neutral, Neutral),
                              fill_edge_row(Size, Neutral, EdgeRow),
                              fill_row(Size, Row),
                              fill_board(Size, EdgeRow, Row, Board).

fill_edge_row(Size, EdgeChar, Row) :- NewSize is Size-2, 
                                      fill_row(NewSize, MiddleRow),
                                      append(MiddleRow, [[EdgeChar, EdgeChar]], TailRow),
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

move(Board-Player, Row/Col, NextBoard-NextPlayer) :- non_empty_stack(Board, Row/Col),
                                                     format('valid input~n').

turn_change(triangle, circle).
turn_change(circle, triangle).

non_empty_stack(Board, Row/Col) :- BoardRow is Row-1,
                                   BoardCol is Col-1,
                                   nth0(BoardRow, Board, ChosenRow),
                                   nth0(BoardCol, ChosenRow, ChosenStack),
                                   length(ChosenStack, Size),
                                   Size > 0.

valid_moves().

game_over().

value().