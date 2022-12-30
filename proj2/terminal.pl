encoding(utf8).

:- dynamic gamemode/1, size/1.
gamemode(h/h).
size(4).

get_chars(border-top, ['\x256D\', '\x2500\', '\x256E\']).
get_chars(border-bottom, ['\x2570\', '\x2500\', '\x256F\']).
get_chars(border-sep, ['\x2502\', '|']).
get_chars(board-stones, ['\x25B2\', '\x25A0\', '\x25CF\']).
get_chars(board-empty, [' ']).

stone_char(triangle, '\x25B2\').
stone_char(circle, '\x25CF\').
stone_char(neutral, '\x25A0\').

display_game(Board-Player) :- size(Size),
                              display_board(Board, Size),
                              display_stack_handler(Board, Size).
%
% Board Display
%

display_board(Board, Size) :- display_column_coordinates(0, Size),
                              display_border(Size, top),
                              display_lines(Board, 1),
                              display_border(Size, bottom).

% NEED TO REFACTOR DISPLAY OF COORDINATES

display_column_coordinates(Size, Size) :- nl.
display_column_coordinates(Index, Size) :- NewIndex is Index + 1,
                                           (
                                           NewIndex = 1, format('   ~w', [NewIndex]);
                                           format(' ~w', [NewIndex])
                                           ),
                                           display_column_coordinates(NewIndex, Size).

display_row_coordinates(Index) :- format('~w ', [Index]).

display_border(Size, Side) :- get_chars(border-Side, [LeftEdge, Middle, RightEdge]), 
                              format('  ~w', [LeftEdge]), display_straight_border(Size, Middle), write(RightEdge), nl.

display_straight_border(1, Char):- write(Char).
display_straight_border(Size, Char) :- write(Char), write(Char), NewSize is Size-1, display_straight_border(NewSize, Char).

display_lines([], _).
display_lines([Line | T], Index) :- get_chars(border-sep, [SideEdge, Sep]),
                             display_row_coordinates(Index),
                             NewIndex is Index+1,
                             write(SideEdge), 
                             display_board_line(Line, Sep), 
                             write(SideEdge), 
                             nl,
                             display_lines(T, NewIndex).

display_board_line([[]], Sep) :- get_chars(board-empty, [Char]), 
                                 write(Char).
display_board_line([[] | T], Sep) :- get_chars(board-empty, [Char]), 
                                     write(Char),
                                     write(Sep),
                                     display_board_line(T, Sep).
display_board_line([[Char | Chars]], Sep) :- write(Char).                           
display_board_line([[Char | Chars] | T], Sep) :- write(Char),
                                                 write(Sep),
                                                 display_board_line(T, Sep).

%
% Stack Display
%

display_stack_handler(Board, Size) :- format('Stacks in play: (Organized by Row/Column, Left = Stack Top, Right = Stack Bottom)~n~n'),
                                      display_stack_list(Board, 0, Size).

display_stack_list(_, Size, Size).
display_stack_list([Row | T], Index, Size) :-    RowIndex is Index + 1,
                                                 display_row_stacks(Row, RowIndex, 0, Size), 
                                                 display_stack_list(T, RowIndex, Size).

display_row_stacks(_, _, Size, Size) :- format('~n~n').
display_row_stacks([Stack | T], RowIndex, ColumnIndex, Size) :- NewIndex is ColumnIndex + 1,
                                                                atomics_to_string(Stack, '', String),
                                                                format('~w/~w = [~w] ', [RowIndex, NewIndex, String]),
                                                                display_row_stacks(T, RowIndex, NewIndex, Size).

%
% Other Menu Options
%                          

get_boardsize :- write('What board size do you want? We recommend 4, but choose any integer above 3.'), nl,
                 read(Size),
                 integer(Size), Size > 3,
                 retract(size(_)), assertz(size(Size)),
                 !.

get_boardsize :- error_message.

error_message :- write('An error ocurred while parsing your input. Returning to main menu.'), nl.