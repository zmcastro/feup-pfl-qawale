encoding(utf8).

%
% Constants and variables
%

:- dynamic gamemode/1, size/1.
gamemode(h/h).
size(4).

total_pieces(16).

%
% ASCII Character Dictionaries
%

% get_chars(?BoardPosition, ?CharArray)
% Depending on the entity to be rendered, return an array of characters to be displayed in the terminal.
get_chars(border-top, ['\x256D\', '\x2500\', '\x256E\']).
get_chars(border-bottom, ['\x2570\', '\x2500\', '\x256F\']).
get_chars(border-sep, ['\x2502\', '|']).
get_chars(board-stones, ['\x25B2\', '\x25A0\', '\x25CF\']).
get_chars(board-empty, [' ']).

stone_char(triangle, '\x25B2\').
stone_char(circle, '\x25CF\').
stone_char(neutral, '\x25A0\').

%
% Game Display
%

% display_game(+GameState)
% Display the board and the stack of pieces in each position.
display_game(Board-Player) :- size(Size),
                              display_board(Board, Size),
                              display_stack_handler(Board, Size), !.

% display_board(+Board, +Size)
% Draw the board and the coordinates of each row/column.
display_board(Board, Size) :- display_column_coordinates(0, Size),
                              display_border(Size, top),
                              display_lines(Board, 1),
                              display_border(Size, bottom).

% NEED TO REFACTOR DISPLAY OF COORDINATES
% display_column_coordinates(+Index, +Size)
% Display the coordinates of each column.
display_column_coordinates(Size, Size) :- nl.
display_column_coordinates(Index, Size) :- NewIndex is Index + 1,
                                           (
                                           NewIndex = 1, format('   ~w', [NewIndex]);
                                           format(' ~w', [NewIndex])
                                           ),
                                           display_column_coordinates(NewIndex, Size).

% display_row_coordinates(+Index)
% Display the coordinate of the current row.
display_row_coordinates(Index) :- format('~w ', [Index]).

% display_border(+Size, +Side)
% Display the correct border according to the side given (top or bottom).
display_border(Size, Side) :- get_chars(border-Side, [LeftEdge, Middle, RightEdge]), 
                              format('  ~w', [LeftEdge]), display_straight_border(Size, Middle), write(RightEdge), nl.

% display_straight_border(+Size, +Char)
% Display the straight line common to the top and bottom borders.
display_straight_border(1, Char):- write(Char).
display_straight_border(Size, Char) :- write(Char), write(Char), NewSize is Size-1, display_straight_border(NewSize, Char).

% display_lines(+Lines, +Index)
% Display each row of the board, including coordinates and border.
display_lines([], _).
display_lines([Line | T], Index) :- get_chars(border-sep, [SideEdge, Sep]),
                             display_row_coordinates(Index),
                             NewIndex is Index+1,
                             write(SideEdge), 
                             display_board_line(Line, Sep), 
                             write(SideEdge), 
                             nl,
                             display_lines(T, NewIndex).

% display_board_line(+Lines, +Sep)
% Display each row of the playable board, with the appropriate separation.
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


% display_stack_handler(+Board, +Size)
% Display the state of each stack on the board, along with a message explaining the ordering of stacks.
display_stack_handler(Board, Size) :- format('Stacks in play: (Organized by Row/Column, Left = Stack Top, Right = Stack Bottom)~n~n',[]),
                                      display_stack_list(Board, 0, Size).

% display_stack_list(+Board, +Index, +Size)
% Display the pieces of each stack on the board.
display_stack_list(_, Size, Size).
display_stack_list([Row | T], Index, Size) :-    RowIndex is Index + 1,
                                                 display_row_stacks(Row, RowIndex/0, Size), 
                                                 display_stack_list(T, RowIndex, Size).

% display_row_stacks(+Row, +Coordinates, +ColumnIndex, +Size)
% Display the pieces of each stack on the current row.
display_row_stacks(_, _/Size, Size) :- format('~n~n',[]).
display_row_stacks([Stack | T], Row/Col, Size) :- NewCol is Col + 1,
                                                                atom_chars(String, Stack),
                                                                format('~w/~w = [~w] ', [Row, NewCol, String]),
                                                                display_row_stacks(T, Row/NewCol, Size).

%
% Other Menu Options
%                   
valid_player(h).
valid_player(c1).
valid_player(c2).
validate_gamemode(P1/P2) :- valid_player(P1), valid_player(P2), write('Gamemode valid!'), nl.

set_gamemode :- format('Which gamemode do you want to play?~nSpecify it using "P1/P2". Options are "h" (Human) and "c{1/2}" (Computer-Level).~n', []),
                read(Gamemode),
                validate_gamemode(Gamemode),
                retract(gamemode(_)), 
                assert(gamemode(Gamemode)),
                !.
set_gamemode :- error_message.

% get_boardsize/0
% Get and reset the size of the board to be used in-game. (To be scraped?)
get_boardsize :- write('What board size do you want? We recommend 4, but choose any integer above 3.'), nl,
                 read(Size),
                 integer(Size), Size > 3,
                 retract(size(_)), assertz(size(Size)),
                 !.
get_boardsize :- error_message.

% error_message/0
% Generic error message to be shown in case of an explosion during program execution.
error_message :- write('An error ocurred while parsing your input. Returning to main menu.'), nl.