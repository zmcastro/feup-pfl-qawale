encoding(utf8).

%
% Constants and variables
%

:- dynamic gamemode/1, size/1.
gamemode(h/h).
size(4).

total_pieces(16).

%
% ASCII Character Lists
%

% board_chars(?BoardPosition, ?CharArray)
% Depending on the entity to be rendered, return an array of characters to be displayed in the terminal.
board_chars(border-top, ['\x256D\', '\x2500\', '\x256E\']).
board_chars(border-bottom, ['\x2570\', '\x2500\', '\x256F\']).
board_chars(border-sep, ['\x2502\', '|']).
board_chars(board-empty, ' ').

% piece_char(?Player, ?PlayerChar)
% Return the character that represents the given player.
piece_char(triangle, '\x25B2\').
piece_char(circle, '\x25CF\').
piece_char(neutral, '\x25A0\').

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
display_border(Size, Side) :- board_chars(border-Side, [LeftEdge, Middle, RightEdge]), 
                              format('  ~w', [LeftEdge]), display_straight_border(Size, Middle), write(RightEdge), nl.

% display_straight_border(+Size, +Char)
% Display the straight line common to the top and bottom borders.
display_straight_border(1, Char):- write(Char).
display_straight_border(Size, Char) :- write(Char), write(Char), NewSize is Size-1, display_straight_border(NewSize, Char).

% display_lines(+Lines, +Index)
% Display each row of the board, including coordinates and border.
display_lines([], _).
display_lines([Line | T], Index) :- board_chars(border-sep, [SideEdge, Sep]),
                             display_row_coordinates(Index),
                             NewIndex is Index+1,
                             write(SideEdge), 
                             display_board_line(Line, Sep), 
                             write(SideEdge), 
                             nl,
                             display_lines(T, NewIndex).

% display_board_line(+Lines, +Sep)
% Display each row of the playable board, with the appropriate separation.
display_board_line([[]], Sep) :- board_chars(board-empty, Char), 
                                 write(Char).
display_board_line([[] | T], Sep) :- board_chars(board-empty, Char), 
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

% valid_player(+PlayerType)
% Check the validity of the given player type.
valid_player(h).
valid_player(c1).
valid_player(c2).

% validate_gamemode(+Gamemode)
% Validates the gamemode typed by the user.
validate_gamemode(P1/P2) :- valid_player(P1), valid_player(P2).

% set_gamemode/0
% Ask the user to choose a gamemode
set_gamemode :- repeat,
                format('Which gamemode do you want to play?~nSpecify it using "P1/P2". Options are "h" (Human) and "c{1/2}" (Computer1 - Easy, Computer2 - Hard).~nDefault is "h/h".~n', []),
                catch(read(Gamemode), Error, fail),
                validate_gamemode(Gamemode),
                retract(gamemode(_)), 
                assertz(gamemode(Gamemode)),
                !.


% set_boardsize/0
% Set the size of the board to be used in-game. (Game-over check only supports 4x4 boards, scrapped.)
/*
set_boardsize :- write('What board size do you want? We recommend 4, but choose any integer above 3.'), nl,
                 read(Size),
                 integer(Size), Size > 3,
                 retract(size(_)), assertz(size(Size)),
                 !.
set_boardsize :- error_message.
*/
