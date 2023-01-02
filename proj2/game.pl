:-consult('terminal.pl').
:-consult('logic.pl').

:- use_module(library(random)).

% play/0
% Start the application.
play :- main_menu.

% main_menu/0
% Display a menu with various options. Loops until it receives a valid input.
main_menu :- repeat,
             format('~nMain Menu~n~n1- Choose gamemode~n2- Start the game~n3- Exit the program~nDo not forget to add a dot after every input!~n', []),
             catch(read(Option), Error, fail),
             (
                Option = 3, main_menu(Option);
                main_menu(Option), false
             ).


% main_menu(+Option)
% Call the appropriate function based on the option chosen.
main_menu(1) :- set_gamemode.
main_menu(2) :- start.
main_menu(3) :- write('Goodbye!').

% start/0
% Start the game, initializing the board and entering the game loop.
start :- size(Size),
         total_pieces(Turns),
         initial_state(Size, GameState),
         display_game(GameState),
         gamemode(PlayerOneType/_),
         game_loop(GameState-PlayerOneType, Turns).

% game_loop(+GamePhase, +TurnsLeft)
% The game's main loop.
% Verify that the game is not over, either by victory or lack of playable pieces (which ends the game in a Draw),
% then asks the player for a move, switching players afterward and rendering the new board.
% On game over, the loop ends.
game_loop(GameState-PlayerType, TurnsLeft) :- game_over(GameState, Winner, TurnsLeft), !,
                                              winner_message(Winner).
game_loop(GameState-PlayerType, TurnsLeft) :- ask_move(GameState, PlayerType, TurnsLeft, Placement),
                                   move(GameState, Placement, MidGameState), !,
                                   (
                                    Placement = give/up, fail
                                   ;
                                    game_over(MidGameState, Winner, TurnsLeft), !,
                                    display_game(MidGameState), !,
                                    winner_message(Winner)
                                   ;
                                    get_stack(MidGameState, Placement, Stack),
                                    format('Stack to be moved: ~w~n~n', [Stack]),
                                    ask_move(MidGameState, PlayerType, Placement, TurnsLeft, Move),
                                    move(MidGameState, Move, Placement, NewGameState),
                                    NewTurnsLeft is TurnsLeft-1,
                                    next_player_type(PlayerType, NextPlayerType),
                                    display_game(NewGameState), !,
                                    game_loop(NewGameState-NextPlayerType, NewTurnsLeft)
                                   ).

% ask_move(+GameState, +PlayerType, +TurnsLeft, -Move)
% Ask a human player for coordinates to place its piece or lets the computer choose a move, depending on its difficulty.
ask_move(Board-Player, h, TurnsLeft, Row/Col) :- repeat,
                                                 format('Turns left: ~w. If no one wins in the remaining turns, the game will end in a draw.~n', [TurnsLeft]),
                                                 format('Time to move, ~w. Where will you place your stone? (Input in "Row/Col" format)~n', [Player]),
                                                 catch(read(Row/Col), Error, fail),
                                                 (
                                                  Row/Col = give/up, surrender_message(Player), !, fail
                                                  ;
                                                  number(Row), number(Col)
                                                 ),
                                                 Row > 0, Col > 0,
                                                 move(Board-Player, Row/Col, _).  % move without saving the board, to check for potentially invalid moves
ask_move(GameState, ComputerLevel, TurnsLeft, Placement) :- choose_move(ComputerLevel, GameState, Placement),
                                                            format('Beep. I place my piece here: ~w~n~n', [Placement]).

% ask_move(+GameState, +PlayerType, +Placement, +TurnsLeft, -Move)
% Ask a human player for a string of characters to move its stack or lets the computer choose a move, depending on its difficulty.
ask_move(Board-Player, h, Placement, TurnsLeft, Move) :- repeat,
                                                                get_stack(Board, Placement, Stack),
                                                                format('~w, where will you move your stack? (Input a string of characters X, such that:~n', [Player]),
                                                                format('X is formed by "n", "s", "e" or "w" (North, South, East, West), and you cannot move to where you were directly before (Which means no "ns", "sn", "ew" or "we").~n',[]),
                                                                format('Make sure the length of your string matches the length of the stack you are moving.~n',[]),
                                                                format('The first piece to be placed is the one on the right.~n',[]),
                                                                catch(read(Move), Error, fail),
                                                                move(Board-Player, Move, Placement, _). % move without saving the board, to check for potentially invalid moves
ask_move(GameState, ComputerLevel, Placement, TurnsLeft, Move) :- choose_move(ComputerLevel, GameState, Placement, Move),
                                                                     format('Boop. I move the stack like this: ~w~n~n', [Move]).

% choose_move(+ComputerLevel, +GameState, -Placement)
% The level 1 computer chooses a random valid placement, while the level 2 computer picks a random valid move from a list of "best" placements.
choose_move(c1, GameState, Placement) :- valid_moves(GameState, Moves), 
                                         random_select(Placement, Moves, _).
choose_move(c2, GameState, Placement) :- setof(Value-Move, NewState^(move(GameState, Move, NewState),
                                         value(NewState, Move, Value)), Results),
                                         last(Results, Value-_),
                                         include(highest_value(Value), Results, BestMoves),
                                         random_select(_-Placement, BestMoves, _).

% choose_move(+ComputerLevel, +GameState, +Placement, -Move)
% The level 1 computer chooses a random valid move, while the level 2 computer picks a random valid move from a list of "best" moves.
choose_move(c1, GameState, Placement, Move) :- valid_moves(GameState, Placement, Moves),
                                               random_select(Move, Moves, _).
choose_move(c2, Board-Player, Placement, Move) :- valid_moves(Board-Player, Placement, Moves),
                                                  value_moves(Board-Player, Placement, Moves, ValuedMoves),
                                                  sort(ValuedMoves, SortedMoves),
                                                  last(SortedMoves, Value-_),
                                                  include(highest_value(Value), SortedMoves, BestMoves),
                                                  random_select(_-Move, BestMoves, _).

% winner_message(+Winner)
% Send a message after game over, depending on the result (triangle win, circle win or draw).
winner_message(triangle) :- format('You are a player of acute intelligence. Nice win, triangle!~n~nGAME OVER~n',[]).
winner_message(circle) :- format('A round of applause to the winner: circle!~n~nGAME OVER~n',[]).
winner_message(draw) :- format('After a true display of skill, the match unfortunately ends in a draw.~n~nGAME OVER~n',[]).

% surrender_message(+Loser)
% Send a message marking the surrender of the player, before sending the appropriate winner message regarding the opponent.
surrender_message(Loser) :- turn_change(Loser, Winner),
                            format('~n~w surrenders...~n', [Loser]),
                            winner_message(Winner).
