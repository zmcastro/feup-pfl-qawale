:-consult('terminal.pl').
:-consult('logic.pl').

play :- main_menu.

main_menu :- repeat,
             format('What would you like to do?~n1- Choose gamemode~n2- Choose board size~n3- Start the game~n4- Exit the program~nDo not forget to add a dot after every input!~n', []),
             read(Option),
             (
                Option = 4, main_menu(Option);
                main_menu(Option), false
             ).


main_menu(1) :- write('Chose gamemode.'), nl.
main_menu(2) :- write('Chose boardsize.'), nl, get_boardsize.
main_menu(3) :- start.
main_menu(4) :- write('Goodbye!').

start :- size(Size),
         initial_state(Size, GameState),
         display_game(GameState), nl,
         ask_move(GameState, h, Row/Col).

ask_move(Board-Player, h, Row/Col) :- repeat,
                                      format('~nTime to move, ~w. Where will you place your stone? (Input in "Row/Col" format)~n', [Player]),
                                      read(Row/Col),
                                      number(Row), number(Col),
                                      Row > 0, Col > 0,
                                      move(Board-Player, Row/Col, _).
