# PFL TP2: Qawale

### **Grupo T11_Qawale3
### Pedro Alexandre Ferreira e Silva - up202004985 (%)
### José Maria Borges Pires do Couto e Castro - up202006963 (%)

# 1. Installation and Execution

### Linux:

For program execution in Linux, just the basic SICSTUS 4.7.1 installation is needed.

### Windows:

For program execution in Windows, only the basic SICSTUS 4.7.1 installation is needed. However, to have the most faithful recreation of the game's display on Linux, change SICSTUS' console font to 'Consolas', as displayed in the screenshot below:

![windows_console](docs/windows_console.png)

# 2. Game Description

The chosen game for this project was [Qawale](https://www.hachetteboardgames.com/products/qawale). Its product page is found [here](https://www.hachetteboardgames.com/products/qawale) and its ruleset, as explained below, is found [here](https://randolphca.sharepoint.com/sites/Public/Documents%20partages/Forms/AllItems.aspx?id=%2Fsites%2FPublic%2FDocuments%20partages%2FSales%20%2D%20Ventes%2FTOOLS%20%2D%20OUTILS%2FVisuels%20jeux%20%2D%20Games%20Visual%2FUSA%2FQawale%20%2D%20media%20kit%2FQawale%20%2D%20rules%2Epdf&parent=%2Fsites%2FPublic%2FDocuments%20partages%2FSales%20%2D%20Ventes%2FTOOLS%20%2D%20OUTILS%2FVisuels%20jeux%20%2D%20Games%20Visual%2FUSA%2FQawale%20%2D%20media%20kit&p=true&ga=1).

## Rules

Qawale is a 2-player versus game in which both players start with 8 pebbles of a specific color. Because of this, the game has a total turn count of 16. To simplify, we made each player's pieces `Circles` and `Triangles` and made the neutral pieces into `Square` ones.
To win, a player needs to make a line out of 4 of their colored pieces (horizontally, vertically or diagonally). These pieces must be the ones at the top of the stack.
If both players have played all their pebbles and no lines have been made, the game ends in a draw. An example of a winning line is shown below:

![win](docs/win.png)

The game board is a 4 by 4 grid and starts off with 4 stacks of 2 `Squares` in each corner, as shown below: 

![start_display](docs/start_display.png)

To play, a player must choose a stack to place one piece. AS such, to start, the only available moves are to place a piece in 1/1, 4/1, 1/4 and 4/4. <br> After choosing a piece placement, the player must take that stack of pieces and lays them adjacently to that stack. Each piece gets layed one at a time, starting with the piece at the bottom of the stack. Each piece movement MUST be orthogonally adjacent to the previous one (vertically or horizontally but NOT diagonally). Adding to this, the player cannot move backwards to a space they passed through - they can only circle back to it. This stack movement is displayed in the image below:

![stack_movement](docs/stack_movement.png)

# 3. Game Logic:
## 3.1 Internal Representation of the State of the Game

The state of the game is condensed into a pair named `GameState` that consists of the game's `Board` and `Player`. The `Board` is a matrix (list of lists) of 4x4 size containing the game's grid and each grid slot is filled with an empty space or a `Triangle`, `Circle` or `Square` piece.
The `Player` is an atom that is solely used to dictate whose turn it is, `triangle` or `circle`.
<br>
To initialize and setup the game, `initial_state(Size, GameState)` is called. Initially, we wanted to try having differing board sizes for different types of difficulty and playing fields, but ultimately decided against it since a 4 by 4 grid for Qawale is fulcrum.
<br>
To enter the game loop, the `gamemode` must also be set. Valid gamemodes include 'Human vs Human' (`h/h`), 'Human vs Easy AI' (`h/c1`) and 'Human vs Hard AI' (`h/c2`).
<br>

## 3.2 Game State View

Our program presents a simple but effective interface for the game.
Inputs are also sanitized with auxiliary functions.
<br>
Starting with the menu, we display 3 options onscreen:
1. *Choose gamemode* - Allows for gamemode and player type selection. Players can either be `h` (human), `c1` (easy computer player) or `c2` (hard computer player). Default is `h/c1`.
2. *Start the game* - Starts the game with the selected gamemode.
3. *Exit the program* - Exits the program.
<br> 
For the game's board itself, we used the `get_chars` predicate to construct an easy-to-understand grid with specific hexadecimal characters. We pair this with a text-description of the game state, showing each grid slot's stack below the top-down view of the board, as shown below:

![start_display](docs/start_display.png)

## 3.3 Moves Execution

Move validation checks for out-of-bounds inputs and illegal stack movements. the `move` predicate has two variants depending on the move it is performing: a piece placement move (first part of a user's turn) or a stack movement move (latter part). Both of these executions use the `GameState` (Board/Player pair)
and the movement input to validate and return a new GameState. <br>
- *`move(Board-Player, Row/Col, piece, NewBoard-Player)`* - Takes in a Row/Col position pair to validate and return a GameState that will be used in the second part of the turn.
- *`move(Board-Player, Move, Row/Col, stack, NewBoard-NextPlayer)`* - Takes in a Row/Col position pair where the previous stack will be moved to. With this, it validates the move and rearranges the stack if there was one at that position, returning a GameState ready for the next turn.

## 3.4 List of Valid Moves 

Valid moves are listed with the predicate `valid_moves`. Again, this function has two variants: one for piece placement and one for stack movement.
- `valid_moves(GameState, Moves)` - Returns a list of valid piece placements (stack positions).
- `valid_moves(Board-Player, Row/Col, Moves)` - Returns a list of valid stack movements. These movements are comprised of a string of characters symbolising a series of cardinal directions ('n'orth, 'w'est, 'e'ast, 's'outh).

## 3.5 End of Game 

The `game_over(+GameState, -Winner)` checks for 4-in-a-line's in the game's board. It checks horizontal, vertical and diagonal lines.

## 3.6 Board Evaluation

For board evaluation. `value` has two variants, one for each part of a user's turn. 

- `value(Board-Player, Row/Col, Value)` - Evaluates piece placement. Analyses the board and gives a calculated weight to a Row/Col position depending on the turn's player. A move's weight is calculated by counting the number of pieces in the specified position's stack.

- `value(Board-Player, Value)` - Evaluates a board depending on the player being analysed. This predicate is used to weigh a possible move's returning board. The board's value is increased by the number of player pieces in the board and
decreased by the number of opponent pieces. If the board represents a game over (winning) situation, its value is automatically set to 99.

# `BOARD EVAL TO BE FINISHED`

## 3.7 Computer Move

As stated in the project's requirements, the level 1 difficulty computer will always perform randomized placements and movements. The level 2 computer, however, makes use of the board evaluation functions and calculates


