# Battleships Game

This is a simple implementation of the Battleships game in Haskell played in commandline. The game is played between two players, who take turns shooting at each other's ships on a game board.

## Getting Started
To run the game, you'll need to have Haskell installed on your system fromhttps://www.haskell.org/downloads/

Download `.exe` file

or

Clone this repository to your local machine using the following command:
```
git clone https://github.com/Larppuli/Battleships.git 
cd Battleships
runghc "app/Main.hs"
```
## How to Play
1. The game board consists of a 10x10 grid, with rows labeled from 'a' to 'j' and columns numbered from 0 to 9.
2. Each player takes turns to place their ships on the game board. Ships have lengths of 2, 3, 4, or 5 units.
3. After all ships are placed, players take turns shooting at each other's ships by specifying a coordinate (e.g., 'a1').
4. If a shot hits an opponent's ship, the player scores a hit. If all units of a ship are hit, it sinks.
5. The game continues until all ships of one player are sunk.
