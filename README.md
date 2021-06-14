# draughts-online

To build the project simply run 
```
stack build
```

To host the game run
```
stack run draughts-online-host-exe [PORT]
```
where `PORT` is the port on wich you want the host to be accessible.
By default `PORT = 8080`


To play the game run
```
stack run draughts-online-client-exe
```

## Usage


After running the client, 
you will be presented with 3 buttons: `Play Game`, `Watch Game` and `Exit`.
You can select the buttons using arrows and `ENTER`.

- selecting `Exit` will exit the game
- after selecting `Play Game` or `Watch Game` you will be prompted to enter 
  the adress of the host. 
  (You do not have to prefix the host with `'http://'`) 
  For example:
  ```
  Enter the game host address:
  localhost:8080
  ``` 
  After enering the host adress, you will see the game board.
  - If you sellected `Watch Game`, you will only observe the game
  - If you selected `Play Game` you will be assigned pieces of a certain color 
    and will be able to move them when it is your turn.

While in the game you will be able to move pieces by selecting a piece and
selecting which direction to move the piece.
Selecting both pieces and directions is done using arrows and `ENTER`. 
Before a piece / direction it will be highlighted which piece /direction you 
will chose if you press `ENTER`.
When selecting the direction you can press `ESC` and return to selecting the a 
piece.


While in the game / watching the game you can return to the menu bu pressing
`ESC`. Be careful, because once you exit to the menu, you won't be able to 
rejoin the game, the client will attempt to join again, but the host will treat
you as a new player and reject you, since both players alredy joined.
(I left this inconvenience, due to the inconvenience of implementing yet 
another little detail like this.)




