# functional-tdd

Samples in Java and Haskell for my talks about functional TDD

## Scoreboard

The example code implements a simple scoreboard to display the score of any
point-based game between two teams/players (e.g. Basketball).

The MVP consists of a simple command line UI using the following commands:
- `a` and `b` for team selection
- '+' and '-' to increase or decrease the score of the selected team
- 'x' to exit the game
- Every command must be followed by pressing `Return`
- Unknown commands are ignored


## Haskell

- Run all tests: `stack test`
- Start app: `stack exec scoreboard-exe`
