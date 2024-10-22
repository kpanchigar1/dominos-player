# Dominoes Match

This Haskell module provides functionality to play a dominoes match between two players. It includes the top-level function `domsMatch` to simulate matches, as well as various utility functions and data types to manage the game state and player strategies.

## Usage

The main function in this module is `domsMatch`, which takes the following arguments:
- `games`: The number of games to play.
- `handSize`: The number of dominoes in each player's hand at the start of each game.
- `target`: The target score to reach to win a game.
- `player1`: A function of type `DomsPlayer` representing the first player.
- `player2`: A function of type `DomsPlayer` representing the second player.
- `seed`: An integer to seed the random number generator.

The function returns a pair of integers indicating the number of games won by each player.

### Data Types
Domino: A type alias for a pair of integers representing a single domino.
Board: Represents the current state of the board.
History: A list of tuples containing the domino played, the player who played it, and the move number.
Player: An enumeration of the two players, P1 and P2.
End: An enumeration of the two ends of the board, L (Left) and R (Right).
Scores: A pair of integers representing the scores of P1 and P2.
MoveNum: An integer representing the move number.
Hand: A list of Domino representing a player's hand.
DomsPlayer: A type alias for a function that decides which domino to play based on the current hand, board, player, and scores.

###Functions
shuffleDoms: Returns a shuffled set of dominoes given a random number generator.
domsMatch: Plays a match of n games between two players.
playGame: Plays a single game between two players.
playDomsRound: Plays a round of dominoes.
pipTotalScore: Calculates the threes and fives score of the total pips.
scoreBoard: Calculates the score of a given board.
canPlay: Checks if a domino can be played on the board.
playDom: Plays a domino on the board.
getValidMoves: Gets all the valid moves a player can play.
blocked: Checks if a player is blocked.
simplePlayer: A simple player strategy that plays the first valid move.
highestScoringMove: Finds the move which results in the highest score.
smartPlayer: A more advanced player strategy that uses multiple strategies to maximize its chances of victory.

###Player Strategies
simplePlayer: Plays the first valid move from the hand.
smartPlayer: Uses various strategies to maximize its chances of victory, including high scoring, blocking, and safety moves.
Contributing

Contributions are welcome! Please fork the repository and submit a pull request with your changes.

###License
This project is licensed under the MIT License.
