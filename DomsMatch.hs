{- 
   DomsMatch: code to play a dominoes match between two players.
   
   The top level function is domsMatch - it takes five arguments:
       games - the number of games to play
       target - the target score to reach
       player1, player2 - two DomsPlayer functions, representing the two players
       seed - an integer to seed the random number generator
   The function returns a pair showing how many games were won by each player.

   The functions of type DomsPlayer must take four arguments:
       The current Hand
       The current Board
       The Player (which will be one of P1 or P2)
       The current Scores
   The function returns a tuple containing the Domino to play and End to play it on.
 -}

module DomsMatch where
    import System.Random
    import Data.List
    import Data.Ord (comparing)
    import Data.Maybe

    -- types used in this module
    type Domino = (Int, Int) -- a single domino
    {- Board data type: either an empty board (InitState) or the current state as represented by
        * the left-most domino (such that in the tuple (x,y), x represents the left-most pips)
        * the right-most domino (such that in the tuple (x,y), y represents the right-most pips)
        * the history of moves in the round so far
     -}
    data Board = InitState | State Domino Domino History deriving (Eq, Show)
    {- History should contain the *full* list of dominos played so far, from leftmost to
       rightmost, together with which player played that move and when they played it
     -}
    type History = [(Domino, Player, MoveNum)]
    data Player = P1 | P2 deriving (Eq, Show)
    data End = L | R deriving (Eq, Show)
    type Scores = (Int, Int) -- P1’s score, P2’s score
    type MoveNum = Int
    type Hand = [Domino]
    {- DomsPlayer is a function that given a Hand, Board, Player and Scores will decide
       which domino to play where. The Player information can be used to "remember" which
       moves in the History of the Board were played by self and which by opponent
     -}

    type DomsPlayer = Hand -> Board -> Player -> Scores -> (Domino, End)

    {- High scoring strategy - When player has a high scoring domino in hand, play it
       Blocking - If opponent is close to winning, switch to a blocking strategy. Play moves that prevent opponent
       from scoring
       Safety - Ensure you always have a safe move
       History Analysis - Keep track of known dominos, Estimating opponent dominos (based on probability)
       Knock tracking
    -}

    {- domSet: a full set of dominoes, unshuffled -}
    domSet = [ (l,r) | l <- [0..6], r <- [0..l] ]

    {- shuffleDoms: returns a shuffled set of dominoes, given a number generator
       It works by generating a random list of numbers, zipping this list together
       with the ordered set of dominos, sorting the resulting pairs based on the random
       numbers that were generated, then outputting the dominos from the resulting list.
     -}
    shuffleDoms :: StdGen -> [Domino]
    shuffleDoms gen = [ d | (r,d) <- sort (zip (randoms gen :: [Int]) domSet)]

    {- domsMatch: play a match of n games between two players,
        given a seed for the random number generator
       input: number of games to play, number of dominos in hand at start of each game,
              target score for each game, functions to determine the next move for each
              of the players, seed for random number generator
       output: a pair of integers, indicating the number of games won by each player
     -}
    domsMatch :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    domsMatch games handSize target p1 p2 seed
        = domsGames games p1 p2 (mkStdGen seed) (0, 0)
          where
          domsGames 0 _  _  _   wins               = wins
          domsGames n p1 p2 gen (p1_wins, p2_wins)
            = domsGames (n-1) p1 p2 gen2 updatedScore
              where
              updatedScore
                | playGame handSize target p1 p2 (if odd n then P1 else P2) gen1 == P1 = (p1_wins+1,p2_wins)
                | otherwise                                            = (p1_wins, p2_wins+1)
              (gen1, gen2) = split gen
              {- Note: the line above is how you split a single generator to get two generators.
                 Each generator will produce a different set of pseudo-random numbers, but a given
                 seed will always produce the same sets of random numbers.
               -}

    {- playGame: play a single game (where winner is determined by a player reaching
          target exactly) between two players
       input: functions to determine the next move for each of the players, player to have
              first go, random number generator 
       output: the winning player
     -}
    playGame :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> Player
    playGame handSize target p1 p2 firstPlayer gen
        = playGame' p1 p2 firstPlayer gen (0, 0)
          where
          playGame' p1 p2 firstPlayer gen (s1, s2)
            | s1 == target = P1
            | s2 == target = P2
            | otherwise   
                = let
                      newScores = playDomsRound handSize target p1 p2 firstPlayer currentG (s1, s2)
                      (currentG, nextG) = split gen
                  in
                  playGame' p1 p2 (if firstPlayer == P1 then P2 else P1) nextG newScores

    {- playDomsRound: given the starting hand size, two dominos players, the player to go first,
        the score at the start of the round, and the random number generator, returns the score at
        the end of the round.
        To complete a round, turns are played until either one player reaches the target or both
        players are blocked.
     -}
    playDomsRound :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> (Int, Int) -> (Int, Int)
    playDomsRound handSize target p1 p2 first gen scores
        = playDomsRound' p1 p2 first (hand1, hand2, InitState, scores)
          where
          -- shuffle the dominoes and generate the initial hands
          shuffled = shuffleDoms gen
          hand1 = take handSize shuffled
          hand2 = take handSize (drop handSize shuffled)
          {- playDomsRound' recursively alternates between each player, keeping track of the game state
             (each player's hand, the board, the scores) until both players are blocked -}
          playDomsRound' p1 p2 turn gameState@(hand1, hand2, board, (score1,score2))
            | (score1 == target) || (score2 == target) || (p1_blocked && p2_blocked) = (score1,score2)
            | turn == P1 && p1_blocked = playDomsRound' p1 p2 P2 gameState
            | turn == P2 && p2_blocked = playDomsRound' p1 p2 P1 gameState
            | turn == P1               = playDomsRound' p1 p2 P2 newGameState
            | otherwise                = playDomsRound' p1 p2 P1 newGameState
              where
              p1_blocked = blocked hand1 board
              p2_blocked = blocked hand2 board
              (domino, end)          -- get next move from appropriate player
                  | turn == P1 = p1 hand1 board turn (score1, score2)
                  | turn == P2 = p2 hand2 board turn (score1, score2)
                                     -- attempt to play this move
              maybeBoard             -- try to play domino at end as returned by the player
                  | turn == P1 && not (elem domino hand1) = Nothing -- can't play a domino you don't have!
                  | turn == P2 && not (elem domino hand2) = Nothing
                  | otherwise = playDom turn domino board end
              newGameState           -- if successful update board state (exit with error otherwise)
                 | maybeBoard == Nothing = error ("Player " ++ show turn ++ " attempted to play an invalid move.")
                 | otherwise             = (newHand1, newHand2, newBoard,
                                              (limitScore score1 newScore1, limitScore score2 newScore2))
              (newHand1, newHand2)   -- remove the domino that was just played
                 | turn == P1 = (hand1\\[domino], hand2)
                 | turn == P2 = (hand1, hand2\\[domino])
              score = scoreBoard newBoard (newHand1 == [] || newHand2 == [])
              (newScore1, newScore2) -- work out updated scores
                 | turn == P1 = (score1+score,score2)
                 | otherwise  = (score1,score2+score)
              limitScore old new     -- make sure new score doesn't exceed target
                 | new > target = old
                 | otherwise    = new
              Just newBoard = maybeBoard -- extract the new board from the Maybe type

    {-
        pipTotalScore: calculates the threes and fives score of the total pips
        input: the pip total
        output: the threes and fives score
    -}
    pipTotalScore :: Int -> Int
    pipTotalScore n
        | n > 20 = 0
        | n == 15 = 8
        | n == 0 = 0 -- avoiding division by 0
        | (n `mod` 3) == 0 = n `div` 3
        | (n `mod` 5) == 0 = n `div` 5
        | otherwise = 0

    {-
        scoreBoard: calculates the score of a given board and adds one if it is the last move. It gets the pip total
          and calls ‘pipTotalScore’ to get the threes and fives score.
        input: the board and a boolean indicating if it is the last move
        output: the score of the board
    -}
    scoreBoard :: Board -> Bool -> Int
    scoreBoard InitState False = 0
    scoreBoard InitState True = 1
    scoreBoard (State leftDomino rightDomino _) b = pipTotalScore (d1 + d2) + lastMove
        where
            d1 -- checking if domino is a double and it is not the only domino on the board
              | fst leftDomino == snd leftDomino && leftDomino /= rightDomino = fst leftDomino + snd leftDomino
              | otherwise = fst leftDomino

            d2 -- checking if domino is a double and it is not the only domino on the board
              | fst rightDomino == snd rightDomino && leftDomino /= rightDomino = fst rightDomino + snd rightDomino
              | otherwise = snd rightDomino

            lastMove
              | b == True = 1
              | otherwise = 0

    {-
        canPlay: checks if a domino can be played on the board
        input: the domino, the end to play on and the board
        output: True if the domino can be played, False otherwise
    -}
    canPlay :: Domino -> End -> Board -> Bool
    canPlay domino _ InitState = True
    canPlay domino L (State leftDomino rightDomino history) =
      fst domino == fst leftDomino || snd domino == fst leftDomino
    canPlay domino R (State leftDomino rightDomino history) =
      fst domino == snd rightDomino || snd domino == snd rightDomino

    {-
        playDom: plays a domino on the board
        input: the player, the domino, the board and the end to play on
        output: the new board if the move is valid, Nothing otherwise
    -}
    playDom :: Player -> Domino -> Board -> End -> Maybe Board
    playDom player domino InitState _ = Just (State domino domino [(domino, player, 1)])
    playDom player domino (State leftDomino rightDomino history) L
        | canPlay domino L (State leftDomino rightDomino history) =
            if snd domino == fst leftDomino then
                Just (State domino rightDomino ((domino, player, moveNum) : history))
            else
                Just (State (snd domino, fst domino) rightDomino ((domino, player, moveNum) : history))
        | otherwise = Nothing
        where
            moveNum = length history + 1
    playDom player domino (State leftDomino rightDomino history) R
        | canPlay domino R (State leftDomino rightDomino history) =
            if fst domino == snd rightDomino then
                Just (State leftDomino domino (history ++ [(domino, player, moveNum)]))
            else
                Just (State leftDomino (snd domino, fst domino) (history ++ [(domino, player, moveNum)]))
        | otherwise = Nothing
        where
            moveNum = length history + 1

    {-
        getValidMoves: gets all the valid moves a player can play based on the hand and board
        Inputs: the hand and the board
        Output: a list of (Domino, End) tuples of valid moves
    -}
    getValidMoves :: Hand -> Board -> [(Domino, End)]
    getValidMoves hand InitState = zip hand (replicate (length hand) L)
    getValidMoves hand (State leftDomino rightDomino history) =
        [(domino, L) | domino <- hand, canPlay domino L (State leftDomino rightDomino history)] ++
        [(domino, R) | domino <- hand, canPlay domino R (State leftDomino rightDomino history)]

    {-
        blocked: checks if a player is blocked. Calls ‘getValidMoves’ to check if there are any valid moves to play in
          the hand. The player is said to be “blocked” if there are no valid moves.
        Inputs: the hand and board
        Output: True if the player is blocked, False otherwise
    -}
    blocked :: Hand -> Board -> Bool
    blocked hand board
      | length (getValidMoves hand board) == 0 = True
      | otherwise = False

    {-
        simplePlayer: plays the first valid move. Calls ‘getValidMoves’ and picks the first Move from the list
          of valid Moves, a Move is a (Domino, End) tuple.
        Inputs: the hand, board, player and scores
        Output: a Move to play
    -}
    simplePlayer :: DomsPlayer
    simplePlayer hand board _ _ = head (getValidMoves hand board)

    {-
      has54: checks if the hand has the 5-4 domino
      input: the hand
      output: True if the hand has the 5-4 domino, False otherwise
    -}
    has54 :: Hand -> Bool
    has54 [] = False
    has54 (x:xs)
      | x == (5, 4) = True
      | otherwise = has54 xs

    {-
      calculateMoveScore: calculates the score of a Move by getting a new board using playDom,
        and calling scoreBoard to get the score of that board.
      Input: the Board and the (Domino, End) Move tuple
      Output: the score of the Move
    -}
    calculateMoveScore :: Board -> (Domino, End) -> Int
    calculateMoveScore InitState (domino, end) = scoreBoard (State domino domino []) False
    calculateMoveScore board (domino, end) = scoreBoard (fromJust (playDom P1 domino board end)) False

    {-
      highestScoringMove: finds the Move which results in the highest score from the current Hand on the current Board.
        It gets all the valid moves by calling getValidMoves and then recursively calls calculateMoveScore to get the
          move with the maximum score.
      Input: the hand and the current board
      Output: the domino and end to play
    -}
    highestScoringMove :: Hand -> Board -> (Domino, End)
    highestScoringMove hand board = findHighestScoringMove (getValidMoves hand board)
      where
        findHighestScoringMove :: [(Domino, End)] -> (Domino, End)
        findHighestScoringMove [] = error "No valid moves"
        findHighestScoringMove [move] = move
        findHighestScoringMove (move:rest) =
            let currentScore = calculateMoveScore board move
                bestMoveInRest = findHighestScoringMove rest
                bestScoreInRest = calculateMoveScore board bestMoveInRest
            in if currentScore >= bestScoreInRest
                then move
                else bestMoveInRest

    {-
        isOpponentWinning: checks if the opponent is within 12 points of winning
        input: the current player and the scores
        output: True if the opponent is within 12 points of winning, False otherwise
    -}
    isOpponentWinning :: Player -> Scores -> Bool
    isOpponentWinning player scores | player == P1 = 61 - snd scores <= 12
                                    | player == P2 = 61 - fst scores <= 12

    {-
       playBlockingMove: plays a move that blocks the opponent from winning. If there is no blocking move, by making
        both the ends the same, increasing the odds of the opponent knocking, if there is no blocking move, it
          calls ‘highestScoringMove’.
       There is potential to make this stronger by checking where the opponent has knocked and making the ends those
          numbers on which the opponent has knocked.
       Input: the hand, board and player
       Output: the domino and end to play
    -}
    playBlockingMove :: Hand -> Board -> Player -> (Domino, End)
    playBlockingMove hand (State leftDomino rightDomino history) player
        | length blockingMoves > 0 = head blockingMoves
        | otherwise = highestScoringMove hand (State leftDomino rightDomino history)
          where
              blockingMoves =
                [(domino, end) | (domino, end) <- getValidMoves hand (State leftDomino rightDomino history),
                  (domino == (fst leftDomino, snd rightDomino)) || (domino == (snd rightDomino, fst leftDomino))]

    {-
        isPlayerCloseToWinning: checks if the player is within 10 points of winning
        Input: the current player and scores
        Output: True if the player is within 10 points of winning, False otherwise
    -}
    isPlayerCloseToWinning :: Player -> Scores -> Bool
    isPlayerCloseToWinning player scores | player == P1 = 61 - fst scores <= 10
                                         | player == P2 = 61 - snd scores <= 10

    {-
      getPlayerScore: gets the player’s score from the Scores tuple
      Input: the player and Scores tuple
      Output: the score of the player
    -}
    getPlayerScore :: Player -> Scores -> Int
    getPlayerScore player scores | player == P1 = fst scores
                                 | player == P2 = snd scores

    {-
      playMoveTo59: play a move that will result in a score of 59. Calls ‘highestScoringMove’
        if a move to 59 does not exist.
      Inputs: the hand, board, player and scores
      Output: the domino and end to play
    -}
    playMoveTo59 :: Hand -> Board -> Player -> Scores -> (Domino, End)
    playMoveTo59 hand (State leftDomino rightDomino history) player scores
        | length movesTo59 > 0 = head movesTo59
        | otherwise = highestScoringMove hand (State leftDomino rightDomino history)
        where
            movesTo59 = [(domino, end) | (domino, end) <- getValidMoves hand (State leftDomino rightDomino history),
              (calculateMoveScore (State leftDomino rightDomino history) (domino, end)) + (getPlayerScore player scores)
                == 59]

    {-
      playWinningMove: play a move that will result in a score of 61, if there is no such move,
        play a move that will result in a score of 59. Calls ‘playMoveTo59’ if a winning move does not exist
      Input: the hand, board, player and scores
      Output: the domino and end to play

    -}
    playWinningMove :: Hand -> Board -> Player -> Scores -> (Domino, End)
    playWinningMove hand (State leftDomino rightDomino _) player scores
      | length winningMoves > 0 = head winningMoves
      | otherwise = playMoveTo59 hand (State leftDomino rightDomino []) player scores
      where
        winningMoves = [(domino, end) | (domino, end) <- getValidMoves hand (State leftDomino rightDomino []),
          (calculateMoveScore (State leftDomino rightDomino []) (domino, end)) + (getPlayerScore player scores) == 61]


    {-
       smartPlayer: uses multiple strategies to play a move to maximise its chances of victory
       if it is the first move, it checks if the player has the 5-4 domino in its hand, by calling has54, if it does,
        it plays it, since it scores 3 but the maximum you can score in reply is 2, otherwise it
          plays the ‘highestScoringMove’
       if ‘playerCloseToWinning’, it will ‘playWinningMove’ or a move that will result in a score of 59,
        since it is much easier to reach 59 from 61
       If the player is not winning, and if the opponent is close to winning, which is checked by calling
        ‘isOpponentWinning’, then the player will switch to a defensive strategy and ‘playBlockingMove’
       Otherwise, if it is in the middle of the game, it will play the ‘highestScoringMove’
       input: the hand, board, player and score
       output: a Move to play
    -}
    smartPlayer :: DomsPlayer
    smartPlayer hand InitState _ _
        | has54 hand = ((5, 4), L)
        | otherwise = highestScoringMove hand InitState
    smartPlayer hand board player scores
        | isPlayerCloseToWinning player scores = playWinningMove hand board player scores
        | isOpponentWinning player scores = playBlockingMove hand board player
        | otherwise = highestScoringMove hand board
