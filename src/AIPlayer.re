open! CS17SetupGame;
open Game;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;
  /* maxDepth represents the maximum depth to which the minimax algorithm runs
   * based on the speedOfControl (0 - 100)*/
  let speedOfControl = 0;
  let maxDepth = 2 + speedOfControl / 12;

  /* alphaBetaMinimax: (PlayerGame.state, int, float, float) => float
      Input: the state, s an int (depth) the depth, alpha, the best value that
             the maximizing player (P1) has found so far and beta, the best value
             that the minimizing player (P2) has found so far.
      Output: a float, representing the estimated value of the best achievable
              outcome for the current player, assuming optimal play from both
              sides.

     Recursion Diagrams:
        OI: ({board: [Empty, Empty],
              turn: P1,
              usedNothingP1: false,
              usedNothingP2: false},
              },
            0,
            infinity,
            -infinity)
            RI: NA (depth is 0)
            RO: NA
              Ideation: NA
        OO: estimateValue({board: [Empty, Empty],
                          turn: P1,
                          usedNothingP1: false,
                          usedNothingP2: false},
                         )

        OI: (
          ({board: [Frog, Frog, Frog, Empty, Toad, Empty, Toad, Toad],
           turn: P1,
           usedNothingP1: false,
           usedNothingP2: true,},
           1,
           infinity,
           -infinity)
            RI: NA (gameStatus is a Win for P1 because Frogs are trapped)
            RO: NA
              Ideation: NA
        OO: estimateValue(
              {board: [Frog, Frog, Frog, Empty, Toad, Empty, Toad, Toad],
               turn: P1,
               usedNothingP1: false,
               usedNothingP1: true},
               )

     */
  let rec alphaBetaMinimax: (PlayerGame.state, int, float, float) => float =
    (s, depth, alpha, beta) => {
      let possibleLegalMoves = PlayerGame.legalMoves(s);
      switch (PlayerGame.gameStatus(s), possibleLegalMoves, depth) {
      | (Win(_), _, _)
      | (_, _, 0)
      | (_, [], _) => PlayerGame.estimateValue(s)
      | (Ongoing(player), _, _) =>
        if (player == P1) {
          let rec returnMax: (list(PlayerGame.move), float, float) => float = (
            (alom, alphaVal, bestVal) =>
              switch (alom) {
              | [] => bestVal
              | [hd, ...tl] =>
                let currVal =
                  alphaBetaMinimax(
                    PlayerGame.nextState(s, hd),
                    depth - 1,
                    alphaVal,
                    beta,
                  );
                let bestOverall = max(currVal, bestVal);
                let bestAlpha = max(currVal, alphaVal);

                if (bestAlpha >= beta) {
                  bestOverall;
                } else {
                  returnMax(tl, bestAlpha, bestOverall);
                };
              }
          );
          returnMax(possibleLegalMoves, alpha, -. infinity);
        } else {
          let rec returnMin: (list(PlayerGame.move), float, float) => float = (
            (alom, betaVal, bestVal) =>
              switch (alom) {
              | [] => bestVal
              | [hd, ...tl] =>
                let currVal =
                  alphaBetaMinimax(
                    PlayerGame.nextState(s, hd),
                    depth - 1,
                    alpha,
                    betaVal,
                  );
                let bestOverall = min(currVal, bestVal);
                let bestBeta = min(currVal, betaVal);

                if (bestBeta <= alpha) {
                  bestOverall;
                } else {
                  returnMin(tl, bestBeta, bestOverall);
                };
              }
          );
          returnMin(possibleLegalMoves, beta, infinity);
        }
      };
    };

  /* nextMove: PlayerGame.state => PlayerGame.move
           Input: the state of the game
           Output: the best move for the AI Player, based on the minimax algorithm
     */
  let nextMove: PlayerGame.state => PlayerGame.move =
    s => {
      let possibleLegalMoves = PlayerGame.legalMoves(s);
      switch (possibleLegalMoves) {
      | [] => failwith("THERE ARE NO LEGAL MOVES!")
      | [m] => m
      | [_, ..._] =>
        let scoresOfPossibleLegalMoves =
          List.map(
            m =>
              (
                m,
                alphaBetaMinimax(
                  PlayerGame.nextState(s, m),
                  maxDepth - 1,
                  -. infinity,
                  infinity,
                ),
              ),
            possibleLegalMoves,
          );
        let chooseBestMove:
          ((float, float) => bool, list((PlayerGame.move, float))) =>
          (PlayerGame.move, float) = (
          (valueComparison, movesAndScores) =>
            List.fold_left(
              (currBest, nextPossible) =>
                if (valueComparison(snd(nextPossible), snd(currBest))) {
                  nextPossible;
                } else {
                  currBest;
                },
              List.hd(movesAndScores),
              List.tl(movesAndScores),
            )
        );

        switch (PlayerGame.gameStatus(s)) {
        | Ongoing(P1) =>
          let (bestPossibleMove, _) =
            chooseBestMove((a, b) => a > b, scoresOfPossibleLegalMoves);
          bestPossibleMove;
        | Ongoing(P2) =>
          let (bestPossibleMove, _) =
            chooseBestMove((a, b) => a < b, scoresOfPossibleLegalMoves);
          bestPossibleMove;
        | Win(_) => failwith("GAME IS OVER!")
        };
      };
    };

  /* TEAM NAME!!! */
  let playerName = "SpikeTyson";
};

module TestGame = TNF.TNF;
open Player;

module TestAIPlayer = AIPlayer(TestGame);
module MyAIPlayer: Player = TestAIPlayer;
open TestAIPlayer;
