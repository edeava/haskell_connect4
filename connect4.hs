module Connect4 where
import RoseTree
import Data.List (transpose)

data Player = Z | C deriving Eq

instance Show Player where
    show Z = "Z"
    show C = "C"

changePlayer :: Player -> Player
changePlayer Z = C
changePlayer C = Z

data Field = Empty | Taken Player deriving Eq

instance Show Field where
    show Empty = " "
    show (Taken p) = show p

data GameState = Open | Won Player | Draw | Invalid String deriving (Show, Eq)

newtype Board = Board [[Field]]

instance Show Board where
    show (Board board) = concat [showRow row ++ "\n" | row <- board] where
        showRow row = "|" ++ concat [show field ++ "|" | field <- row]

data ConnectFour = ConnectFour {
    board :: Board,
    nextToPlay :: Player,
    gameState :: GameState
}

instance Show ConnectFour where
    show cFour =
        "Board:\n" ++ show (board cFour) ++
        "Current state: " ++ show (gameState cFour) ++
        "\nPlayer " ++ show (nextToPlay cFour) ++ "'s turn\n"

newtype GameStateOp a = GameStateOp { gameStateOp :: ConnectFour -> (a, ConnectFour) }

instance Functor GameStateOp where
    fmap f (GameStateOp g) = GameStateOp $ \s ->
        let (x, s') = g s
        in (f x, s')

instance Applicative GameStateOp where
    pure x = GameStateOp $ \s -> (x, s)
    (GameStateOp f) <*> (GameStateOp g) = GameStateOp $ \s ->
        let (f', s')  = f s
            (x, s'') = g s'
        in (f' x, s'')

instance Monad GameStateOp where
    return = pure
    (GameStateOp g) >>= f = GameStateOp $ \s ->
        let (x, s') = g s
            GameStateOp g' = f x
        in g' s'

isMoveValid :: Board -> Int -> Either String Bool
isMoveValid (Board board) col
    | col < 0 || col >= length (head board) = Left "Dodavanje van granica table"
    | head (board !! col) == Empty = Right True
    | otherwise = Left "Dodavanje u punu kolonu"

applyMove :: Int -> GameStateOp Bool
applyMove col = GameStateOp $ \game ->
    case gameState game of
        Open ->
            let Board oldBoard = board game
                player = nextToPlay game
            in case isMoveValid (Board oldBoard) col of
                Right True ->
                    let newBoard = Board (updateColumn oldBoard col player)
                        newState = checkEnd newBoard
                        newPlayer = changePlayer player
                    in (True, ConnectFour {
                        board = newBoard,
                        gameState = newState,
                        nextToPlay = newPlayer
                    })
                Left errorMsg ->
                    (False, game { gameState = Invalid errorMsg })
        _ -> (False, game { gameState = Invalid "Game is already finished" })

applyMovesFromList :: [Int] -> GameStateOp Bool
applyMovesFromList = foldr ((>>) . applyMove) (return False)

updateColumn :: [[Field]] -> Int -> Player -> [[Field]]
updateColumn board col player =
    let column = board !! col
        updatedColumn = dropPiece column player
    in take col board ++ [updatedColumn] ++ drop (col + 1) board

dropPiece :: [Field] -> Player -> [Field]
dropPiece [] _ = []
dropPiece (Empty:xs) player = Taken player : xs
dropPiece (x:xs) player = x : dropPiece xs player

checkEnd :: Board -> GameState
checkEnd board@(Board fields)
    | any (checkWin board) [Z, C] = Won (head [p | p <- [Z, C], checkWin board p])
    | all (notElem Empty) fields = Draw
    | otherwise = Open

checkWin :: Board -> Player -> Bool
checkWin (Board fields) player =
    let lines = fields ++ transpose fields ++ diagonals fields ++ diagonals (reverse fields)
    in any (hasConsecutive 4 (Taken player)) lines

hasConsecutive :: Int -> Field -> [Field] -> Bool
hasConsecutive n x = any ((>= n) . length) . filter (all (== x)) . divvy n 1

diagonals :: [[a]] -> [[a]]
diagonals = transpose . zipWith drop [0..]

divvy :: Int -> Int -> [a] -> [[a]]
divvy n step = takeWhile ((== n) . length) . map (take n) . iterate (drop step)

validMoves :: Board -> [Int]
validMoves board = [col | col <- [0..6], isMsg (isMoveValid board col)]
  where
    isMsg (Right _) = True
    isMsg (Left _) = False

generateMoveTree :: ConnectFour -> Int -> Rose ConnectFour
generateMoveTree initialGame maxDepth = generateRose generateMoves maxDepth initialGame
  where
    generateMoves :: ConnectFour -> [ConnectFour]
    generateMoves game@ConnectFour{gameState = Open, board = b} =
      [snd $ gameStateOp (applyMove move) game | move <- validMoves b]
    generateMoves _ = [] --ako nije open -> kraj

getGameState :: Rose ConnectFour -> GameState
getGameState (Node game _) = gameState game

countWinningPositions :: Player -> Rose ConnectFour -> Int
countWinningPositions player = foldRose countWins 0
  where
    countWins childrenSum (ConnectFour _ _ (Won p))
      | p == player = childrenSum + 1
      | otherwise = childrenSum
    countWins childrenSum _ = childrenSum

-- Helper function to run a GameStateOp
runGameStateOp :: GameStateOp a -> ConnectFour -> (a, ConnectFour)
runGameStateOp = gameStateOp

-- Function to execute a move and return the new game state
executeMove :: Int -> ConnectFour -> ConnectFour
executeMove col game = snd $ runGameStateOp (applyMove col) game