module Connect4 where
import Data.List (transpose, findIndex)
import RoseTree

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

boardDimensions :: Board -> (Int, Int)
boardDimensions (Board []) = (0, 0)
boardDimensions (Board (row:rows)) = (length (row:rows), length row)

getCol :: Int -> [[Field]] -> [Field]
getCol n = map (head . drop n)

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
    | col < 0 || col > length (head board) - 1= Left "Dodavanje van granica table"
    | head (getCol col board) == Empty = Right True
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
        _ -> (False, game{ gameState = Invalid "Dodavanje nakon zavrsene igre" })

applyMovesFromList :: [Int] -> GameStateOp Bool
applyMovesFromList = foldr ((>>) . applyMove) (return False)

isEmpty :: Field -> Bool
isEmpty a = a == Empty

updateColumn :: [[Field]] -> Int -> Player -> [[Field]]
updateColumn board col player =
    case findIndex isEmpty (reverse (getCol col board)) of
        Just index -> 
            let rIndex = length (getCol col board) - 1 - index
            in take rIndex board ++
            [updateRow (board !! rIndex) col player] ++
            drop (rIndex + 1) board
        Nothing -> board
  where
    updateRow row colIndex newPlayer =
        take colIndex row ++ [Taken newPlayer] ++ drop (colIndex + 1) row

checkEnd :: Board -> GameState
checkEnd board@(Board fields)
    | checkWin board Z = Won Z
    | checkWin board C = Won C
    | all (notElem Empty) fields = Draw
    | otherwise = Open

checkWin :: Board -> Player -> Bool
checkWin board@(Board fields) player =
    let takenPlayer = Taken player
        (height, width) = boardDimensions board
    in  any (hasConsecutive 4 takenPlayer) fields ||                 -- Check rows
        any (hasConsecutive 4 takenPlayer) (transpose fields) ||     -- Check columns
        any (hasConsecutive 4 takenPlayer) (diagonals fields) ||     -- Check diagonals
        any (hasConsecutive 4 takenPlayer) (diagonals (map reverse fields))  -- Check anti-diagonals

hasConsecutive :: Int -> Field -> [Field] -> Bool
hasConsecutive n x xs
    | length xs < n = False
    | take n xs == replicate n x = True
    | otherwise = hasConsecutive n x (tail xs)

diagonals :: [[a]] -> [[a]]
diagonals board = diagonalsTopLeft board ++ diagonalsTopRight board
  where
    diagonalsTopLeft = concatMap diagonalsFromRowCol . zip [0..]
    diagonalsTopRight = diagonalsTopLeft . map reverse
    
    diagonalsFromRowCol (row, rowFields) = 
        [getDiagonal board (row, col) | (col, _) <- zip [0..] rowFields, row + col < length board]
    
    getDiagonal b (row, col) = 
        [b !! (row + i) !! (col + i) | i <- [0..min (length b - row - 1) (length (head b) - col - 1)]]

validMoves :: Board -> [Int]
validMoves board@(Board fields) = 
    [col | col <- [0..width-1], any (\row -> row !! col == Empty) fields]
  where
    (_, width) = boardDimensions board

generateMoveTree :: ConnectFour -> Int -> Rose ConnectFour
generateMoveTree initialGame maxDepth = generateRose generateMoves maxDepth initialGame
  where
    generateMoves :: ConnectFour -> [ConnectFour]
    generateMoves game@ConnectFour{gameState = Open, board = b} =
      [snd $ gameStateOp (applyMove move) game | move <- validMoves b]
    generateMoves _ = [] --prazni listovi za kraj

getGameState :: Rose ConnectFour -> GameState
getGameState (Node game _) = gameState game

countWinningPositions :: Player -> Rose ConnectFour -> Int
countWinningPositions player = foldRose countWins 0
  where
    countWins childrenSum (ConnectFour _ _ (Won p))
      | p == player = childrenSum + 1
      | otherwise = childrenSum
    countWins childrenSum _ = childrenSum

runGameStateOp :: GameStateOp a -> ConnectFour -> (a, ConnectFour)
runGameStateOp = gameStateOp

executeMove :: Int -> ConnectFour -> ConnectFour
executeMove col game = snd $ runGameStateOp (applyMove col) game