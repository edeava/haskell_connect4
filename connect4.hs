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

data Move = Valid Int | InvalidMsg String

instance Show Field where
    show Empty = " "
    show (Taken p) = show p

data GameStateInfo = Open | Won Player | Draw | Invalid String deriving (Show, Eq)

newtype Board = Board [[Field]]

data GameState = GameState{
    b :: Board,
    p :: Player
}

instance Show Board where
    show (Board board) = foldl (++) "\n\n" [showRow row ++ "\n" | row <- board] where
        showRow row = foldl (++) "|" [show field ++ "|" | field <- row]

instance Show GameState where
    show GameState{b, p} = show b

-- data GameState = GameState {
--     board :: Board,
--     info :: GameStateInfo,
--     toPlay :: Player
-- }

isMoveValid :: Board -> Int -> Bool
isMoveValid (Board b) c
  | head b !! c /= Empty = False --InvalidMsg "Dodavanje u punu kolonu"
--   | info /= Open = InvalidMsg "Dodavanje nakon kraja Igre"
  | otherwise = True --Valid c

getAllValidMoves :: Board -> [Int]
getAllValidMoves (Board b) = map fst (filter (\x->snd x == Empty) (zip [0..] (head b)))

flipBoard :: Board -> Board
flipBoard (Board b) = Board (foldl (flip (:)) [] b)

insertAt :: [a] -> Int -> a -> [a]
insertAt (a:as) 0 e = e:as
insertAt (a:as) i e = a : insertAt as (i - 1) e

dropPiece ::  Board -> Player -> Int -> Board
dropPiece (Board b) p c = Board (dropper b)
    where
        dropper [] = []
        dropper (row:rows) = if row !! c == Empty && (null rows || head rows !! c /= Empty)
                             then insertAt row c (Taken p) : rows
                             else row : dropper rows

diagonals :: [[a]] -> [[a]]
diagonals matrix = tail (diagonalsHelper [] matrix)
    where
        diagonalsHelper accumulator remainingRows =
            currentDiagonal : case remainingRows of
                []     -> transpose accumulator'
                (r:rs) -> diagonalsHelper (r : accumulator') rs
            where
                currentDiagonal = [head row | row <- accumulator, not (null row)]
                accumulator' = [tail row | row <- accumulator, not (null row)]

addMoves :: Int -> Field -> Int
addMoves acc f
    | f == Taken Z && acc <= 0 = acc - 1
    | f == Taken C && acc >= 0 = acc + 1
    | otherwise = 0

checkRows :: Board -> GameStateInfo
checkRows (Board (row:rows))
    | result <= -4 = Won Z
    | result >= 4 = Won C
    | null rows = Open
    | otherwise = checkRows (Board rows)
    where
        result = foldl addMoves 0 row

checkColumns :: Board -> GameStateInfo
checkColumns (Board b)
    | any (<= -4) result = Won Z
    | any (>= 4) result = Won C
    | otherwise = Open
    where
        result = foldl (zipWith addMoves) (replicate (length b) 0) b

checkDiagonals :: Board -> GameStateInfo
checkDiagonals (Board b)
    | any (<= -4) result = Won Z
    | any (>= 4) result = Won C
    | otherwise = Open
    where
        result = map (foldl addMoves 0) (filter (\x-> length x >= 4) (diagonals b))


checkGameState :: Board -> GameStateInfo
checkGameState (Board b)
    | Won Z `elem` [rows, columns, diags] = Won Z
    | Won C `elem` [rows, columns, diags] = Won C
    | draw b = Draw
    | otherwise = Open
    where
        rows = checkRows (Board b)
        columns = checkColumns (Board b)
        diags = checkDiagonals (Board b)
        draw [] = True
        draw (bs:bss) = notElem Empty bs && draw bss

basicGameStart :: GameState
basicGameStart = GameState{b = Board [
    [Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Taken C, Empty, Empty],
    [Empty, Empty, Taken C, Taken Z, Empty],
    [Empty, Empty, Taken C, Taken Z, Empty]
  ], p = Z}

movesForRose :: GameState -> [GameState]
movesForRose GameState{b, p} = [GameState{b = dropPiece b p move, p = changePlayer p} | move <- getAllValidMoves b]

metaGame = generateRose movesForRose 3 basicGameStart

bordo = Board [
    [Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Taken C, Empty, Empty],
    [Empty, Empty, Taken C, Taken Z, Empty],
    [Empty, Empty, Taken C, Taken Z, Empty]]


newtype GameStateOp g = GameStateOp { runGameState :: GameState -> (g, GameState) }

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

applyMove :: Int -> GameStateOp (Either String Field)
applyMove c = GameStateOp $ \s@GameState{b = b, p = p} ->
    if checkGameState b /= Open then (Left "Nevalidan potez, igra je vec zavrsena", s)
    else if isMoveValid b c
        then ( Right (getField c b)
             , GameState { b = dropPiece b p c
                         , p = changePlayer p
                         }
             )
        else(Left "Nevalidan potez, dodavanje u punu kolonu", s)
    where
        getField c (Board b) = head [f | row <- b, let f = row !! c, f /= Empty]

applyMovesFromList :: [Int] -> GameStateOp (Either String ())
applyMovesFromList = foldr (\move acc -> do
    result <- applyMove move
    case result of
        Left err -> return (Left err)
        Right _ -> acc) (return (Right ()))