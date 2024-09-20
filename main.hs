import RoseTree
import FileReader (getBoard, readMixedFile, MixedFileContent, getMoveList)
import Connect4

-- exampleTree :: Rose Int
-- exampleTree = generateRose (\x -> [x+1, x+2, x+3]) 1 2

-- -- createInitialGame :: Board -> ConnectFour
-- -- createInitialGame b = ConnectFour {
-- --     board = b,
-- --     nextToPlay = Z, --Zuti je uvek prvi
-- --     gameState = Open
-- -- }

-- main :: IO ()
-- main = do
--     let filePath = "testFile.txt"
--     putStrLn $ "Reading from file: " ++ filePath

--     -- Read from file
--     mixedContent <- readMixedFile filePath

--     -- Load game (board) from file
--     let loadedBoard = getBoard mixedContent
--     putStrLn "Loaded board:"
--     print loadedBoard

--     -- Load move list from file
--     let moveList = getMoveList mixedContent
--     putStrLn "Loaded move list:"
--     print moveList

--     -- Create initial game state
--     let initialGame = createInitialGame loadedBoard

--     -- Apply loaded moves and get final game state
--     let (_, finalGame) = gameStateOp (applyMovesFromList moveList) initialGame

--     -- Print the final game
--     putStrLn "Final game state:"
--     print finalGame

--     -- putStrLn $ "Original tree: " ++ show exampleTree
--     -- putStrLn $ "Size of the tree: " ++ show (size exampleTree)
--     -- putStrLn $ "Height of the tree: " ++ show (height exampleTree)
--     -- putStrLn $ "Number of leaves in the tree: " ++ show (leavesCount exampleTree)
--     -- putStrLn $ "Leaves: " ++ show (leaves exampleTree)
--     -- putStrLn $ "At depth: " ++ show (elemsOnDepth exampleTree 1)
--     -- putStrLn $ "At depth: " ++ show (sumRose exampleTree)

-- main :: IO ()
-- main = do
--     let player = Z
--     let initialBoard = Board [
--     [Empty, Empty, Empty, Empty, Empty],
--     [Empty, Empty, Taken C, Empty, Empty],
--     [Empty, Empty, Taken C, Taken Z, Empty],
--     [Empty, Empty, Taken C, Taken Z, Empty]]

--     let initialState = GameState initialBoard PlayerX -- Define these appropriately
--     let moves = [1, 2, 3, 1]
--     let (result, finalState) = runGameState (applyMovesFromList moves) initialState
--     case result of
--         Left err -> putStrLn $ "Error: " ++ err
--         Right _ -> putStrLn "All moves applied successfully"
--     putStrLn $ "Final state: " ++ show finalState