import RoseTree
import FileReader (getBoard, readMixedFile, MixedFileContent)
import Connect4 (board, ConnectFour (ConnectFour))

exampleTree :: Rose Int
exampleTree = generateRose (\x -> [x+1, x+2, x+3]) 1 2


main :: IO ()
main = do
    let filePath = "mixedformat.txt"
    result <- readMixedFile filePath
    putStrLn $ "Original tree: " ++ show result

    -- putStrLn $ "Original tree: " ++ show exampleTree
    -- putStrLn $ "Size of the tree: " ++ show (size exampleTree)
    -- putStrLn $ "Height of the tree: " ++ show (height exampleTree)
    -- putStrLn $ "Number of leaves in the tree: " ++ show (leavesCount exampleTree)
    -- putStrLn $ "Leaves: " ++ show (leaves exampleTree)
    -- putStrLn $ "At depth: " ++ show (elemsOnDepth exampleTree 1)
    -- putStrLn $ "At depth: " ++ show (sumRose exampleTree)