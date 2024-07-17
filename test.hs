import RoseTree

exampleTree :: Rose Int
exampleTree = generateRose (\x -> [x+1, x+2, x+3]) 1 2

prettyPrint :: Show a => Rose a -> String
prettyPrint = go 0
  where
    go indent (Node x children) =
        replicate indent ' ' ++ show x ++ "\n" ++
        concatMap (go (indent + 2)) children

main :: IO ()
main = do
    putStrLn $ "Original tree: " ++ show exampleTree
    putStrLn $ "Size of the tree: " ++ show (size exampleTree)
    putStrLn $ "Height of the tree: " ++ show (height exampleTree)
    putStrLn $ "Number of leaves in the tree: " ++ show (leavesCount exampleTree)
    putStrLn $ "Leaves: " ++ show (leaves exampleTree)
    putStrLn $ "At depth: " ++ show (elemsOnDepth exampleTree 1)
    putStrLn $ "At depth: " ++ show (sumRose exampleTree)
    putStrLn "Example tree with depth 1:"
    putStrLn $ prettyPrint exampleTree

    putStrLn "\nLarger tree with depth 3:"
    let largerTree = generateRose (\x -> [x+1, x+2]) 3 1
    putStrLn $ prettyPrint largerTree
    
    -- putStrLn $ "Tree after adding 'newChild' to 'child1': " ++ 
    --     show (add "child1" "newChild" exampleTree)
    -- putStrLn $ "Tree after removing 'child2': " ++ 
    --     show (remove "child2" exampleTree)
    -- putStrLn $ "Finding node 'grandchild2': " ++ 
    --     show (find (== "grandchild2") exampleTree)