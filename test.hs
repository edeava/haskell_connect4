-- import RoseTree

-- exampleTree :: Rose Int
-- exampleTree = generateRose (\x -> [x+1, x+2, x+3]) 1 2

-- prettyPrint :: Show a => Rose a -> String
-- prettyPrint = go 0
--   where
--     go indent (Node x children) =
--         replicate indent ' ' ++ show x ++ "\n" ++
--         concatMap (go (indent + 2)) children

-- generateDepthRose :: Int -> Int -> Rose Int
-- generateDepthRose depth root
--   | depth <= 0 = Node root []
--   | otherwise = Node (root + depth) (map (generateDepthRose (depth - 1)) [1, 1])

-- -- generateTrueRose :: (a -> Bool) -> Rose Int -> Rose Bool
-- -- generateTrueRose f (Node root children) 
-- --   | children == [] = Node (f root) []
-- --   | otherwise = Node (f root) (map (generateTrueRose f )

-- addIfEven :: Int -> Int
-- addIfEven a = if even a then 1 else 0

-- evenRose :: Rose Int -> Int
-- evenRose = foldRose (\acc x -> if (even x) then (acc + 1) else acc) 0

-- main :: IO ()
-- main = do
--     putStrLn $ "Original tree: " ++ show exampleTree
--     putStrLn $ "Size of the tree: " ++ show (size exampleTree)
--     putStrLn $ "Height of the tree: " ++ show (height exampleTree)
--     putStrLn $ "Number of leaves in the tree: " ++ show (leavesCount exampleTree)
--     putStrLn $ "Leaves: " ++ show (leaves exampleTree)
--     putStrLn $ "At depth: " ++ show (elemsOnDepth exampleTree 1)
--     putStrLn $ "Sum Rose: " ++ show (sumRose exampleTree)
--     putStrLn "Example tree with depth 1:"
--     putStrLn $ prettyPrint exampleTree

--     putStrLn "\nLarger tree with depth 3:"
--     let largerTree = generateRose (\x -> [x+1, x+1]) 3 1
--     putStrLn $ prettyPrint largerTree
--     let depthTree = generateDepthRose 3 3
--     putStrLn $ "\nDepth Tree\n:"
--     putStrLn $ prettyPrint depthTree
--     putStrLn $ "Even Rose: " ++ show (evenRose largerTree)


--     -- putStrLn $ "Tree after adding 'newChild' to 'child1': " ++ 
--     --     show (add "child1" "newChild" exampleTree)
--     -- putStrLn $ "Tree after removing 'child2': " ++ 
--     --     show (remove "child2" exampleTree)
--     -- putStrLn $ "Finding node 'grandchild2': " ++ 
--     --     show (find (== "grandchild2") exampleTree)