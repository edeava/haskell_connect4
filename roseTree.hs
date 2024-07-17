module RoseTree where
data Rose a = Node a [Rose a] deriving (Show)

instance Functor Rose where
    fmap f (Node x children) = Node (f x) (map (fmap f) children)

size :: Rose r -> Int
size (Node _ children) = 1 + sum (map size children)

height :: Rose r -> Int
height (Node _ []) = 1
height (Node _ children) = 1 + maximum (map height children)

leavesCount :: Rose r -> Int
leavesCount (Node _ children) =
    case children of
        [] -> 1
        _ -> sum (map leavesCount children)

leaves :: Rose r -> [r]
leaves (Node value children) =
    case children of
        [] -> value : concatMap leaves children
        _ -> concatMap leaves children

elemsOnDepth :: Rose r -> Int -> [r]
elemsOnDepth (Node value children) depth 
    | depth < 0 = []
    | depth == 0 = [value]
    | otherwise = concatMap (\child -> elemsOnDepth child (depth - 1)) children

foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f id (Node x children) = 
    f (foldl (foldRose f) id children) x

sumRose :: Rose Int -> Int
sumRose = foldRose (+) 0

generateRose :: (a -> [a]) -> Int -> a -> Rose a
generateRose f depth root
    | depth <= 0 = Node root []
    | otherwise  = Node root (map (generateRose f (depth - 1)) (f root))