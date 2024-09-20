module RoseTree where
data Rose a = Node a [Rose a] deriving (Show)

size :: Rose a -> Int
size (Node a children) = 1 + sum (map size children)

height :: Rose a -> Int
height (Node a children) = 1 + maximum (map height children)

leavesCount :: Rose a -> Int
leavesCount (Node a children) = if null children
                                then 1
                                else sum (map leavesCount children)

leaves :: Rose a -> [a]
leaves (Node a children) = if null children
                           then [a]
                           else foldl (++) [] (map leaves children)

elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth atDepth (Node a children) = if atDepth == 0
                                         then [a]
                                         else foldl (++) [] (map (elemsOnDepth (atDepth-1)) children)

instance Functor Rose where
    fmap f (Node a children) = Node (f a) (map (fmap f) children)

foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Node a children) = f (foldl (foldRose f) acc children) a

generateRose :: (a -> [a]) -> Int -> a -> Rose a
generateRose f depth root = if depth == 0 
                            then Node root []
                            else Node root (map (generateRose f (depth - 1)) (f root))