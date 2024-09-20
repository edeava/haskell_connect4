myFoldr :: (a->b->b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

izbroj f = foldl (\acc x -> if (f x) then acc + 1 else acc) 0