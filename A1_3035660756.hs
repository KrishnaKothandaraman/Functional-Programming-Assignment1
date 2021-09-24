fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

isMember :: a -> [b] -> Bool
isMember _ [] = False
isMember a (x:xs) = a == x || isMember a xs