fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

ismember :: [Char] -> [[Char]] -> Bool
ismember _ [] = False
ismember a (x:xs) = a == x || ismember a xs

-- isStepped :: [Int] -> Bool
-- isStepped [] = True
-- isStepped x | length x >= 3 && x !! 0 == x !! 1 == x !! 2 = True && isStepped (drop 3 x)
--             | otherwise = False

sort :: [Int] -> [Int]
sort [] = []
sort(x:xs) = sort ys ++ [x] ++ sort zs
    where
        ys = [a | a <- xs, a <= x]
        zs = [b | b <- xs, b > x]

getmiddle :: [Int] -> [Int]
getmiddle [] = []
getmiddle (x:xs) | length xs >= 1 = tail (reverse xs)
                 | otherwise = [x]

wave' :: [Int] -> [Int]
wave' x | length x == 1 = x
        | otherwise = head (reverse x) : head x : wave (getmiddle x)


wave :: [Int] -> [Int]
wave [] = []
wave x = wave' (sort x)