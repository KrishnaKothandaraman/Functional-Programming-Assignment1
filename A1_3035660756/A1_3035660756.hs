import Debug.Trace

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

ismember :: String -> [String] -> Bool
ismember _ [] = False
ismember a (x:xs) = a == x || ismember a xs

numberOfOccurences :: Int -> [Int] -> Int
numberOfOccurences _ [] = 0
numberOfOccurences n x = length [a | a <- x, a == n]

--sort(x) == x implies that it is always increasing then recursively call on same list but without all the occurences of the first element
isStepped :: [Int] -> Bool
isStepped [] = True
isStepped x = numberOfOccurences (x !! 0) x >= 3 && sort(x) == x && isStepped (drop (numberOfOccurences (x !! 0) x) x)
 
sort :: [Int] -> [Int]
sort [] = []
sort(x:xs) = sort ys ++ [x] ++ sort zs
    where
        ys = [a | a <- xs, a <= x]
        zs = [b | b <- xs, b > x]

--returns middle elements of list excluding first and last element
getmiddle :: [Int] -> [Int]
getmiddle [] = []
getmiddle (x:xs) | length xs >= 1 = tail (reverse xs)
                 | otherwise = [x]

-- x is sorted so last is greatest and first is smallest
wave' :: [Int] -> [Int]
wave' x | length x == 1 = x
        | otherwise = head (reverse x) : head x : wave (getmiddle x)

--wave' is a helper function that works on sorted x rather than unsorted x to reduce the number of sorts needed.
wave :: [Int] -> [Int]
wave [] = []
wave x = wave' (sort x)

--calculates nth Catalan Number using formula: 2n!/(n!)(n+1)!
catalan :: Integer -> Integer 
catalan n = (foldr (*) 1 [1..(2*n)]) `div` ((foldr (*) 1 [1..n]) * (foldr (*) 1 [1..n+1]))

--number of structurally unique Binary Search Trees with exactly n nodes is the nth Catalan Number 2n!/(n!)(n+1)!
--source: https://www.quora.com/Given-n-how-many-structurally-unique-BSTs-binary-search-trees-that-store-values-1-to-n-are-there-How-would-I-come-up-with-the-solution-Can-you-explain-the-thought-process-that-leads-to-the-solution
numBST :: Int -> Int
numBST n = fromInteger ((catalan (toInteger n)) `mod` (10^9+7))

--helper function that checks possible conditions and returns the point that satisfies the condition for k
--intuition is that, when we sort the squares, a point on the border of the kth largest square is a part of exactly the k squares(including itself) that are bigger than it
findAnswer :: [Int] -> Int -> [Int]
findAnswer n k  | length (dropWhile (<0) n) >= k = [head (drop (k-1) (reverse n)),head (drop (k-1) (reverse n))]
                | length (takeWhile (<0) n) >= k = [head (drop (k-1) n),head (drop (k-1) n)]
                | otherwise = [-1]

-- intuition is to split the list into two lists of squares in first and third quadrant and search independently in both lists for a point that satisfies the condition.
-- This works because we know that squares in first and third quadrant have no area of intersection and therefore share no points
-- if k == 0, return a point outside all squares
pointIn :: [Int] -> Int -> [Int]
pointIn n k | k > length n = [-1]
            | k == 0 = [(head(reverse(sort n)))*2, (head (reverse(sort n)))*2]
            | otherwise = findAnswer (sort n) k



--backtracking inspiration came from studying solution to N-Queen implementation using Haskell
--source: https://rosettacode.org/wiki/N-queens_problem#Haskell
chess :: Int -> Int
chess n = solveChess [[]] 0
  where
    solveChess :: [[Int]] -> Int -> Int
    solveChess boards counter
      | counter == n = length boards
      | otherwise = solveChess (concatMap createBoard boards) (counter+1)

    --returns list of legal position of knights where board[i] refers to column of knight on ith row
    createBoard :: [Int] -> [[Int]]
    createBoard board = [board ++ [x] | x <- [1..n], safePosition x board 1 (length board + 1)]

    --checks if there is no colision by placing a new knight in row 'r' at column 'col'
    safePosition :: Int -> [Int] -> Int -> Int -> Bool    
    safePosition x [] _ _ = True
    safePosition x (col:cols) n r = and [x /= col, abs(x - col) /= abs(r - n) , validLShape x col n r, safePosition x cols (n+1) r]

    --validates if knight at row r forms an L shaped collision with knight at row
    -- x: candidate column, r = candidate row, col = current col of existing knight, n = current row of existing knight
    validLShape:: Int -> Int -> Int -> Int -> Bool
    validLShape _ _ _ 1 = True
    validLShape x col n r | abs(x-col) > 2 = True 
                          | abs(r - n) > 2 = True
                          | otherwise = not (((abs(r - n) == 2) && (abs(x-col) == 1)) || ((abs(r - n) == 1) && (abs(x-col) == 2)))
