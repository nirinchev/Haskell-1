{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- 1
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f xs = foldl (\ys y -> ys ++ [f y]) [] xs

-- 2
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f xs = foldl (\ys y -> ys ++ (if f y then [y] else [])) [] xs

-- 3
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x]

-- 4
repeat' :: a -> [a]
repeat' x = map (\_ -> x) [1..]

-- 5
cycle' :: [a] -> [a]
cycle' [] = []
cycle' (xs) = foldr (\_ ys -> xs ++ ys) [] [1..]

-- 6
every :: Int -> [a] -> [a]
every _ [] = []
every n (xs) = temp (drop (n-1) xs)
               where
               temp :: [a] -> [a]
               temp [] = []
               temp (x:xs) = x : every n xs

-- 7
localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:xs) | y > x && y > z = y : localMaxima(z:xs)
                       | otherwise = localMaxima (y:z:xs)
localMaxima _          = []

-- 8
mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap func [] = []
mapMap func (x:xs) = map func x : (mapMap func xs)

-- 9
filterFilter :: (a -> Bool) -> [[a]] -> [[a]]
filterFilter func [] = []
filterFilter func (x:xs) = filter func x : filterFilter func xs

-- 10
unit :: Int -> Int -> [[Int]]
unit _ 0 = []
unit e n = map (\i -> take (i - 1) zeros ++ [e] ++ take (n - i) zeros) [1..n]
           where
           zeros :: [Int]
           zeros = repeat 0

-- 11
row :: Int -> [[a]] -> [a]
row _ [] = []
row 0 (x:_) = x
row n (x:xs) | n < 0 = error "Invalid index"
             | otherwise = row (n-1) xs

-- 12
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose a = (map head a) : transpose (map tail a)

-- 13
sumMatrices :: [[Int]] -> [[Int]] -> [[Int]]
sumMatrices (x:xs) (y:ys) = zipWith (+) x y : sumMatrices xs ys
sumMatrices _      _      = []

-- 14
multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices xs ys = multMatricesCore xs (transpose ys)
                   where
                   multMatricesCore (x:xs) ys = map (calc x) ys : multMatricesCore xs ys
                   multMatricesCore [] _ = []
                   calc :: [Int] -> [Int] -> Int
                   calc x y = sum (zipWith (*) x y)
