import           Data.Char

-- 1
even' :: Int -> Bool
even' x = mod x 2 == 0

-- 2
odd' :: Int -> Bool
odd' x = not (even' x)

-- 3
bmi :: Float -> Float -> Float
bmi height mass = mass / ((height / 100) ^ 2)

-- 4
deg2Rad :: Float -> Float
deg2Rad deg = pi * deg / 180

-- 5
rad2Deg :: Float -> Float
rad2Deg rad = 180 * rad / pi

-- 6
isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c  = (a + b) > c && (a + c) > b && (b + c) > a

-- 7
perimeter :: [Float] -> Float
perimeter (a) = sum a

-- 8
area :: [Float] -> Float
area (a) = sqrt(halfPerimeter * areaCore halfPerimeter a)
    where
    halfPerimeter :: Float
    halfPerimeter = perimeter a / 2
    areaCore :: Float -> [Float] -> Float
    areaCore p [a] = p - a
    areaCore p (a:as) = (p - a) * areaCore p as

-- 9
calculate :: Char -> Float -> Float -> Float
calculate op a b
    | op == '+' = a + b
    | op == '-' = a - b
    | op == '*' = a * b
    | otherwise = error "undefined"

-- 10
convert :: String -> String -> Float -> Float
convert from to amount = amount * convertCore from / convertCore to
    where
    convertCore :: String -> Float
    convertCore "bgn" = 1
    convertCore "eur" = 1.959
    convertCore "usd" = 1.76
    convertCore _ = error "unknown currency"

-- 13
head' :: [Float] -> Float
head' (x:_) = x

-- 14
tail' :: [Float] -> [Float]
tail' (_:xs) = xs

-- 16
last' :: [Float] -> Float
last' [] = error "list must not be empty"
last' [x] = x
last' (_:xs) = last' xs

-- 17
double :: [Float] -> [Float]
double (x) = mult 2 x

-- 18
mult :: Float -> [Float] -> [Float]
mult n [] = [] :: [Float]
mult n (x:xs) = x * n : mult n xs

-- 19
nth :: Float -> [Float] -> Float
nth n [] = error "error!"
nth 0 (x:_) = x
nth n (x:xs) | n < 0 = error "Invalid index"
             | otherwise = nth (n-1) xs

-- 20
member :: Float -> [Float] -> Bool
member n [] = False
member n (x:xs) = n == x || member n xs

-- 21
isFibonacciSequence :: [Float] -> Bool
isFibonacciSequence (first:second:third:remaining) = first + second == third && isFibonacciSequence (second : third : remaining)
isFibonacciSequence (_) = True

-- 22
sum' :: [Float] -> Float
sum' (x:xs) = x + sum' xs
sum' [] = 0

-- 23
product' :: [Float] -> Float
product' [x] = x
product' (x:xs) = x * product' xs
product' [] = 0

-- 24
multiply :: [Float] -> [Float] -> [Float]
multiply (x:xs) (y:ys) = x * y : multiply xs ys
multiply _ _ = []

-- 25
number2string :: Int -> String
number2string x = show x

-- 26
string2number :: String -> Int
string2number (x) = read x

-- 27
isValidId :: String -> Bool
isValidId (id) = mod (mod (weightedSum 0 (convertToIntArr id)) 11) 10 == digitToInt (last id)
                 where
                 convertToIntArr :: String -> [Int]
                 convertToIntArr (c:cs) = digitToInt c : convertToIntArr cs
                 convertToIntArr _ = []

                 weightedSum :: Int -> [Int] -> Int
                 weightedSum index (d:ds) = d * weight index + weightedSum (index + 1) ds
                 weightedSum _ _ = 0

                 weight :: Int -> Int
                 weight 0 = 2
                 weight 1 = 4
                 weight 2 = 8
                 weight 3 = 5
                 weight 4 = 10
                 weight 5 = 9
                 weight 6 = 7
                 weight 7 = 3
                 weight 8 = 6
                 weight _ = 0

-- 28
whatZodiacSignIs :: String -> String
whatZodiacSignIs (_:_:m0:m1:d0:d1:_) = checkSignCore (convertToDate m0 m1) (convertToDate d0 d1)
       where
       convertToDate :: Char -> Char -> Int
       convertToDate d0 d1 = string2number (d0 : [d1])
       checkSignCore :: Int -> Int -> String
       checkSignCore month day | (month == 1 && day >= 21) || (month == 2 && day < 19) = "Aquarius"
                               | (month == 2 && day >= 19) || (month == 3 && day < 21) = "Pisces"
                               | (month == 3 && day >= 21) || (month == 4 && day < 21) = "Aries"
                               | (month == 4 && day >= 21) || (month == 5 && day < 21) = "Taurus"
                               | (month == 5 && day >= 21) || (month == 6 && day < 22) = "Gemini"
                               | (month == 6 && day >= 22) || (month == 7 && day < 23) = "Cancer"
                               | (month == 7 && day >= 23) || (month == 8 && day < 24) = "Leo"
                               | (month == 8 && day >= 24) || (month == 9 && day < 24) = "Virgo"
                               | (month == 9 && day >= 24) || (month == 10 && day < 24) = "Libra"
                               | (month == 10 && day >= 24) || (month == 11 && day < 23) = "Scorpio"
                               | (month == 11 && day >= 23) || (month == 12 && day < 22) = "Sagittarius"
                               | (month == 12 && day >= 22) || (month == 1 && day < 21) = "Capricorn"
                               | otherwise = error "Invalid month or day"
