module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where


-- №1
unique :: Eq a => [a] -> Bool
unique [] = True
unique l = if (head l) `elem` (tail l)
            then False
            else unique (tail l)

-- №2
pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], a^2 + b^2 == c^2]

-- №3
primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], a^2 + b^2 == c^2, gcd a b == 1, gcd b c == 1, gcd a c == 1]

-- №4
sumDivisors :: Integral a => a -> a
sumDivisors n = 1 + sum [ x + y | x <- [2..(round (sqrt (fromIntegral n)))], n `mod` x == 0,
                                    let z = n `div` x
                                        y = if z /= x
                                            then z
                                            else 0]
 
perfectNumbers :: Integral a => [a]
perfectNumbers = [a | a <- [2..], a == sumDivisors a]

-- №5
cantorPairs :: Integral a => [(a, a)]
cantorPairs = [(n - a, a) | n <- [0..], a <- [0..n]]

-- №6
minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
